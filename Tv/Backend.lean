/-
  Backend: PRQL compilation + chunked query execution
  Views store PRQL strings, compile to SQL on demand
-/
import Tv.Adbc
import Tv.Types
import Tv.Prql

namespace Backend

-- | Generate table expression for file path or source
def fileExpr (path : String) : String :=
  if path.endsWith ".parquet" then s!"read_parquet('{path}')"
  else if path.endsWith ".csv" || path.endsWith ".csv.gz" then s!"read_csv('{path}')"
  else if path.endsWith ".json" then s!"read_json('{path}')"
  else if path.startsWith srcPfx then "tv_source"
  else s!"'{path}'"

-- | Check if path is a system source
def isSource (path : String) : Bool := path.startsWith srcPfx

-- | Create source table in DuckDB
def createSource (path : String) : IO Unit := do
  let src := path.drop srcPfx.length
  let (cmd, args, cols) := match src with
    | "ls" => ("ls", #["-la", "--time-style=+%Y-%m-%d_%H:%M"], "permissions,links,owner,grp,size,datetime,name")
    | "ps" => ("ps", #["aux"], "user,pid,cpu,mem,vsz,rss,tty,stat,start,time,command")
    | "env" => ("env", #[], "name,value")
    | "df" => ("df", #["-h"], "filesystem,size,used,avail,pct,mount")
    | s => if s.startsWith "ls:" then ("ls", #["-la", "--time-style=+%Y-%m-%d_%H:%M", s.drop 3], "permissions,links,owner,grp,size,datetime,name")
           else if s.startsWith "lr:" then ("find", #[s.drop 3, "-type", "f", "-printf", "%M\t%n\t%u\t%g\t%s\t%TY-%Tm-%Td_%TH:%TM\t%p\n"], "permissions,links,owner,grp,size,datetime,path")
           else ("echo", #["unknown source"], "line")
  let out ← IO.Process.output { cmd := cmd, args := args }
  let hasHeader := cmd == "ls" || cmd == "ps" || cmd == "df"
  let lines := out.stdout.splitOn "\n" |>.filter (!·.isEmpty) |> (if hasHeader then (·.drop 1) else id)
  if lines.isEmpty then return ()
  -- Build INSERT statements
  let colArr := cols.splitOn "," |>.toArray
  let mut vals : Array String := #[]
  for line in lines do
    -- try tab first (find -printf), fall back to space (ls, ps, df)
    let parts := let ts := line.splitOn "\t"
                 if ts.length > 1 then ts.toArray else line.splitOn " " |>.filter (!·.isEmpty) |>.toArray
    let escaped := parts.map (fun s => "'" ++ s.replace "'" "''" ++ "'")
    -- Pad or truncate to match column count
    let padded := escaped ++ Array.replicate (colArr.size - escaped.size) "''"
    vals := vals.push s!"({(padded.extract 0 colArr.size).join ", "})"
  let createSql := s!"CREATE OR REPLACE TABLE tv_source ({(colArr.map (· ++ " VARCHAR")).join ", "})"
  let _ ← Adbc.query createSql
  if vals.size > 0 then
    let insertSql := s!"INSERT INTO tv_source VALUES {vals.join ", "}"
    let _ ← Adbc.query insertSql
  return ()

-- | Replace "df" placeholder with actual table expression
def replaceDf (sql : String) (tableExpr : String) : String :=
  sql.replace "\"df\"" tableExpr
     |>.replace " df " s!" {tableExpr} "
     |>.replace " df\n" s!" {tableExpr}\n"
     |>.replace "FROM df" s!"FROM {tableExpr}"

-- | Init backend
def init : IO Bool := Adbc.init

-- | Shutdown backend
def shutdown : IO Unit := Adbc.shutdown

-- | Execute raw SQL, return SomeTable (zero-copy)
def execSql (sql : String) : IO SomeTable := do
  let qr ← Adbc.query sql
  SomeTable.ofQueryResult qr

-- | Get timestamp as HH:MM:SS.mmm
def timestamp : IO String := do
  let ms ← IO.monoMsNow
  let s := ms / 1000 % 86400  -- seconds in day
  let h := s / 3600
  let m := (s % 3600) / 60
  let sec := s % 60
  let milli := ms % 1000
  let d2 := fun n : Nat => s!"{Char.ofNat (48 + n / 10)}{Char.ofNat (48 + n % 10)}"
  pure s!"{h}:{d2 m}:{d2 sec}.{milli}"

-- | Log to /tmp/tv.log
def logPrql (prql : String) : IO Unit := do
  let ts ← timestamp
  let h ← IO.FS.Handle.mk "/tmp/tv.log" .append
  h.putStrLn s!"[{ts}] [prql] {prql}"

-- | Log error to file (not stderr - silent is golden)
def logError (msg : String) : IO Unit := do
  let ts ← timestamp
  let h ← IO.FS.Handle.mk "/tmp/tv.log" .append
  h.putStrLn s!"[{ts}] [error] {msg}"

-- | Check if string contains "take "
def hasLimit (s : String) : Bool := (s.splitOn "take ").length > 1

-- | PRQL query with proof it has a limit
structure LimitedQuery where
  prql : String
  proof : hasLimit prql = true

-- | Execute PRQL query on path (requires proof of limit)
def query (q : LimitedQuery) (path : String) : IO (Except String SomeTable) := do
  logPrql q.prql
  if isSource path then createSource path
  match ← Prql.compile q.prql with
  | .error e => return .error e
  | .ok sql =>
    let sql := replaceDf sql (fileExpr path)
    try
      let st ← execSql sql
      return .ok st
    catch e =>
      return .error s!"SQL error: {e}"

-- | Create LimitedQuery by appending take
def mkLimited (prql : String) (n : Nat) : LimitedQuery :=
  let q := s!"{prql} | take {n}"
  if h : hasLimit q = true then ⟨q, h⟩
  else ⟨"select 1 | take 1", by native_decide⟩  -- fallback, never reached

-- | Theorem: example shows mkLimited has limit
theorem mkLimited_example : hasLimit "from df | take 1000" = true := by native_decide

-- | Get total row count for PRQL query
def queryCount (prql : String) (path : String) : IO (Except String Nat) := do
  let countPrql := prql ++ " | aggregate {n = std.count this}"
  match ← query (mkLimited countPrql 1) path with
  | .error e => return .error e
  | .ok st =>
    if st.nRows > 0 then
      match st.getIdx 0 0 with
      | .int cnt => return .ok cnt.toNat
      | _ => return .ok 0
    else
      return .ok 0

-- | Get cell values for a specific row (for freq Enter filter)
def queryRow (prql : String) (path : String) (row : Nat) (ncols : Nat) : IO (Except String (Array Cell)) := do
  let rowPrql := prql ++ s!" | take {row + 1}"
  match ← query (mkLimited rowPrql (row + 1)) path with
  | .error e => return .error e
  | .ok st =>
    if st.nRows > row then
      return .ok (Array.range ncols |>.map fun c => st.getIdx row c)
    else
      return .ok #[]

-- | Query all distinct values for a column (for fzf picker)
def queryDistinct (prql : String) (path : String) (col : String) : IO (Except String (Array String)) := do
  let distinctPrql := prql ++ " | select {" ++ col ++ "} | group {" ++ col ++ "} (take 1)"
  logPrql distinctPrql
  if isSource path then createSource path
  match ← Prql.compile distinctPrql with
  | .error e => return .error e
  | .ok sql =>
    let sql := replaceDf sql (fileExpr path)
    try
      let st ← execSql sql
      return .ok ((Array.range st.nRows).map fun r => toString (st.getIdx r 0))
    catch e =>
      return .error s!"SQL error: {e}"

end Backend
