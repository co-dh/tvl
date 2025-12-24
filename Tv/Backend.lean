/-
  Backend: PRQL compilation + chunked query execution
  Views store PRQL strings, compile to SQL on demand
-/
import Tv.Adbc
import Tv.Types

namespace Backend

-- | PRQL function definitions (prepended to all queries)
def prqlFuncs : String := "
let freq = func c tbl <relation> -> (from tbl | group {c} (aggregate {Cnt = count this}) | derive {Pct = Cnt * 100 / sum Cnt, Bar = s\"repeat('#', CAST({Pct} / 5 AS INTEGER))\"} | sort {-Cnt})
let cnt = func tbl <relation> -> (from tbl | aggregate {n = count this})
"

-- | Theorems: freq PRQL includes required columns
theorem freq_has_pct : (prqlFuncs.splitOn "Pct").length > 1 := by native_decide
theorem freq_has_bar : (prqlFuncs.splitOn "Bar").length > 1 := by native_decide

-- | Compile PRQL to SQL using prqlc CLI (stdin → stdout)
def compilePrql (prql : String) : IO (Except String String) := do
  let full := prqlFuncs ++ "\n" ++ prql
  let child ← IO.Process.spawn {
    cmd := "prqlc"
    args := #["compile", "--hide-signature-comment"]
    stdin := .piped
    stdout := .piped
    stderr := .piped
  }
  child.stdin.putStr full
  child.stdin.flush
  let (_, child') ← child.takeStdin
  let stdout ← child'.stdout.readToEnd
  let stderr ← child'.stderr.readToEnd
  let code ← child'.wait
  if code == 0 then return .ok stdout
  else return .error s!"prqlc: {stderr}"

-- | Generate table expression for file path or source
def fileExpr (path : String) : String :=
  if path.endsWith ".parquet" then s!"read_parquet('{path}')"
  else if path.endsWith ".csv" || path.endsWith ".csv.gz" then s!"read_csv('{path}')"
  else if path.endsWith ".json" then s!"read_json('{path}')"
  else if path.startsWith "source:" then "tv_source"
  else s!"'{path}'"

-- | Check if path is a system source
def isSource (path : String) : Bool := path.startsWith "source:"

-- | Create source table in DuckDB
def createSource (path : String) : IO Unit := do
  let src := path.drop 7  -- remove "source:"
  let (cmd, args, cols) := match src with
    | "ls" => ("ls", #["-la"], "permissions,links,owner,grp,size,mon,day,time,name")
    | "ps" => ("ps", #["aux"], "user,pid,cpu,mem,vsz,rss,tty,stat,start,time,command")
    | "env" => ("env", #[], "name,value")
    | "df" => ("df", #["-h"], "filesystem,size,used,avail,pct,mount")
    | s => if s.startsWith "ls:" then ("ls", #["-la", s.drop 3], "permissions,links,owner,grp,size,mon,day,time,name")
           else ("echo", #["unknown source"], "line")
  let out ← IO.Process.output { cmd := cmd, args := args }
  let lines := out.stdout.splitOn "\n" |>.filter (!·.isEmpty) |>.drop 1  -- skip header
  if lines.isEmpty then return ()
  -- Build INSERT statements
  let colList := cols.splitOn ","
  let mut vals : List String := []
  for line in lines do
    let parts := line.splitOn " " |>.filter (!·.isEmpty)
    let escaped := parts.map (fun s => "'" ++ s.replace "'" "''" ++ "'")
    -- Pad or truncate to match column count
    let padded := escaped ++ List.replicate (colList.length - escaped.length) "''"
    vals := vals ++ [s!"({String.intercalate ", " (padded.take colList.length)})"]
  let createSql := s!"CREATE OR REPLACE TABLE tv_source ({cols.splitOn "," |>.map (· ++ " VARCHAR") |> String.intercalate ", "})"
  let _ ← Adbc.query createSql
  if vals.length > 0 then
    let insertSql := s!"INSERT INTO tv_source VALUES {String.intercalate ", " vals}"
    let _ ← Adbc.query insertSql
  return ()

-- | Replace "df" placeholder with actual table expression
def replaceDf (sql : String) (tableExpr : String) : String :=
  sql.replace "\"df\"" tableExpr
     |>.replace " df " s!" {tableExpr} "
     |>.replace " df\n" s!" {tableExpr}\n"
     |>.replace "FROM df" s!"FROM {tableExpr}"

-- | Parse first char of Arrow format string
def fmtChar (fmt : String) : Char :=
  if h : fmt.length > 0 then fmt.toList[0] else '?'

-- | Convert QueryResult to Table
def qrToTable (qr : Adbc.QueryResult) : IO Table := do
  let nc ← Adbc.ncols qr
  let nr ← Adbc.nrows qr
  let mut cols := #[]
  for i in [:nc.toNat] do
    let name ← Adbc.colName qr i.toUInt64
    cols := cols.push ⟨name⟩
  let mut rows := #[]
  for r in [:nr.toNat] do
    let mut row := #[]
    for c in [:nc.toNat] do
      let isNull ← Adbc.cellIsNull qr r.toUInt64 c.toUInt64
      if isNull then
        row := row.push .null
      else
        let fmt ← Adbc.colFmt qr c.toUInt64
        let cell ← match fmtChar fmt with
          | 'l' | 'i' | 's' | 'c' =>
            let v ← Adbc.cellInt qr r.toUInt64 c.toUInt64
            pure (.int v)
          | 'g' | 'f' | 'd' =>
            let s ← Adbc.cellStr qr r.toUInt64 c.toUInt64
            pure (.str s)
          | 'b' =>
            let s ← Adbc.cellStr qr r.toUInt64 c.toUInt64
            pure (.bool (s == "true"))
          | _ =>
            let s ← Adbc.cellStr qr r.toUInt64 c.toUInt64
            pure (.str s)
        row := row.push cell
    rows := rows.push row
  return Table.create cols rows

-- | Init backend
def init : IO Bool := Adbc.init

-- | Shutdown backend
def shutdown : IO Unit := Adbc.shutdown

-- | Execute raw SQL
def execSql (sql : String) : IO Table := do
  let qr ← Adbc.query sql
  qrToTable qr

-- | Execute PRQL query on path (compiles PRQL, replaces df, executes)
def query (prql : String) (path : String) : IO (Except String Table) := do
  -- Create source table if needed
  if isSource path then createSource path
  match ← compilePrql prql with
  | .error e => return .error e
  | .ok sql =>
    let tableExpr := fileExpr path
    let sql := replaceDf sql tableExpr
    try
      let tbl ← execSql sql
      return .ok tbl
    catch e =>
      return .error s!"SQL error: {e}"

-- | Execute PRQL with chunk (for viewport rendering)
def queryChunk (prql : String) (path : String) (offset limit : Nat) : IO (Except String Table) := do
  let chunkPrql := s!"{prql} | take {offset + limit}"
  -- Note: PRQL doesn't have offset, so we take more and skip in Lean
  -- Better: use SQL LIMIT/OFFSET directly after compilation
  query chunkPrql path

-- | Get total row count for PRQL query
def queryCount (prql : String) (path : String) : IO (Except String Nat) := do
  let countPrql := prql ++ " | aggregate {n = count this}"
  match ← query countPrql path with
  | .error e => return .error e
  | .ok tbl =>
    if tbl.nRows > 0 then
      match tbl.get 0 0 with
      | .int cnt => return .ok cnt.toNat
      | _ => return .ok 0
    else
      return .ok 0

end Backend
