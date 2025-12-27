/-
  Backend: PRQL compilation + chunked query execution
  Views store PRQL strings, compile to SQL on demand
-/
import Tv.Adbc
import Tv.Types
import Tv.Prql

namespace Backend

-- | Cache path for meta data (alongside source file)
def metaCachePath (path : String) : String := path ++ ".tv.meta"

-- | Serialize cell to string (type:value format)
def serializeCell : Cell → String
  | .null => "n:"
  | .int v => s!"i:{v}"
  | .float v => s!"f:{v}"
  | .str v => s!"s:{v.replace "\t" "\\t" |>.replace "\n" "\\n"}"
  | .bool v => s!"b:{v}"

-- | Parse cell from string
def parseCell (s : String) : Cell :=
  if s.startsWith "i:" then .int (s.drop 2 |>.toInt? |>.getD 0)
  else if s.startsWith "f:" then .float (s.drop 2 |>.toNat? |>.getD 0 |> Float.ofNat)
  else if s.startsWith "s:" then .str (s.drop 2 |>.replace "\\t" "\t" |>.replace "\\n" "\n")
  else if s.startsWith "b:" then .bool (s.drop 2 == "true")
  else .null

-- | Save meta table to cache file
def saveMetaCache (path : String) (st : SomeTable) : IO Unit := do
  let cachePath := metaCachePath path
  let tbl := st.table
  let colNames := tbl.colNames.join "\t"
  let rows := (Array.range st.nRows).map fun r =>
    (tbl.colNames.map fun c => serializeCell (tbl.get r c)).join "\t"
  let content := (#[colNames] ++ rows).join "\n"
  IO.FS.writeFile cachePath content

-- | Load meta table from cache file (returns none if missing/invalid)
def loadMetaCache (path : String) : IO (Option SomeTable) := do
  let cachePath := metaCachePath path
  try
    -- Check if cache is newer than source
    let srcMeta ← System.FilePath.metadata path
    let cacheMeta ← System.FilePath.metadata cachePath
    if cacheMeta.modified.sec < srcMeta.modified.sec then return none
    let content ← IO.FS.readFile cachePath
    let lines := content.splitOn "\n" |>.filter (!·.isEmpty)
    match lines with
    | [] => return none
    | hdr :: dataLines =>
      let colNames := hdr.splitOn "\t" |>.toArray
      let rows := dataLines.map (fun line =>
        line.splitOn "\t" |>.map parseCell |>.toArray) |>.toArray
      return some (Table.create colNames rows)
  catch _ => return none

-- | PRQL function definitions (prepended to all queries)
-- | Matches rust tv's cfg/funcs.prql (use std.count to avoid ambiguity with column named 'count')
def prqlFuncs : String := "
let freq  = func c tbl <relation> -> (from tbl | group {c} (aggregate {Cnt = std.count this}) | derive {Pct = Cnt * 100 / std.sum Cnt, Bar = s\"repeat('#', CAST({Pct} / 5 AS INTEGER))\"} | sort {-Cnt})
let cnt   = func tbl   <relation> -> (from tbl | aggregate {n = std.count this})
let uniq  = func c tbl <relation> -> (from tbl | group {c} (take 1) | select {c})
let stats = func c tbl <relation> -> (from tbl | aggregate {n = std.count this, min = std.min c, max = std.max c, avg = std.average c, std = std.stddev c})
let meta  = func c tbl <relation> -> (from tbl | aggregate {cnt = s\"COUNT({c})\", dist = std.count_distinct c, total = std.count this, min = std.min c, max = std.max c})
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

-- | Parse first char of Arrow format string
def fmtChar (fmt : String) : Char :=
  if h : fmt.length > 0 then fmt.toList[0] else '?'

-- | Convert QueryResult to SomeTable
def qrToTable (qr : Adbc.QueryResult) : IO SomeTable := do
  let nc ← Adbc.ncols qr
  let nr ← Adbc.nrows qr
  let mut cols : Array String := #[]
  for i in [:nc.toNat] do
    let name ← Adbc.colName qr i.toUInt64
    cols := cols.push name
  let mut rows : Array (Array Cell) := #[]
  for r in [:nr.toNat] do
    let mut row : Array Cell := #[]
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
            let v ← Adbc.cellFloat qr r.toUInt64 c.toUInt64
            pure (.float v)
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
def execSql (sql : String) : IO SomeTable := do
  let qr ← Adbc.query sql
  qrToTable qr

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
  -- Create source table if needed
  if isSource path then createSource path
  match ← compilePrql q.prql with
  | .error e => return .error e
  | .ok sql =>
    let tableExpr := fileExpr path
    let sql := replaceDf sql tableExpr
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
      match st.table.getIdx 0 0 with
      | .int cnt => return .ok cnt.toNat
      | _ => return .ok 0
    else
      return .ok 0

-- | Get cell values for a specific row (for freq Enter filter)
def queryRow (prql : String) (path : String) (row : Nat) (ncols : Nat) : IO (Except String (Array Cell)) := do
  -- skip row rows, take 1
  let rowPrql := prql ++ s!" | take {row + 1}"
  match ← query (mkLimited rowPrql (row + 1)) path with
  | .error e => return .error e
  | .ok st =>
    if st.nRows > row then
      return .ok (Array.range ncols |>.map fun c => st.table.getIdx row c)
    else
      return .ok #[]

-- | Query all distinct values for a column (for fzf picker)
-- | No limit - distinct values bounded by column cardinality
def queryDistinct (prql : String) (path : String) (col : String) : IO (Except String (Array String)) := do
  let distinctPrql := prql ++ " | select {" ++ col ++ "} | group {" ++ col ++ "} (take 1)"
  logPrql distinctPrql
  if isSource path then createSource path
  match ← compilePrql distinctPrql with
  | .error e => return .error e
  | .ok sql =>
    let sql := replaceDf sql (fileExpr path)
    try
      let st ← execSql sql
      return .ok ((Array.range st.nRows).map fun r => toString (st.table.getIdx r 0))
    catch e =>
      return .error s!"SQL error: {e}"

-- | Map Arrow format char to type name
def fmtToType : Char → String
  | 'l' => "int64" | 'i' => "int32" | 's' => "int16" | 'c' => "int8"
  | 'L' => "uint64" | 'I' => "uint32" | 'S' => "uint16" | 'C' => "uint8"
  | 'g' => "float64" | 'f' => "float32" | 'd' => "decimal"
  | 'u' | 'U' => "str" | 'b' => "bool"
  | 'w' => "date" | 'D' => "timestamp" | 't' => "time"
  | _ => "?"

-- | Query column metadata (stats for all columns), with cache
def queryMeta (prql : String) (path : String) : IO (Except String SomeTable) := do
  -- Check cache first (only for base prql "from df")
  let isBasePrql := prql == "from df"
  if isBasePrql then
    if let some cached ← loadMetaCache path then
      return .ok cached
  -- Cache miss - compute stats
  let schemaPrql := prql ++ " | take 1"
  match ← query (mkLimited schemaPrql 1) path with
  | .error e => return .error e
  | .ok schema =>
    let colNames := schema.table.colNames
    if colNames.isEmpty then return .ok ⟨0, Table.empty⟩
    -- Get types from schema query result
    let typesPrql := prql ++ " | take 0"  -- just schema
    if isSource path then createSource path
    match ← compilePrql typesPrql with
    | .error _ => pure ()
    | .ok sql =>
      let sql := replaceDf sql (fileExpr path)
      let _ ← Adbc.query sql  -- run to get schema
    -- Now query for types by getting format of each col
    let typeSchemaPrql := prql ++ " | take 1"
    let mut types : Array String := #[]
    match ← compilePrql typeSchemaPrql with
    | .error _ => types := Array.replicate colNames.size "?"
    | .ok sql =>
      let sql := replaceDf sql (fileExpr path)
      try
        let qr ← Adbc.query sql
        let nc ← Adbc.ncols qr
        for c in [:nc.toNat] do
          let fmt ← Adbc.colFmt qr c.toUInt64
          types := types.push (fmtToType (fmtChar fmt))
      catch _ => types := Array.replicate colNames.size "?"
    -- Query stats for each column
    let mut rows : Array (Array Cell) := #[]
    for i in [:colNames.size] do
      let colName := colNames.getD i ""
      let colType := types.getD i "?"
      let metaPrql := (Prql.Query.parse prql).colMeta colName |>.render
      match ← query (mkLimited metaPrql 1) path with
      | .error _ => rows := rows.push #[.str colName, .str colType, .null, .null, .str "?", .null, .null]
      | .ok st =>
        if st.nRows > 0 then
          let cnt := st.table.getIdx 0 0
          let dist := st.table.getIdx 0 1
          let total := st.table.getIdx 0 2
          let minV := st.table.getIdx 0 3
          let maxV := st.table.getIdx 0 4
          let nullPct := match cnt, total with
            | .int c, .int t => if t > 0 then s!"{(t - c) * 100 / t}%" else "0%"
            | _, _ => "?"
          rows := rows.push #[.str colName, .str colType, cnt, dist, .str nullPct, minV, maxV]
        else
          rows := rows.push #[.str colName, .str colType, .null, .null, .str "?", .null, .null]
    let metaCols := #["column", "type", "cnt", "dist", "null%", "min", "max"]
    let result := Table.create metaCols rows
    -- Save to cache for base prql
    if isBasePrql then
      try saveMetaCache path result catch _ => pure ()
    return .ok result

end Backend
