/-
  Backend: PRQL compilation + chunked query execution
  Views store PRQL strings, compile to SQL on demand
-/
import Tv.Adbc
import Tv.Types

namespace Backend

-- | PRQL function definitions (prepended to all queries)
def prqlFuncs : String := "
let freq = func c tbl <relation> -> (from tbl | group {c} (aggregate {Cnt = count this}) | sort {-Cnt})
let cnt = func tbl <relation> -> (from tbl | aggregate {n = count this})
"

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

-- | Generate table expression for file path
def fileExpr (path : String) : String :=
  if path.endsWith ".parquet" then s!"read_parquet('{path}')"
  else if path.endsWith ".csv" || path.endsWith ".csv.gz" then s!"read_csv('{path}')"
  else if path.endsWith ".json" then s!"read_json('{path}')"
  else s!"'{path}'"

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
