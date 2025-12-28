/-
  Meta view operations (column metadata view)
-/
import Tv.Types
import Tv.State
import Tv.Backend
import Tv.Prql
import Tv.Source

namespace App.Meta

-- | Column indices in meta table (from queryMeta schema)
def colDist : Nat := 3   -- distinct count column
def colNull : Nat := 4   -- null% column

-- | Cache path for meta data (parquet alongside source file)
def cachePath (path : String) : String := path ++ ".tv.meta.parquet"

-- | Parse first char of Arrow format string
def fmtChar (fmt : String) : Char :=
  if h : fmt.length > 0 then fmt.toList[0] else '?'

-- | Map Arrow format char to type name
def fmtToType : Char → String
  | 'l' => "int64" | 'i' => "int32" | 's' => "int16" | 'c' => "int8"
  | 'L' => "uint64" | 'I' => "uint32" | 'S' => "uint16" | 'C' => "uint8"
  | 'g' => "float64" | 'f' => "float32" | 'd' => "decimal"
  | 'u' | 'U' => "str" | 'b' => "bool"
  | 'w' => "date" | 'D' => "timestamp" | 't' => "time"
  | _ => "?"

-- | Check if cache is valid (exists and newer than source)
def cacheValid (path : String) : IO Bool := do
  let cp := cachePath path
  try
    let srcMeta ← System.FilePath.metadata path
    let cacheMeta ← System.FilePath.metadata cp
    return decide (cacheMeta.modified.sec.toNat >= srcMeta.modified.sec.toNat)
  catch _ => return false

-- | Load meta from parquet cache
def loadCache (path : String) : IO (Option SomeTable) := do
  if !(← cacheValid path) then return none
  try
    let st ← Backend.execSql s!"SELECT * FROM read_parquet('{cachePath path}')"
    return some st
  catch _ => return none

-- | Save meta to parquet cache
def saveCache (path : String) (metaSql : String) : IO Unit := do
  let cp := cachePath path
  let _ ← Adbc.query s!"COPY ({metaSql}) TO '{cp}' (FORMAT PARQUET)"
  return ()

-- | Quote column name for SQL
def quoteCol (c : String) : String := "\"" ++ c.replace "\"" "\"\"" ++ "\""

-- | Build SQL for one column's stats
def colStatsSql (colName colType : String) : String :=
  let q := quoteCol colName
  s!"SELECT '{colName}' AS \"column\", '{colType}' AS type, COUNT({q}) AS cnt, " ++
  s!"COUNT(DISTINCT {q}) AS dist, " ++
  s!"CAST(CAST(ROUND((1.0 - CAST(COUNT({q}) AS DOUBLE) / NULLIF(COUNT(*), 0)) * 100) AS INTEGER) AS VARCHAR) || '%' AS \"null%\", " ++
  s!"CAST(MIN({q}) AS VARCHAR) AS min, CAST(MAX({q}) AS VARCHAR) AS max"

-- | Query column metadata (stats for all columns via SQL UNION)
def queryMeta (prql : String) (path : String) : IO (Except String SomeTable) := do
  -- Try cache for base queries on real files (no pipe = base query)
  let canCache := (prql.splitOn " | ").length == 1 && !Source.isSource path
  if canCache then
    if let some st ← loadCache path then
      Backend.logPrql s!"[meta] cached {cachePath path}"
      return .ok st
  -- Get schema first
  let schemaPrql := prql ++ " | take 1"
  match ← Backend.query (Backend.mkLimited schemaPrql 1) with
  | .error e => return .error e
  | .ok schema =>
    let colNames := schema.colNames
    if colNames.isEmpty then return .ok (← SomeTable.empty)
    -- Get types from Arrow format
    match ← Prql.compile schemaPrql with
    | .error e => return .error e
    | .ok sql =>
      try
        let qr ← Adbc.query sql
        let nc ← Adbc.ncols qr
        let mut types : Array String := #[]
        for c in [:nc.toNat] do
          let fmt ← Adbc.colFmt qr c.toUInt64
          types := types.push (fmtToType (fmtChar fmt))
        -- Build UNION ALL query for all columns
        let mut unions : Array String := #[]
        for i in [:colNames.size] do
          unions := unions.push (colStatsSql (colNames.getD i "") (types.getD i "?"))
        -- Compile base PRQL to get FROM clause
        let basePrql := prql ++ " | take 1"
        match ← Prql.compile basePrql with
        | .error e => return .error e
        | .ok baseSql =>
          -- Normalize whitespace for parsing
          let baseSql := baseSql.replace "\n" " " |>.replace "  " " "
          -- Extract FROM clause (after "FROM ", before WHERE/ORDER/LIMIT)
          let parts := baseSql.splitOn "FROM "
          let rest := parts.getD 1 ""
          let tbl := ((rest.splitOn " WHERE").head?.getD rest).splitOn " ORDER"
                     |>.head?.getD rest |>.splitOn " LIMIT" |>.head?.getD rest
          let fromClause := "FROM " ++ tbl
          -- Full meta query
          let metaSql := unions.map (· ++ " " ++ fromClause) |>.toList |> String.intercalate " UNION ALL "
          Backend.logPrql s!"[meta] {metaSql}"
          -- Save to cache for base queries
          if canCache then saveCache path metaSql
          let st ← Backend.execSql metaSql
          return .ok st
      catch e =>
        Backend.logError s!"queryMeta: {e}"
        return .error s!"{e}"

-- | Select rows where cell at column satisfies predicate
def selectRows (st : SomeTable) (col : Nat) (pred : Cell → Bool) : Array Nat :=
  (Array.range st.nRows).filter fun r => pred (st.getIdx r col)

-- | Select 100% null columns
def selNull (st : SomeTable) : Array Nat :=
  selectRows st colNull (·.str?.any (· == "100%"))

-- | Select single-value columns (distinct == 1)
def selSingle (st : SomeTable) : Array Nat :=
  selectRows st colDist (·.int?.any (· == 1))

-- | Get column names from selected rows (col 0 is "name")
def selNames (st : SomeTable) (selRows : Array Nat) : Array String :=
  selRows.filterMap fun r =>
    match st.getIdx r 0 with
    | .str s => some s
    | _ => none

-- | Pop meta view and set parent's keyCols + selCols
def popState (s : State) (selColNames : Array String) : State :=
  if h : s.parents.size > 0 then
    let parent := s.parents[0]
    let rest := s.parents.extract 1 s.parents.size
    let nav' := { parent.nav with keyCols := selColNames, colCur := ⟨0⟩, colOff := ⟨0⟩ }
    let parent' := { parent with nav := nav', selCols := selColNames }
    { s with curView := parent', parents := rest }
  else s

-- | Theorem: popState sets cursor = 0 and keyCols = sel
theorem popState_cursor (s : State) (sel : Array String) (h : s.parents.size > 0) :
    (popState s sel).curView.nav.colCur.val = 0 ∧
    (popState s sel).curView.nav.keyCols = sel := by
  simp [popState, h]

-- | Handle meta-specific keys. Returns none if not handled.
def runKey (v : View) (s : State) (key : PureKey) : Option State :=
  match key with
  | ._0  => v.cache.map fun st => s.setCur { v with selRows := selNull st }
  | ._1  => v.cache.map fun st => s.setCur { v with selRows := selSingle st }
  | .ret => if v.selRows.isEmpty then none
            else v.cache.map fun st => popState s (selNames st v.selRows)
  | _    => none

-- | Simp lemmas for keys Meta doesn't handle (used by Key.lean theorems)
@[simp] theorem runKey_j : runKey v s .j = none := rfl
@[simp] theorem runKey_k : runKey v s .k = none := rfl
@[simp] theorem runKey_l : runKey v s .l = none := rfl
@[simp] theorem runKey_h : runKey v s .h = none := rfl
@[simp] theorem runKey_g : runKey v s .g = none := rfl
@[simp] theorem runKey_G : runKey v s .G = none := rfl

end App.Meta
