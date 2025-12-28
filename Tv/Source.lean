/-
  Source: system command sources (ls, lr, ps, env, df)
  Handles source:* paths and table creation via DuckDB temp tables
-/
import Tv.Adbc
import Tv.Types
import Tv.State

namespace Source

-- | Source path prefixes
def pfx : String := "source:"    -- base prefix
def ls  : String := "source:ls:" -- ls command
def lr  : String := "source:lr:" -- lr command

-- | Counter for unique temp table names
initialize srcCounter : IO.Ref Nat ← IO.mkRef 0

-- | Generate unique temp table name
def nextTmpName : IO String := do
  let n ← srcCounter.modifyGet fun n => (n, n + 1)
  pure s!"_tv_src_{n}"

-- | Column indices in source table (from ls/lr output)
def colCount : Nat := 7
def colPerms : Nat := 0
def colPath  : Nat := 6

-- | Check if path is a system source
def isSource (path : String) : Bool := path.startsWith pfx

-- | Column definitions for sources without headers
def lsCols : String := "permissions,links,owner,grp,size,datetime,path"
def envCols : String := "name,value"

-- | Awk command to print first n fields tab-separated
def awkN (n : Nat) : String :=
  " | awk '{for(i=1;i<=" ++ toString n ++ ";i++) printf \"%s\\t\",$i; print \"\"}'"

-- | find printf format for ls/lr (perms, links, owner, group, size, datetime, path)
def findFmt : String := "\"%M\\t%n\\t%u\\t%g\\t%s\\t%TY-%Tm-%Td_%TH:%TM\\t%p\\n\""

-- | Build shell command that outputs tab-separated data
-- Returns (cmd, hasHeader). ls/lr without path default to "."
def shellCmd (src : String) : String × Bool :=
  match src with
  | "ps"  => ("ps aux" ++ awkN 11, true)
  | "df"  => ("df -h" ++ awkN 6, true)
  | "env" => ("env | awk -F= '{print $1\"\\t\"substr($0,index($0,\"=\")+1)}'", false)
  | "ls"  => (s!"find . -maxdepth 1 -printf {findFmt} 2>/dev/null", false)
  | "lr"  => (s!"find . -printf {findFmt} 2>/dev/null", false)
  | s => if s.startsWith "ls:" then
           (s!"find {s.drop 3} -maxdepth 1 -printf {findFmt} 2>/dev/null", false)
         else if s.startsWith "lr:" then
           (s!"find {s.drop 3} -printf {findFmt} 2>/dev/null", false)
         else ("echo unknown", false)

-- | Check if source is ls/lr type (needs lsCols)
def isLsLr (s : String) : Bool :=
  s == "ls" || s == "lr" || s.startsWith "ls:" || s.startsWith "lr:"

-- | Build column spec for read_csv (only for headerless sources)
def colSpec (src : String) : Option String :=
  let cols := match src with
    | "ps" | "df" => none  -- use header from command
    | "env" => some envCols
    | s => if isLsLr s then some lsCols else some "line"
  cols.map fun c => "{" ++ (c.splitOn "," |>.map (s!"'{·}':'VARCHAR'") |> String.intercalate ",") ++ "}"

-- | Escape single quotes for SQL (double them)
def escSql (s : String) : String := s.replace "'" "''"

-- | Build shellfs read_csv expression for a source (for SQL)
def sourceExpr (path : String) : String :=
  let src := path.drop pfx.length
  let (cmd, hasHeader) := shellCmd src
  let colPart := colSpec src |>.map (s!", columns={·}") |>.getD ""
  s!"read_csv('{escSql cmd} |', delim='\\t', header={hasHeader}, null_padding=true{colPart})"

-- | Type conversion SQL for ls/lr sources (size→int, datetime→timestamp)
def lsCast : String :=
  "permissions, CAST(links AS BIGINT) AS links, owner, grp, " ++
  "CAST(size AS BIGINT) AS size, " ++
  "strptime(datetime, '%Y-%m-%d_%H:%M') AS datetime, path"

-- | Create temp table from source, return table name
-- Executes shell command once, stores result in DuckDB temp table
def createTmpTable (path : String) : IO String := do
  let tbl ← nextTmpName
  let expr := sourceExpr path
  let src := path.drop pfx.length
  let cols := if isLsLr src then lsCast else "*"
  let sql := s!"CREATE OR REPLACE TEMP TABLE {tbl} AS SELECT {cols} FROM {expr}"
  let _ ← Adbc.query sql
  pure tbl

-- | Generate PRQL from clause for path or temp table name
-- Files use backticks, temp tables use bare name
def fromExpr (path : String) : String :=
  if path.startsWith "_tv_src_" then s!"from {path}"  -- temp table
  else s!"from `{path}`"                               -- file path

-- | Create source temp table and return (path for display, temp table name for PRQL)
def initSource (cmd : String) : IO (String × String) := do
  let path := s!"{pfx}{cmd}"
  let tbl ← createTmpTable path
  pure (path, tbl)

-- | Push source view with temp table (IO: creates temp table)
def pushView (cmd : String) (disp : String) (vk : App.ViewKind) (s : App.State) : IO App.State := do
  let (path, tbl) ← initSource cmd
  pure (s.push ⟨path, { base := fromExpr tbl }, disp, {}, vk, none, #[], #[], none, App.defDecimals⟩)

-- | r key - recursive list directory
def r (s : App.State) : IO App.State := pushView "lr:." "lr ./" App.ViewKind.fld s

-- | R key - list directory (non-recursive)
def R (s : App.State) : IO App.State := pushView "ls:." "ls ./" App.ViewKind.fld s

end Source
