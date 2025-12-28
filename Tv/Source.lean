/-
  Source: system command sources (ls, lr, ps, env, df)
  Handles source:* paths and table creation
-/
import Tv.Adbc
import Tv.Types

namespace Source

-- | Source path prefixes
def pfx : String := "source:"    -- base prefix
def ls  : String := "source:ls:" -- ls command
def lr  : String := "source:lr:" -- lr command

-- | Column indices in source table (from ls/lr output)
def colCount : Nat := 7
def colPerms : Nat := 0
def colPath  : Nat := 6

-- | Check if path is a system source
def isSource (path : String) : Bool := path.startsWith pfx

-- | Column definitions for sources without headers
def lsCols : String := "permissions,links,owner,grp,size,datetime,name"
def envCols : String := "name,value"

-- | Awk command to print first n fields tab-separated
def awkN (n : Nat) : String :=
  " | awk '{for(i=1;i<=" ++ toString n ++ ";i++) printf \"%s\\t\",$i; print \"\"}'"

-- | Build shell command that outputs tab-separated data
-- Returns (cmd, hasHeader). Quotes escaped for PRQL s-string (\").
def shellCmd (src : String) : String × Bool :=
  match src with
  | "ps"  => ("ps aux" ++ awkN 11, true)
  | "df"  => ("df -h" ++ awkN 6, true)
  | "env" => (r#"env | awk -F= '{print $1\"\t\"substr($0,index($0,\"=\")+1)}'"#, false)
  | s => if s.startsWith "ls:" then
           (s!"find {s.drop 3} -maxdepth 1 -printf " ++ r#"\"%M\t%n\t%u\t%g\t%s\t%TY-%Tm-%Td_%TH:%TM\t%f\n\""# ++ " 2>/dev/null", false)
         else if s.startsWith "lr:" then
           (s!"find {s.drop 3} -type f -printf " ++ r#"\"%M\t%n\t%u\t%g\t%s\t%TY-%Tm-%Td_%TH:%TM\t%p\n\""# ++ " 2>/dev/null", false)
         else ("echo unknown", false)

-- | Build column spec for read_csv (only for headerless sources)
-- Uses double braces for PRQL s-string escaping
def colSpec (src : String) : Option String :=
  let cols := match src with
    | "ps" | "df" => none  -- use header from command
    | "env" => some envCols
    | s => if s.startsWith "ls:" || s.startsWith "lr:" then some lsCols else some "line"
  cols.map fun c => "{{" ++ (c.splitOn "," |>.map (s!"'{·}':'VARCHAR'") |> String.intercalate ",") ++ "}}"

-- | Build shellfs read_csv expression for a source
def sourceExpr (path : String) : String :=
  let src := path.drop pfx.length
  let (cmd, hasHeader) := shellCmd src
  let colPart := colSpec src |>.map (s!", columns={·}") |>.getD ""
  s!"read_csv('{cmd} |', delim='\\t', header={hasHeader}, null_padding=true{colPart})"

-- | Generate PRQL from clause for path
-- Files use backticks, sources use s-string with SELECT
def fromExpr (path : String) : String :=
  if isSource path then s!"from s\"SELECT * FROM {sourceExpr path}\""
  else s!"from `{path}`"

end Source
