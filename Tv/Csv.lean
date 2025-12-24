/-
  Simple CSV parser
-/
import Tv.Types

namespace Csv

-- | Split string by delimiter
def splitBy (delim : Char) (s : String) : List String :=
  s.splitOn (delim.toString) |>.map String.trim

-- | Parse a single cell value
def parseCell (s : String) : Cell :=
  if s.isEmpty || s == "null" || s == "NULL" then .null
  else if s == "true" then .bool true
  else if s == "false" then .bool false
  else match s.toInt? with
    | some n => .int n
    | none => match s.toNat? with
      | some n => .int n
      | none => .str s

-- | Parse CSV content into Table
def parse (content : String) : Table :=
  let lines := content.splitOn "\n" |>.filter (·.trim.length > 0)
  match lines with
  | [] => Table.empty
  | hdr :: rest =>
    let colNames := splitBy ',' hdr
    let cols := colNames.map (⟨·⟩) |>.toArray
    let rows := rest.map fun line =>
      (splitBy ',' line).map parseCell |>.toArray
    ⟨cols, rows.toArray⟩

-- | Load CSV from file
def loadFile (path : String) : IO Table := do
  let content ← IO.FS.readFile path
  return parse content

end Csv
