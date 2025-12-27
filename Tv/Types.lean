/-
  Core types: Cell, Column, Table
  Table stores columns by name (HashMap) for direct name-based access
-/
import Std.Data.HashMap

-- | Index into display order (keyCols first, then rest)
structure DispIdx where val : Nat deriving Repr, Inhabited, BEq

-- | Access array by DispIdx
def Array.getDisp (arr : Array α) (idx : DispIdx) (default : α) : α :=
  arr.getD idx.val default

-- | Find index in array, returns DispIdx
def Array.findDispIdx? (arr : Array α) (p : α → Bool) : Option DispIdx :=
  arr.findIdx? p |>.map (⟨·⟩)

-- | Join array of strings with separator
def Array.join (arr : Array String) (sep : String) : String :=
  String.intercalate sep arr.toList

-- | Cell value (sum type)
inductive Cell where
  | null
  | int (v : Int)
  | float (v : Float)
  | str (v : String)
  | bool (v : Bool)
  deriving Repr, Inhabited

namespace Cell

-- | Format integer with comma separators
def fmtInt (n : Int) : String :=
  let s := s!"{n.natAbs}"
  let chars := s.toList.reverse
  let rec go (cs : List Char) (acc : List Char) (cnt : Nat) : List Char :=
    match cs with
    | [] => acc
    | c :: rest =>
      let acc' := if cnt > 0 && cnt % 3 = 0 then c :: ',' :: acc else c :: acc
      go rest acc' (cnt + 1)
  let digits := go chars [] 0
  if n < 0 then "-" ++ String.ofList digits else String.ofList digits

-- | Theorem: fmtInt preserves digit order (no reversal)
theorem fmtInt_123 : fmtInt 123 = "123" := by native_decide
theorem fmtInt_1234 : fmtInt 1234 = "1,234" := by native_decide
theorem fmtInt_2015 : fmtInt 2015 = "2,015" := by native_decide

-- | Check if cell is numeric (for right-alignment)
def isNum : Cell → Bool
  | .int _   => true
  | .float _ => true
  | _        => false

-- | Theorem: int is numeric
theorem int_isNum (n : Int) : (Cell.int n).isNum = true := rfl

-- | Theorem: float is numeric (must right-align)
theorem float_isNum (f : Float) : (Cell.float f).isNum = true := rfl

-- | Theorem: str is not numeric
theorem str_not_isNum (s : String) : (Cell.str s).isNum = false := rfl

-- | Format float with n decimal places
def fmtFloat (f : Float) (n : Nat) : String :=
  let s := s!"{f}"
  match s.splitOn "." with
  | [intPart, decPart] =>
    if n == 0 then intPart
    else intPart ++ "." ++ decPart.take n
  | _ => s

def toString : Cell → String
  | .null    => ""
  | .int n   => fmtInt n
  | .float f => s!"{f}"
  | .str s   => s
  | .bool b  => if b then "true" else "false"

-- | Format cell with decimal precision
def toStringD (c : Cell) (decimals : Nat) : String :=
  match c with
  | .float f => fmtFloat f decimals
  | _ => c.toString

-- | Cell equality
def eq : Cell → Cell → Bool
  | .null, .null => true
  | .int a, .int b => a == b
  | .float a, .float b => a == b
  | .str a, .str b => a == b
  | .bool a, .bool b => a == b
  | _, _ => false

instance : BEq Cell where beq := eq
instance : ToString Cell where toString := toString

-- | Compare floats
def cmpFloat (a b : Float) : Ordering :=
  if a < b then .lt else if a > b then .gt else .eq

-- | Compare cells (for sorting): null < bool < int/float < str
def compare : Cell → Cell → Ordering
  | .null, .null => .eq
  | .null, _ => .lt
  | _, .null => .gt
  | .bool a, .bool b => if a == b then .eq else if a then .gt else .lt
  | .bool _, _ => .lt
  | _, .bool _ => .gt
  | .int a, .int b => Ord.compare a b
  | .int a, .float b => cmpFloat (Float.ofInt a) b
  | .float a, .int b => cmpFloat a (Float.ofInt b)
  | .float a, .float b => cmpFloat a b
  | .int _, .str _ => .lt
  | .float _, .str _ => .lt
  | .str _, .int _ => .gt
  | .str _, .float _ => .gt
  | .str a, .str b => Ord.compare a b

instance : Ord Cell where compare := compare

end Cell

-- | Sized array (array with type-level length proof)
abbrev SizedArray (α : Type) (n : Nat) := { arr : Array α // arr.size = n }

-- | Column data with typed row count
structure ColData (nRows : Nat) where
  cells : SizedArray Cell nRows
  width : Nat
  deriving Repr

-- | Table with typed row count, columns in insertion order
structure Table (nRows : Nat) where
  cols : Array (String × ColData nRows)
  deriving Repr

-- | Existential wrapper for tables with unknown row count
structure SomeTable where
  nRows : Nat
  table : Table nRows

namespace Table
-- | INVARIANT: Table is READ-ONLY for display. No client-side sorting, filtering,
-- | or aggregation (freq). All data transformations go through PRQL/Backend.
-- | Table only provides: cell access for rendering, column metadata for navigation.

def nCols (t : Table n) : Nat := t.cols.size

-- | Get column names in order
def colNames (t : Table n) : Array String := t.cols.map (·.1)

-- | Find column by name (linear scan, OK for <100 cols)
def findCol (t : Table n) (name : String) : Option (ColData n) :=
  t.cols.find? (·.1 == name) |>.map (·.2)

-- | Get cell at (row, colName)
def get (t : Table n) (row : Nat) (name : String) : Cell :=
  t.findCol name |>.map (·.cells.val.getD row .null) |>.getD .null

-- | Get cell at (row, colIdx) - for backwards compat
def getIdx (t : Table n) (row col : Nat) : Cell :=
  t.cols[col]? |>.map (·.2.cells.val.getD row .null) |>.getD .null

-- | Compute width for column (max of name and data, capped at 50)
def calcWidth (name : String) (cells : Array Cell) : Nat :=
  let hdrW := name.length
  let dataW := cells.foldl (init := hdrW) fun acc c =>
    max acc c.toString.length
  min 50 dataW

-- | Create table from column names and row data (returns existential)
def create (colNames : Array String) (rows : Array (Array Cell)) : SomeTable :=
  let nRows := rows.size
  -- transpose row-major to column-major
  let cols := colNames.mapIdx fun i name =>
    let cells := rows.map fun row => row.getD i .null
    let width := calcWidth name cells
    -- proof that cells.size = nRows
    have h : cells.size = nRows := Array.size_map ..
    (name, ColData.mk ⟨cells, h⟩ width)
  ⟨nRows, ⟨cols⟩⟩

-- | Empty table
def empty : Table 0 := ⟨#[]⟩

-- | Get all column widths in order
def colWidths (t : Table n) : Array Nat :=
  t.cols.map (·.2.width)

end Table

-- | INVARIANT: Table is for rendering only. All data ops go through Backend.
-- | DisplayInfo exposes only metadata needed for navigation/PRQL building.
structure DisplayInfo where
  colNames  : Array String
  colWidths : Array Nat
  nRows     : Nat
  nCols     : Nat

-- | Extract display info from table (only way to get metadata)
def Table.info (t : Table n) : DisplayInfo :=
  ⟨t.colNames, t.colWidths, n, t.nCols⟩

-- | INVARIANT: handleKey receives DisplayInfo, not Table.
-- | This makes it impossible to access cell data outside rendering.
-- | Enforcement: handleKey signature takes DisplayInfo, not Table.
