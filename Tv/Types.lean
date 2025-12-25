/-
  Core types: Cell, Column, Table
-/

-- | Cell value (sum type)
inductive Cell where
  | null
  | int (v : Int)
  | float (v : Float)
  | str (v : String)
  | bool (v : Bool)
  deriving Repr, Inhabited

namespace Cell

-- | Format integer with comma separators (iterate right-to-left)
def fmtInt (n : Int) : String :=
  let s := s!"{n.natAbs}"
  let chars := s.toList.reverse  -- start from least significant
  let rec go (cs : List Char) (acc : List Char) (cnt : Nat) : List Char :=
    match cs with
    | [] => acc
    | c :: rest =>
      let acc' := if cnt > 0 && cnt % 3 = 0 then c :: ',' :: acc else c :: acc
      go rest acc' (cnt + 1)
  let digits := go chars []  0
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

-- | Column metadata
structure Column where
  name : String
  deriving Repr, Inhabited

-- | Table with sized dimensions and cached widths
structure Table where
  cols : Array Column
  rows : Array (Array Cell)
  widths : Array Nat  -- cached column widths
  deriving Repr

namespace Table

def nCols (t : Table) : Nat := t.cols.size
def nRows (t : Table) : Nat := t.rows.size

-- | Compute column widths (max of header and data, capped at 50)
def calcWidths (cols : Array Column) (rows : Array (Array Cell)) : Array Nat :=
  let hdrW := cols.map (·.name.length)
  let dataW := rows.foldl (init := hdrW) fun acc row =>
    let rowW := row.map (Cell.toString ·) |>.map String.length
    Array.zipWith (fun a b => max a b) acc rowW
  dataW.map (fun w => min 50 w)

-- | Create table with cached widths
def create (cols : Array Column) (rows : Array (Array Cell)) : Table :=
  ⟨cols, rows, calcWidths cols rows⟩

def empty : Table := ⟨#[], #[], #[]⟩

-- | Get cell at (row, col), default to null
def get (t : Table) (r c : Nat) : Cell :=
  t.rows.getD r #[] |>.getD c .null

-- | Delete column at index
def delCol (t : Table) (idx : Nat) : Table :=
  if idx ≥ t.nCols then t
  else
    let newCols := t.cols.eraseIdx! idx
    let newRows := t.rows.map (·.eraseIdx! idx)
    ⟨newCols, newRows, t.widths.eraseIdx! idx⟩

-- | Access cached widths
def colWidths (t : Table) : Array Nat := t.widths

-- | Filter rows where column c equals value v
def filter (t : Table) (c : Nat) (v : Cell) : Table :=
  let newRows := t.rows.filter fun row => row.getD c .null == v
  create t.cols newRows

-- | Sort table by column index (asc = true for ascending)
def sortBy (t : Table) (col : Nat) (asc : Bool) : Table :=
  let cmp := fun r1 r2 : Array Cell =>
    let c1 := r1.getD col .null
    let c2 := r2.getD col .null
    if asc then Cell.compare c1 c2 else Cell.compare c2 c1
  let sorted := t.rows.toList.mergeSort (fun a b => cmp a b == .lt) |>.toArray
  { t with rows := sorted }

-- | Build bar string with # chars
def mkBar (pct : Nat) (maxW : Nat := 20) : String :=
  let n := min maxW (pct * maxW / 100)
  String.ofList (List.replicate n '#')

-- | Frequency table for column c: (value, count, pct, bar) sorted by count desc
def freq (t : Table) (c : Nat) : Table :=
  let colName := t.cols.getD c ⟨"?"⟩ |>.name
  let total := t.nRows
  -- count occurrences using fold
  let counts := t.rows.foldl (init := #[]) fun acc row =>
    let v := row.getD c .null
    match acc.findIdx? (·.1 == v) with
    | some i => acc.set! i (v, acc[i]!.2 + 1)
    | none => acc.push (v, 1)
  -- sort by count descending
  let sorted := counts.qsort (fun a b => a.2 > b.2)
  -- build result table with pct and bar
  let cols := #[⟨colName⟩, ⟨"Cnt"⟩, ⟨"Pct"⟩, ⟨"Bar"⟩]
  let rows := sorted.map fun (v, n) =>
    let pct := if total > 0 then n * 100 / total else 0
    #[v, .int n, .str s!"{pct}%", .str (mkBar pct)]
  create cols rows

end Table

-- | INVARIANT: Table is for rendering only. All data ops go through Backend.
-- | DisplayInfo exposes only metadata needed for navigation/PRQL building.
structure DisplayInfo where
  colNames : Array String
  nRows    : Nat
  nCols    : Nat

-- | Extract display info from table (only way to get metadata)
def Table.info (t : Table) : DisplayInfo :=
  ⟨t.cols.map (·.name), t.nRows, t.nCols⟩

-- | INVARIANT: handleKey receives DisplayInfo, not Table.
-- | This makes it impossible to access cell data outside rendering.
-- | Enforcement: handleKey signature takes DisplayInfo, not Table.
