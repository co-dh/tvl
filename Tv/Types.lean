/-
  Core types: Cell, Column, Table, PureKey
  Table stores columns by name (HashMap) for direct name-based access
-/
import Std.Data.HashMap
import Tv.Adbc

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

-- | Toggle element in array (add if absent, remove if present)
def Array.toggle [BEq α] (arr : Array α) (x : α) : Array α :=
  if arr.contains x then arr.filter (· != x) else arr.push x

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

-- | Extract string value
def str? : Cell → Option String | .str s => some s | _ => none

-- | Extract int value
def int? : Cell → Option Int | .int n => some n | _ => none

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

-- | DisplayInfo: metadata for navigation/PRQL building (no cell access)
structure DisplayInfo where
  colNames  : Array String
  colWidths : Array Nat
  nRows     : Nat
  nCols     : Nat

-- | Zero-copy table: data stays in Arrow/C memory, accessed via FFI
structure SomeTable where
  qr        : Adbc.QueryResult   -- arrow data (opaque, C memory)
  colNames  : Array String       -- cached column names
  colWidths : Array Nat          -- cached column widths
  colFmts   : Array Char         -- cached format chars per column
  nRows     : Nat
  nCols     : Nat

namespace SomeTable

-- | Cap for column width
def maxColWidth : Nat := 50

-- | Build SomeTable from QueryResult (caches metadata, no cell copies)
def ofQueryResult (qr : Adbc.QueryResult) : IO SomeTable := do
  let nc ← Adbc.ncols qr
  let nr ← Adbc.nrows qr
  let mut names : Array String := #[]
  let mut fmts : Array Char := #[]
  for i in [:nc.toNat] do
    let n ← Adbc.colName qr i.toUInt64
    names := names.push n
    let fmt ← Adbc.colFmt qr i.toUInt64
    fmts := fmts.push (if h : fmt.length > 0 then fmt.toList[0] else '?')
  let widths ← Adbc.colWidths qr
  let widths := widths.map (min maxColWidth)
  pure ⟨qr, names, widths, fmts, nr.toNat, nc.toNat⟩

-- | Get cell at (row, col) - pure interface via unsafeIO
@[inline] unsafe def getIdxImpl (t : SomeTable) (row col : Nat) : Cell :=
  match unsafeIO (do
    let isNull ← Adbc.cellIsNull t.qr row.toUInt64 col.toUInt64
    if isNull then return Cell.null
    let ch := t.colFmts.getD col '?'
    match ch with
    | 'l' | 'i' | 's' | 'c' => return Cell.int (← Adbc.cellInt t.qr row.toUInt64 col.toUInt64)
    | 'g' | 'f' | 'd' => return Cell.float (← Adbc.cellFloat t.qr row.toUInt64 col.toUInt64)
    | 'b' => return Cell.bool ((← Adbc.cellStr t.qr row.toUInt64 col.toUInt64) == "true")
    | _ => return Cell.str (← Adbc.cellStr t.qr row.toUInt64 col.toUInt64)) with
  | Except.ok c => c
  | Except.error _ => Cell.null

@[implemented_by getIdxImpl]
def getIdx (t : SomeTable) (_ _ : Nat) : Cell := .null

-- | Get DisplayInfo
def info (t : SomeTable) : DisplayInfo :=
  ⟨t.colNames, t.colWidths, t.nRows, t.nCols⟩

-- | Empty table
def empty : IO SomeTable := do
  let qr ← Adbc.query "SELECT 1 WHERE 1=0"
  ofQueryResult qr

end SomeTable

-- | All pure key operations (no IO needed)
inductive PureKey where
  -- navigation
  | j | k | l | h | g | G | _0 | _1 | dollar | c_d | c_u
  | colJump (idx : DispIdx)
  -- view transforms
  | asc | desc | D | I | dup | swap
  | bang | spc | incDec (inc : Bool) | q | esc
  -- views (push new view)
  | F | pushFilter (expr : String) | selectCols (cols : Array String)
  | pushMeta (metaTbl : SomeTable) | pushFile (path : String)
  -- input modes
  | inputRename
  -- agg (funcs as strings: "count", "sum", "average", "min", "max", "stddev")
  | pushAgg (keys : Array String) (funcs : Array String) (cols : Array String)
  -- enter key
  | ret
