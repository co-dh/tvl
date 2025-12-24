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

def toString : Cell → String
  | .null    => "∅"
  | .int n   => s!"{n}"
  | .float f => s!"{f}"
  | .str s   => s
  | .bool b  => if b then "true" else "false"

end Cell

-- | Column metadata
structure Column where
  name : String
  deriving Repr, Inhabited

-- | Table with sized dimensions
-- Uses Array for simplicity; Vector for stricter sizing
structure Table where
  cols : Array Column
  rows : Array (Array Cell)
  deriving Repr

namespace Table

def nCols (t : Table) : Nat := t.cols.size
def nRows (t : Table) : Nat := t.rows.size

def empty : Table := ⟨#[], #[]⟩

-- | Get cell at (row, col), default to null
def get (t : Table) (r c : Nat) : Cell :=
  t.rows.getD r #[] |>.getD c .null

-- | Delete column at index
def delCol (t : Table) (idx : Nat) : Table :=
  if idx ≥ t.nCols then t
  else
    { cols := t.cols.eraseIdx! idx
    , rows := t.rows.map (·.eraseIdx! idx) }

-- | Get column widths (max of header and data, capped at 20)
def colWidths (t : Table) : Array Nat :=
  let hdrW := t.cols.map (·.name.length)
  let dataW := t.rows.foldl (init := hdrW) fun acc row =>
    let rowW := row.map (Cell.toString ·) |>.map String.length
    Array.zipWith (fun a b => max a b) acc rowW
  dataW.map (fun w => min 20 w)

end Table
