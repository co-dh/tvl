/-
  Qtv.Types - Core types matching Q version
  Q: t, GL, st, reg
-/

-- | Cell value (matches Q types: numeric, string, symbol, null)
inductive Cell where
  | null
  | int (v : Int)
  | float (v : Float)
  | str (v : String)
  | sym (v : String)  -- Q symbol type
  | bool (v : Bool)
  deriving Repr, Inhabited

namespace Cell

-- | Format int with commas
-- Q: commify:{","sv reverse 3 cut reverse string x}
def fmtInt (n : Int) : String :=
  let s := s!"{n.natAbs}"
  let cs := s.toList.reverse
  let rec go (xs : List Char) (acc : List Char) (i : Nat) : List Char :=
    match xs with
    | [] => acc
    | c :: rest =>
      let acc' := if i > 0 && i % 3 = 0 then c :: ',' :: acc else c :: acc
      go rest acc' (i + 1)
  let r := go cs [] 0
  if n < 0 then "-" ++ String.ofList r else String.ofList r

def isNum : Cell → Bool
  | .int _ | .float _ => true
  | _ => false

def toString : Cell → String
  | .null => ""
  | .int n => fmtInt n
  | .float f => s!"{f}"
  | .str s => s
  | .sym s => s
  | .bool b => if b then "1b" else "0b"

-- | Raw value for search/filter (no formatting)
def toRaw : Cell → String
  | .null => ""
  | .int n => s!"{n}"
  | .float f => s!"{f}"
  | .str s => s
  | .sym s => s
  | .bool b => if b then "1" else "0"

instance : ToString Cell := ⟨toString⟩

end Cell

-- | Column: name + data array
structure Col where
  name : String
  data : Array Cell
  deriving Repr

-- | Table: array of columns
-- Q: t (flip of dict)
structure Tbl where
  cols : Array Col
  deriving Repr

namespace Tbl

def empty : Tbl := ⟨#[]⟩

-- Q: count t
def nRows (t : Tbl) : Nat :=
  t.cols.getD 0 ⟨"", #[]⟩ |>.data.size

def nCols (t : Tbl) : Nat := t.cols.size

-- | Get column by index
def col (t : Tbl) (i : Nat) : Option Col :=
  if h : i < t.cols.size then some t.cols[i] else none

-- | Get column by name
def colByName (t : Tbl) (nm : String) : Option Col :=
  t.cols.find? (·.name == nm)

-- | Get cell at (row, col)
-- Q: t[r;c]
def get (t : Tbl) (r c : Nat) : Cell :=
  t.cols.getD c ⟨"", #[]⟩ |>.data.getD r .null

-- | Column names
-- Q: cols t
def colNames (t : Tbl) : Array String :=
  t.cols.map (·.name)

-- | Delete column by index
-- Q: del:{$[1<count cols y; ![y;();0b;(),x]; y]}
def delCol (t : Tbl) (c : Nat) : Tbl :=
  if h : c < t.cols.size then ⟨t.cols.eraseIdx c h⟩ else t

-- | Copy column
-- Q: cp:{nc:`$string[x],"1"; i:1+cols[y]?x; ((i#cols[y]),nc,i _ cols y)xcols ![y;();0b;enlist[nc]!enlist x]}
def cpCol (t : Tbl) (c : Nat) : Tbl :=
  match t.col c with
  | some col =>
    let nc := { col with name := col.name ++ "1" }
    let before := t.cols.extract 0 (c + 1)
    let after := t.cols.extract (c + 1) t.cols.size
    ⟨before ++ #[nc] ++ after⟩
  | none => t

-- | Rename column
-- Q: ren:{(enlist[y]!enlist[x]) xcol z}
def renCol (t : Tbl) (c : Nat) (nm : String) : Tbl :=
  if h : c < t.cols.size then
    let col := t.cols[c]
    ⟨t.cols.set c { col with name := nm }⟩
  else t

-- | Reorder columns: put keys first
-- Q: xcols
def xcols (t : Tbl) (keys : Array String) : Tbl :=
  let keyCols := keys.filterMap t.colByName
  let rest := t.cols.filter (fun c => !keys.contains c.name)
  ⟨keyCols ++ rest⟩

end Tbl

-- | Globals for one view
-- Q: GL:`r0`cr`cc`kc`type`t
-- r0: first row on screen, cr: cursor row, cc: cursor col, kc: key column count
structure GL where
  r0  : Nat := 0
  cr  : Nat := 0
  cc  : Nat := 0
  kc  : Nat := 0
  typ : String := ""  -- view type name
  t   : Tbl := .empty
  deriving Repr

-- | Registry for search patterns
-- Q: reg:enlist[::]!enlist[::]
abbrev Reg := Array (Char × String)

-- | View stack entry
-- Q: st:enlist (GL:`r0`cr`cc`kc`type`t)!(0;0;0;0;`;t)
abbrev ViewStack := Array GL
