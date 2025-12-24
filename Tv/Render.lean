/-
  Table rendering to terminal
-/
import Tv.Types
import Tv.Viewport
import Tv.Term

namespace Render

-- | Render header row with underline attribute
def header (t : Table) (widths : Array Nat) (startCol endCol selCol : Nat) (y : UInt32) : IO Unit := do
  let mut x : UInt32 := 0
  for i in [startCol:endCol] do
    let col := t.cols.getD i default
    let w := widths.getD i 10
    let fg := if i == selCol then Term.black else Term.cyan ||| Term.underline
    let bg := if i == selCol then Term.cyan else Term.black
    Term.printPad x y w.toUInt32 fg bg col.name
    x := x + w.toUInt32 + 1

-- | Render single data row (with column viewport)
def row (t : Table) (widths : Array Nat) (startCol endCol : Nat) (rowIdx : Nat)
        (curRow curCol : Nat) (y : UInt32) : IO Unit := do
  let cells := t.rows.getD rowIdx #[]
  let isCurRow := rowIdx == curRow
  let mut x : UInt32 := 0
  for i in [startCol:endCol] do
    let cell := cells.getD i .null
    let w := widths.getD i 10
    let isCurCol := i == curCol
    let isCursor := isCurRow && isCurCol
    let (fg, bg) := if isCursor then (Term.black, Term.white)
                    else if isCurRow then (Term.white, Term.black)
                    else if isCurCol then (Term.yellow, Term.black)
                    else (Term.white, Term.black)
    Term.printPad x y w.toUInt32 fg bg cell.toString
    x := x + w.toUInt32 + 1

-- | Compute offset so cursor is visible (as last col when scrolling right)
-- Returns (startCol, endCol)
def visibleRange (widths : Array Nat) (cursor : Nat) (screenW : Nat) : Nat × Nat :=
  -- Find start by going backwards from cursor until we fill screen
  let curW := widths.getD cursor 10 + 1
  let rec findStart (i : Nat) (used : Nat) : Nat :=
    if i = 0 then 0
    else
      let w := widths.getD (i - 1) 10 + 1
      if used + w > screenW then i else findStart (i - 1) (used + w)
  let startCol := findStart cursor curW
  -- Find end by going forward from start
  let rec findEnd (i : Nat) (used : Nat) : Nat :=
    if i >= widths.size then i
    else
      let w := widths.getD i 10 + 1
      if used + w > screenW then i else findEnd (i + 1) (used + w)
  let endCol := findEnd startCol 0
  (startCol, endCol)

-- | Render table with viewport
def table (t : Table) (rowVP colVP : Viewport) (screenH screenW : Nat) : IO Unit := do
  Term.clear
  let widths := t.colWidths
  let curRow := rowVP.cursor
  let curCol := colVP.cursor
  -- column range: computed from cursor position
  let (startCol, endCol) := visibleRange widths curCol screenW
  -- row range: computed from cursor position (header + status = 2)
  let visRows := screenH - 2
  let startRow := if curRow < visRows then 0 else curRow - visRows + 1
  let endRow := min t.nRows (startRow + visRows)
  -- header at y=0
  header t widths startCol endCol curCol 0
  -- data rows start at y=1
  for i in [:endRow - startRow] do
    let ri := startRow + i
    row t widths startCol endCol ri curRow curCol (i + 1).toUInt32
  Term.present

-- | Render status bar at bottom
def statusBar (path : String) (curRow curCol nRows nCols : Nat) (y : UInt32) : IO Unit := do
  let pos := s!"{curRow + 1}/{nRows} {curCol + 1}/{nCols}"
  let msg := s!"{path}  {pos}"
  Term.print 0 y Term.cyan Term.black msg

end Render
