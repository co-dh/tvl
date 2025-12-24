/-
  Table rendering to terminal
-/
import Tv.Types
import Tv.Viewport
import Tv.Term

namespace Render

-- | Render header row
def header (t : Table) (widths : Array Nat) (selCol : Nat) (y : UInt32) : IO Unit := do
  let mut x : UInt32 := 0
  for i in [:t.cols.size] do
    let col := t.cols.getD i default
    let w := widths.getD i 10
    let fg := if i == selCol then Term.black else Term.cyan
    let bg := if i == selCol then Term.cyan else Term.black
    Term.printPad x y w.toUInt32 fg bg col.name
    x := x + w.toUInt32 + 1

-- | Render single data row
def row (t : Table) (widths : Array Nat) (rowIdx : Nat) (curRow curCol : Nat)
        (y : UInt32) : IO Unit := do
  let cells := t.rows.getD rowIdx #[]
  let isCurRow := rowIdx == curRow
  let mut x : UInt32 := 0
  for i in [:t.nCols] do
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

-- | Render table with viewport
def table (t : Table) (rowVP colVP : Viewport) (screenH : Nat) : IO Unit := do
  Term.clear
  let widths := t.colWidths
  let curRow := rowVP.cursor
  let curCol := colVP.cursor
  -- header at y=0
  header t widths curCol 0
  -- data rows
  let startRow := rowVP.offset
  let visRows := min (screenH - 2) (t.nRows - startRow)
  for i in [:visRows] do
    let ri := startRow + i
    row t widths ri curRow curCol (i + 1).toUInt32
  Term.present

-- | Render status bar at bottom
def statusBar (path : String) (curRow curCol nRows nCols : Nat) (y : UInt32) : IO Unit := do
  let pos := s!"{curRow + 1}/{nRows} {curCol + 1}/{nCols}"
  let msg := s!"{path}  {pos}"
  Term.print 0 y Term.cyan Term.black msg

end Render
