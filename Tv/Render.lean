/-
  Table rendering to terminal
-/
import Tv.Types
import Tv.Viewport
import Tv.Term

namespace Render

-- | Column position: (colIdx, xPos, width)
abbrev ColPos := Nat × Nat × Nat

-- | Render header row with underline attribute
def header (t : Table) (cols : Array ColPos) (selCol : Nat) (y : UInt32) : IO Unit := do
  for (i, x, w) in cols do
    let col := t.cols.getD i default
    let fg := if i == selCol then Term.black else Term.cyan ||| Term.underline
    let bg := if i == selCol then Term.cyan else Term.black
    Term.printPad x.toUInt32 y w.toUInt32 fg bg col.name

-- | Render single data row
def row (t : Table) (cols : Array ColPos) (rowIdx curRow curCol : Nat) (y : UInt32) : IO Unit := do
  let cells := t.rows.getD rowIdx #[]
  let isCurRow := rowIdx == curRow
  for (i, x, w) in cols do
    let cell := cells.getD i .null
    let isCursor := isCurRow && i == curCol
    let (fg, bg) := if isCursor then (Term.black, Term.white)
                    else if isCurRow then (Term.white, Term.black)
                    else if i == curCol then (Term.yellow, Term.black)
                    else (Term.white, Term.black)
    if cell.isNum then
      Term.printPadR x.toUInt32 y w.toUInt32 fg bg cell.toString
    else
      Term.printPad x.toUInt32 y w.toUInt32 fg bg cell.toString

-- | Visible columns with right-alignment proof
structure VisRange where
  cols    : Array ColPos           -- visible columns with positions
  screenW : Nat
  hAlign  : cols.back?.map (fun (_, x, w) => x + w) = some screenW

-- | Compute visible columns right-to-left from cursor
-- Cursor column ends at screenW, previous columns go left
def visibleRange (widths : Array Nat) (cursor : Nat) (screenW : Nat) : VisRange :=
  let curW := min (widths.getD cursor 10) screenW  -- clamp to screenW
  let curX := screenW - curW
  -- build columns right-to-left
  let rec goLeft (i : Nat) (x : Nat) (acc : Array ColPos) : Array ColPos :=
    if i = 0 then acc
    else
      let w := widths.getD (i - 1) 10
      if w + 1 > x then acc  -- no room (need gap too)
      else
        let x' := x - w - 1
        goLeft (i - 1) x' (#[(i - 1, x', w)] ++ acc)
  let leftCols := goLeft cursor curX #[]
  let cols := leftCols.push (cursor, curX, curW)
  have hCurW : curW ≤ screenW := Nat.min_le_right _ _
  have hAlign : curX + curW = screenW := Nat.sub_add_cancel hCurW
  ⟨cols, screenW, by simp [Array.back?, cols, hAlign]⟩

-- | Render table with viewport
def table (t : Table) (rowVP colVP : Viewport) (screenH screenW : Nat) : IO Unit := do
  Term.clear
  let widths := t.colWidths
  let curRow := rowVP.cursor
  let curCol := colVP.cursor
  -- visible columns: right-aligned from cursor
  let vr := visibleRange widths curCol screenW
  -- row range: computed from cursor position (header + status = 2)
  let visRows := screenH - 2
  let startRow := if curRow < visRows then 0 else curRow - visRows + 1
  let endRow := min t.nRows (startRow + visRows)
  -- header at y=0
  header t vr.cols curCol 0
  -- data rows start at y=1
  for i in [:endRow - startRow] do
    let ri := startRow + i
    row t vr.cols ri curRow curCol (i + 1).toUInt32
  Term.present

-- | Render status bar at bottom
def statusBar (path : String) (curRow curCol nRows nCols : Nat) (y : UInt32) : IO Unit := do
  let pos := s!"{curRow + 1}/{nRows} {curCol + 1}/{nCols}"
  let msg := s!"{path}  {pos}"
  Term.print 0 y Term.cyan Term.black msg

end Render
