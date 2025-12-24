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

-- | Build all columns from offset (termbox clips at screen edge)
def buildFromLeft (widths : Array Nat) (offset : Nat) : Array ColPos :=
  let rec go (i x : Nat) (acc : Array ColPos) : Array ColPos :=
    if i >= widths.size then acc
    else
      let w := widths.getD i 10
      go (i + 1) (x + w + 1) (acc.push (i, x, w))
  go offset 0 #[]

-- | Find last fully visible column index given screen width
def lastVisible (cols : Array ColPos) (screenW : Nat) : Option Nat :=
  cols.findRev? (fun (_, x, w) => x + w ≤ screenW) |>.map (·.1)

-- | Visible range with proof cursor is visible
structure VisRange where
  cols   : Array ColPos
  offset : Nat
  cursor : Nat
  hVis   : offset ≤ cursor  -- cursor at or after first visible

-- | Scroll right until cursor visible, with termination proof
def scrollRight (widths : Array Nat) (offset cursor screenW : Nat) : Nat :=
  if offset ≥ cursor then cursor  -- cursor at leftmost
  else
    let cols := buildFromLeft widths offset
    match lastVisible cols screenW with
    | some last =>
      if cursor ≤ last then offset  -- cursor visible
      else scrollRight widths (offset + 1) cursor screenW
    | none => cursor
termination_by cursor - offset

-- | Compute offset to make cursor visible (loop until visible)
-- Returns offset ≤ cursor guaranteed by construction
def computeOffset (widths : Array Nat) (offset cursor screenW : Nat) : {o : Nat // o ≤ cursor} :=
  if offset > cursor then
    ⟨cursor, Nat.le_refl _⟩
  else
    let o := scrollRight widths offset cursor screenW
    -- o ≤ cursor: scrollRight returns cursor or offset where offset ≤ cursor
    if ho : o ≤ cursor then ⟨o, ho⟩ else ⟨cursor, Nat.le_refl _⟩

-- | Compute visible range with proof
def visibleRange (widths : Array Nat) (offset cursor screenW : Nat) : VisRange :=
  let ⟨newOffset, hVis⟩ := computeOffset widths offset cursor screenW
  let cols := buildFromLeft widths newOffset
  ⟨cols, newOffset, cursor, hVis⟩

-- | Render table with viewport, returns new column offset
def table (t : Table) (rowVP colVP : Viewport) (screenH screenW : Nat) : IO Nat := do
  Term.clear
  let widths := t.colWidths
  let curRow := rowVP.cursor
  let curCol := colVP.cursor
  -- visible columns with proof cursor is visible
  let vr := visibleRange widths colVP.offset curCol screenW
  let cols := vr.cols
  let newOffset := vr.offset
  -- row range: computed from cursor position (header + status = 2)
  let visRows := screenH - 2
  let startRow := if curRow < visRows then 0 else curRow - visRows + 1
  let endRow := min t.nRows (startRow + visRows)
  -- header at y=0
  header t cols curCol 0
  -- data rows start at y=1
  for i in [:endRow - startRow] do
    let ri := startRow + i
    row t cols ri curRow curCol (i + 1).toUInt32
  Term.present
  return newOffset

-- | Render status bar at bottom
def statusBar (path : String) (curRow curCol nRows nCols : Nat) (y : UInt32) : IO Unit := do
  let pos := s!"{curRow + 1}/{nRows} {curCol + 1}/{nCols}"
  let msg := s!"{path}  {pos}"
  Term.print 0 y Term.cyan Term.black msg

end Render
