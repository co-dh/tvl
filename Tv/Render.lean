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

-- | Cumulative x positions: cumX[i] = start x of column i from offset 0
def cumX (widths : Array Nat) : Array Nat :=
  widths.foldl (init := #[0]) fun acc w => acc.push (acc.back! + w + 1)

-- | Build visible columns from offset (stop at screenW)
def buildCols (widths : Array Nat) (cx : Array Nat) (offset screenW : Nat) : Array ColPos :=
  let x0 := cx.getD offset 0  -- x of first visible col from global origin
  let rec go (i : Nat) (acc : Array ColPos) : Array ColPos :=
    if i >= widths.size then acc
    else
      let x := cx.getD i 0 - x0  -- relative x from offset
      if x > screenW then acc
      else go (i + 1) (acc.push (i, x, widths.getD i 10))
  go offset #[]

-- | Find offset to make cursor visible using cumX
def findOffset (widths : Array Nat) (cx : Array Nat) (offset cursor screenW : Nat) : Nat :=
  if offset > cursor then cursor  -- scroll left
  else
    let curEnd := cx.getD cursor 0 + widths.getD cursor 10  -- cursor right edge
    let offX := cx.getD offset 0  -- offset left edge
    if curEnd - offX ≤ screenW then offset  -- cursor visible
    else
      -- find smallest o where cx[o] ≥ curEnd - screenW
      let minX := curEnd - screenW
      let rec search (o : Nat) : Nat :=
        if o ≥ cursor then cursor
        else if cx.getD o 0 ≥ minX then o
        else search (o + 1)
      termination_by cursor - o
      search offset

-- | Visible range with proof cursor is visible
structure VisRange where
  cols   : Array ColPos
  offset : Nat
  cursor : Nat
  hVis   : offset ≤ cursor

-- | Compute visible range (builds cumX once)
def visibleRange (widths : Array Nat) (offset cursor screenW : Nat) : VisRange :=
  let cx := cumX widths
  let o := findOffset widths cx offset cursor screenW
  if h : o ≤ cursor then
    ⟨buildCols widths cx o screenW, o, cursor, h⟩
  else
    ⟨buildCols widths cx cursor screenW, cursor, cursor, Nat.le_refl _⟩

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
