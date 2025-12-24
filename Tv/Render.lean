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

-- | Build columns left-to-right from offset
def buildFromLeft (widths : Array Nat) (offset screenW : Nat) : Array ColPos :=
  let rec go (i x : Nat) (acc : Array ColPos) : Array ColPos :=
    if i >= widths.size then acc
    else
      let w := widths.getD i 10
      if x + w > screenW then acc
      else go (i + 1) (x + w + 1) (acc.push (i, x, w))
  go offset 0 #[]

-- | Build columns right-to-left ending at screenW (cursor at right edge)
def buildFromRight (widths : Array Nat) (cursor screenW : Nat) : Array ColPos :=
  let curW := min (widths.getD cursor 10) screenW
  let curX := screenW - curW
  let rec goLeft (i x : Nat) (acc : Array ColPos) : Array ColPos :=
    if i = 0 then acc
    else
      let w := widths.getD (i - 1) 10
      if w + 1 > x then acc
      else goLeft (i - 1) (x - w - 1) (#[(i - 1, x - w - 1, w)] ++ acc)
  (goLeft cursor curX #[]).push (cursor, curX, curW)

-- | Get first/last column index from cols
def colRange (cols : Array ColPos) : Option (Nat × Nat) :=
  match cols[0]?, cols.back? with
  | some (f, _, _), some (l, _, _) => some (f, l)
  | _, _ => none

-- | Compute visible columns based on offset and cursor
-- Left-align from offset, scroll right/left when cursor out of view
def visibleRange (widths : Array Nat) (offset cursor screenW : Nat) : Array ColPos × Nat :=
  let cols := buildFromLeft widths offset screenW
  match colRange cols with
  | some (first, last) =>
    if cursor > last then
      -- scroll right: cursor at right edge
      (buildFromRight widths cursor screenW, cursor)
    else if cursor < first then
      -- scroll left: cursor at left edge
      (buildFromLeft widths cursor screenW, cursor)
    else
      -- cursor visible, keep current offset
      (cols, offset)
  | none =>
    -- no columns fit, just show cursor column
    (buildFromRight widths cursor screenW, cursor)

-- | Render table with viewport, returns new column offset
def table (t : Table) (rowVP colVP : Viewport) (screenH screenW : Nat) : IO Nat := do
  Term.clear
  let widths := t.colWidths
  let curRow := rowVP.cursor
  let curCol := colVP.cursor
  -- visible columns based on offset and cursor
  let (cols, newOffset) := visibleRange widths colVP.offset curCol screenW
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
