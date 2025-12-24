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

-- | Key columns come first in display order
def keyColsFirst (keyCols : List Nat) (allCols : List Nat) : Bool :=
  allCols.take keyCols.length == keyCols

-- | Theorem: key columns are at the start of combined column list
theorem keyColsFirst_append (ks : List Nat) (rest : List Nat) :
    keyColsFirst ks (ks ++ rest) = true := by
  simp [keyColsFirst]

-- | Theorem: empty key columns trivially first
theorem keyColsFirst_empty (rest : List Nat) :
    keyColsFirst [] rest = true := by simp [keyColsFirst]

-- | Display order: key columns first, then rest
def displayOrder (keyCols : List Nat) (nCols : Nat) : List Nat :=
  keyCols ++ (List.range nCols).filter (!keyCols.contains ·)

-- | Navigation should follow display order (next column in display)
def nextInDisplay (keyCols : List Nat) (nCols : Nat) (cur : Nat) : Nat :=
  let order := displayOrder keyCols nCols
  match order.findIdx? (· == cur) with
  | some i => order.getD (i + 1) cur  -- next in order, or stay
  | none => cur

def prevInDisplay (keyCols : List Nat) (nCols : Nat) (cur : Nat) : Nat :=
  let order := displayOrder keyCols nCols
  match order.findIdx? (· == cur) with
  | some 0 => cur  -- at start, stay
  | some i => order.getD (i - 1) cur
  | none => cur

-- | Theorem: next in display advances in display order (concrete example)
-- keyCols=[1], 3 cols → display order is [1,0,2], cursor on 1 → next is 0
theorem nextInDisplay_example :
    nextInDisplay [1] 3 1 = 0 := by native_decide

-- | Theorem: with no key cols, next is just increment
theorem nextInDisplay_noKeys :
    nextInDisplay [] 5 2 = 3 := by native_decide

-- | Theorem: prev from first key col stays (can't go left of leftmost)
theorem prevInDisplay_atStart :
    prevInDisplay [1] 3 1 = 1 := by native_decide

-- | Theorem: prev from non-key goes to key col
theorem prevInDisplay_toKey :
    prevInDisplay [1] 3 0 = 1 := by native_decide

-- | Render table with viewport and key columns, returns new column offset
def table (t : Table) (rowVP colVP : Viewport) (screenH screenW : Nat)
          (keyCols : List Nat := []) : IO Nat := do
  Term.clear
  let widths := t.colWidths
  let curRow := rowVP.cursor
  let curCol := colVP.cursor
  -- compute key columns width (pinned left)
  let keyW := keyCols.foldl (fun acc i => acc + (widths.getD i 10) + 1) 0
  let sepW := if keyCols.isEmpty then 0 else 1  -- width of | separator
  let restW := screenW - keyW - sepW
  -- build key column positions (always visible, starting at x=0)
  let mut keyPos : Array ColPos := #[]
  let mut kx : Nat := 0
  for i in keyCols do
    let w := widths.getD i 10
    keyPos := keyPos.push (i, kx, w)
    kx := kx + w + 1
  -- non-key columns (scrollable, after separator)
  let nonKeyCols := (List.range t.nCols).filter (!keyCols.contains ·)
  let nonKeyWidths := nonKeyCols.map (widths.getD · 10) |>.toArray
  -- find cursor in non-key columns for scrolling
  let cursorInNonKey := nonKeyCols.findIdx? (· == curCol) |>.getD 0
  let vr := visibleRange nonKeyWidths colVP.offset cursorInNonKey restW
  -- build non-key column positions (offset by keyW + sepW)
  let startX := keyW + sepW
  let nonKeyPos := vr.cols.map fun (i, x, w) => (nonKeyCols.getD i 0, startX + x, w)
  -- combine: key cols + non-key cols
  let cols := keyPos ++ nonKeyPos
  -- row range
  let visRows := screenH - 2
  let startRow := if curRow < visRows then 0 else curRow - visRows + 1
  let endRow := min t.nRows (startRow + visRows)
  -- render header
  header t cols curCol 0
  -- render separator in header
  if !keyCols.isEmpty then
    Term.print (keyW).toUInt32 0 Term.white Term.black "|"
  -- render data rows
  for i in [:endRow - startRow] do
    let ri := startRow + i
    row t cols ri curRow curCol (i + 1).toUInt32
    -- render separator for each row
    if !keyCols.isEmpty then
      Term.print (keyW).toUInt32 (i + 1).toUInt32 Term.white Term.black "|"
  Term.present
  return vr.offset

-- | Render status bar at bottom
def statusBar (path : String) (curRow nRows viewCnt : Nat)
              (keyCols : List Nat) (cols : Array Column) (y : UInt32) : IO Unit := do
  let pos := s!"{curRow + 1}/{nRows}"
  let viewStr := if viewCnt > 1 then s!"[{viewCnt}] " else ""
  let keyStr := if keyCols.isEmpty then ""
    else " !" ++ String.intercalate "," (keyCols.map fun i => (cols.getD i default).name)
  let msg := s!"{viewStr}{path}  {pos}{keyStr}"
  Term.print 0 y Term.cyan Term.black msg

-- | Render info box (centered overlay)
def infoBox (t : Table) (col row : Nat) (screenH screenW : Nat) : IO Unit := do
  Term.clear
  let colName := t.cols.getD col default |>.name
  let cell := t.get row col
  let cellStr := cell.toString
  let cellLen := cellStr.length
  -- box content
  let lines := #[
    s!"Column: {colName}",
    s!"Row: {row + 1}/{t.nRows}",
    s!"Value: {cellStr}",
    s!"Length: {cellLen}",
    s!"Type: {match cell with | .null => "null" | .int _ => "int" | .float _ => "float" | .str _ => "str" | .bool _ => "bool"}"
  ]
  let boxW := lines.foldl (fun m l => max m l.length) 20
  let boxH := lines.size + 2
  let x0 := (screenW - boxW - 4) / 2
  let y0 := (screenH - boxH) / 2
  -- draw box
  Term.print x0.toUInt32 y0.toUInt32 Term.white Term.blue (String.ofList (List.replicate (boxW + 4) ' '))
  for i in [:lines.size] do
    let line := lines.getD i ""
    let padded := line ++ String.ofList (List.replicate (boxW - line.length) ' ')
    Term.print x0.toUInt32 (y0 + i + 1).toUInt32 Term.white Term.blue s!"  {padded}  "
  Term.print x0.toUInt32 (y0 + boxH - 1).toUInt32 Term.white Term.blue (String.ofList (List.replicate (boxW + 4) ' '))
  Term.print x0.toUInt32 (y0 + boxH).toUInt32 Term.cyan Term.black "Press q or Esc to close"
  Term.present

end Render
