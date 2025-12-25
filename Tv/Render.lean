/-
  Table rendering to terminal
-/
import Tv.Types
import Tv.Viewport
import Tv.Term

namespace Render

-- | Column position: (colIdx, xPos, width)
abbrev ColPos := Nat × Nat × Nat

-- | Render header row with underline attribute (highlights selected columns)
def header (t : Table) (cols : Array ColPos) (selCol : Nat) (y : UInt32)
           (selCols : List Nat := []) : IO Unit := do
  for (i, x, w) in cols do
    let col := t.cols.getD i default
    let isSel := selCols.contains i
    let (fg, bg) := if i == selCol then (Term.black, Term.cyan)
                    else if isSel then (Term.black, Term.magenta)
                    else (Term.cyan ||| Term.underline, Term.default)
    Term.printPad x.toUInt32 y w.toUInt32 fg bg col.name

-- | Render single data row with decimal precision (highlights selected cols/rows)
def row (t : Table) (cols : Array ColPos) (rowIdx curRow curCol decimals : Nat)
        (y : UInt32) (selCols : List Nat := []) (selRows : List Nat := []) : IO Unit := do
  let cells := t.rows.getD rowIdx #[]
  let isCurRow := rowIdx == curRow
  let isSelRow := selRows.contains rowIdx
  for (i, x, w) in cols do
    let cell := cells.getD i .null
    let isCursor := isCurRow && i == curCol
    let isSel := selCols.contains i
    let (fg, bg) := if isCursor then (Term.black, Term.white)
                    else if isSelRow then (Term.black, Term.green)  -- selected row
                    else if isSel && isCurRow then (Term.black, Term.magenta)
                    else if isSel then (Term.magenta, Term.default)
                    else if isCurRow then (Term.default, Term.default)
                    else if i == curCol then (Term.yellow, Term.default)
                    else (Term.default, Term.default)
    let cellStr := cell.toStringD decimals
    if cell.isNum then
      Term.printPadR x.toUInt32 y w.toUInt32 fg bg cellStr
    else
      Term.printPad x.toUInt32 y w.toUInt32 fg bg cellStr

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

-- | Render table with viewport and key columns, returns (offset, cols, keyW)
def table (t : Table) (rowVP colVP : Viewport) (screenH screenW : Nat)
          (keyCols : List Nat := []) (decimals : Nat := 3)
          (selCols : List Nat := []) (selRows : List Nat := []) : IO (Nat × Array ColPos × Nat) := do
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
  -- row range (screenH-1: 1 for header at top)
  let visRows := screenH - 1
  let startRow := if curRow < visRows then 0 else curRow - visRows + 1
  let endRow := min t.nRows (startRow + visRows)
  -- render header
  header t cols curCol 0 selCols
  -- render separator in header
  if !keyCols.isEmpty then
    Term.print (keyW).toUInt32 0 Term.default Term.default "|"
  -- render data rows
  for i in [:endRow - startRow] do
    let ri := startRow + i
    row t cols ri curRow curCol decimals (i + 1).toUInt32 selCols selRows
    -- render separator for each row
    if !keyCols.isEmpty then
      Term.print (keyW).toUInt32 (i + 1).toUInt32 Term.default Term.default "|"
  return (vr.offset, cols, keyW)

-- | Format number with comma separators (1000000 -> "1,000,000")
def fmtNum (n : Nat) : String :=
  let s := toString n
  if s.length <= 3 then s
  else
    let rec go (cs : List Char) (i : Nat) : List Char :=
      match cs with
      | [] => []
      | c :: rest =>
        if i > 0 && i % 3 == 0 then ',' :: c :: go rest (i + 1)
        else c :: go rest (i + 1)
    (go s.toList.reverse 0).reverse |> String.ofList

-- | Get memory usage in MB from /proc/self/status
def memMB : IO Nat := do
  try
    let s ← IO.FS.readFile "/proc/self/status"
    -- find "VmRSS:" line, parse kB value
    for line in s.splitOn "\n" do
      if line.startsWith "VmRSS:" then
        let parts := line.splitOn " " |>.filter (!·.isEmpty)
        if parts.length >= 2 then
          return (parts[1]!.toNat? |>.getD 0) / 1024
    return 0
  catch _ => return 0

-- | Shorten PRQL: "from df | freq {a} (df)" -> "freq a"
def shortenPrql (prql : String) : String :=
  if prql == "from df" then ""
  else if prql.startsWith "from df | " then
    let rest := prql.drop 10  -- drop "from df | "
    -- simplify common patterns
    if rest.startsWith "freq {" then
      let col := rest.drop 6 |>.takeWhile (· != '}')
      s!"freq {col}"
    else if rest.startsWith "filter " then
      s!"filter {rest.drop 7}"
    else if rest.startsWith "sort " then
      s!"sort {rest.drop 5}"
    else rest
  else prql

-- | Shorten path for display (strip source: prefix)
def shortenPath (p : String) : String :=
  if p.startsWith "source:" then p.drop 7 else p

-- | Render tab line: view1 | view2 | ... (all views on stack)
def tabLine (views : List (String × String × String)) (y : UInt32) (screenW : Nat) : IO Unit := do
  -- views: (path, disp, prql); head=current, tail=parents; reverse for display
  let rev := views.reverse
  let n := rev.length
  -- build labels, omit path if same as previous
  let (labels, _) := rev.foldl (init := ([], ("", 0))) fun (acc, (prevPath, idx)) (path, d, p) =>
    let lbl := if d.isEmpty then shortenPrql p else d
    let sp := shortenPath path
    let txt := if path == prevPath then (if lbl.isEmpty then s!"#{idx+1}" else lbl)
               else if lbl.isEmpty then sp else s!"{sp} {lbl}"
    (acc ++ [txt], (path, idx + 1))
  -- bracket current view (last after reverse)
  let marked := (List.range n).zip labels |>.map fun (i, lbl) =>
    if i == n - 1 then s!"[{lbl}]" else lbl
  Term.printPad 0 y screenW.toUInt32 Term.white Term.blue (String.intercalate " | " marked)

-- | Render status bar at bottom
def statusBar (curRow total screenW : Nat) (keyCols selCols selRows : List Nat)
              (colNames : Array String) (y : UInt32) (msg : String := "") : IO Unit := do
  -- left side: message or key/sel columns/rows
  let left := if msg.isEmpty then
    let keyStr := if keyCols.isEmpty then "" else s!"keys={keyCols.length} "
    let selStr := if selCols.isEmpty then ""
      else s!"sel={selCols.length} *" ++ String.intercalate "," (selCols.map fun i => colNames.getD i "?")
    let rowStr := if selRows.isEmpty then "" else s!" rows={selRows.length}"
    s!"{keyStr}{selStr}{rowStr}"
  else msg
  -- right side: mem + row/total
  let mb ← memMB
  let right := s!"{mb}MB {curRow}/{fmtNum total}"
  -- print left, then right-aligned position
  Term.print 0 y Term.cyan Term.default left
  let rx := screenW - right.length
  Term.print rx.toUInt32 y Term.cyan Term.default right

-- | Key bindings for info overlay (2 columns: key | hint)
def keyHints : List (String × String) := [
  ("j/k", "up/down"), ("h/l", "left/right"),
  ("g/G", "top/end"), ("^D/^U", "page"),
  ("0/$", "first/last"), ("[/]", "sort"),
  ("\\", "filter"), ("F", "freq"),
  ("M", "meta"), ("D", "delete"),
  ("s", "select"), ("!", "key col"),
  ("b", "agg"), ("T", "dup"),
  ("S", "swap"), (":", "cmd"),
  ("r", "lr"), ("q", "quit")
]

-- | Render info overlay at bottom-right (key | hint)
def infoOverlay (_ : Table) (_ _ : Nat) (screenH screenW : Nat) : IO Unit := do
  let nRows := keyHints.length
  let keyW := 5; let hintW := 10
  let boxW := keyW + 1 + hintW
  let x0 := screenW - boxW - 2
  let y0 := screenH - nRows - 3
  for i in [:nRows] do
    let (k, d) := keyHints.getD i ("", "")
    let kpad := String.ofList (List.replicate (keyW - k.length) ' ') ++ k
    let dpad := d.take hintW ++ String.ofList (List.replicate (hintW - min d.length hintW) ' ')
    Term.print x0.toUInt32 (y0 + i).toUInt32 Term.black Term.yellow (kpad ++ " " ++ dpad)

end Render
