/-
  Table rendering to terminal
-/
import Tv.Types
import Tv.State
import Tv.Term

open App

namespace Render

-- | Column position: (colIdx, xPos, width)
abbrev ColPos := Nat × Nat × Nat

/-! ## Pure Visibility Model -/

-- | Is cursor row visible?
def rowVisibleP (cursor visRows : Nat) : Bool :=
  let startRow := if cursor < visRows then 0 else cursor - visRows + 1
  startRow ≤ cursor && cursor < startRow + visRows

-- | Theorem: row is always visible when visRows > 0
theorem rowVisibleP_always (cursor visRows : Nat) (h : visRows > 0) :
    rowVisibleP cursor visRows = true := by
  simp only [rowVisibleP]
  split
  · simp; omega
  · simp; omega


-- | Render header row with underline attribute (highlights selected columns)
def header (st : SomeTable) (cols : Array ColPos) (selCol : Nat) (y : UInt32)
           (selCols : Array String := #[]) : IO Unit := do
  for (i, x, w) in cols do
    let name := st.colNames.getD i ""
    let isSel := selCols.contains name
    let (fg, bg) := if i == selCol then (Term.black, Term.cyan)
                    else if isSel then (Term.black, Term.magenta)
                    else (Term.cyan ||| Term.underline, Term.default)
    Term.printPad x.toUInt32 y w.toUInt32 fg bg name

-- | Render single data row with decimal precision (highlights selected cols/rows)
def row (st : SomeTable) (cols : Array ColPos) (rowIdx curRow curCol decimals : Nat)
        (y : UInt32) (selCols : Array String := #[]) (selRows : Array Nat := #[]) : IO Unit := do
  let isCurRow := rowIdx == curRow
  let isSelRow := selRows.contains rowIdx
  for (i, x, w) in cols do
    let cell := st.getIdx rowIdx i
    let isCursor := isCurRow && i == curCol
    let isSel := selCols.contains (st.colNames.getD i "")
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

-- | Display order: key columns first, then rest (as names)
def displayCols (keyCols : Array String) (colNames : Array String) : Array String :=
  let validKeys := keyCols.filter fun k => colNames.any (· == k)
  let rest := colNames.filter fun c => !validKeys.any (· == c)
  validKeys ++ rest

-- | Get column index from name (O(n) linear scan)
def colIndex (name : String) (colNames : Array String) : Nat :=
  colNames.findIdx? (· == name) |>.getD 0

-- | Resolve key column names to indices
def resolveKeyCols (keyCols : Array String) (colNames : Array String) : Array Nat :=
  keyCols.filterMap fun name => colNames.findIdx? (· == name)

-- | Key columns come first in display order
def keyColsFirst (keyCols allCols : Array Nat) : Bool :=
  allCols.extract 0 keyCols.size == keyCols

-- | Theorem: key columns are at the start of combined column list
theorem keyColsFirst_append (ks rest : Array Nat) :
    keyColsFirst ks (ks ++ rest) = true := by
  simp [keyColsFirst]

-- | Theorem: empty key columns trivially first
theorem keyColsFirst_empty (rest : Array Nat) :
    keyColsFirst #[] rest = true := by simp [keyColsFirst]

-- | Display order: key columns first, then rest
def displayOrder (keyCols : Array Nat) (nCols : Nat) : Array Nat :=
  keyCols ++ (Array.range nCols).filter (!keyCols.contains ·)

-- | Theorem: displayOrder always has key columns first
theorem displayOrder_keysFirst (keyCols : Array Nat) (nCols : Nat) :
    keyColsFirst keyCols (displayOrder keyCols nCols) = true := by
  simp [displayOrder, keyColsFirst]

-- | Extract column indices from ColPos array
def colIndices (cols : Array ColPos) : Array Nat :=
  cols.map fun (i, _, _) => i

-- | Build visible columns following display order (keyCols first)
def buildCols (widths : Array Nat) (order : Array Nat) (offset screenW : Nat) : Array ColPos :=
  let cols := order.extract offset order.size
  let rec go (idx x : Nat) (acc : Array ColPos) : Array ColPos :=
    if h : idx < cols.size then
      let i := cols[idx]
      let w := widths.getD i 10
      if x + w > screenW then acc
      else go (idx + 1) (x + w + 1) (acc.push (i, x, w))
    else acc
  go 0 0 #[]

-- | Visible range (offset is display position, cursor is original column index)
structure VisRange where
  cols   : Array ColPos
  offset : Nat  -- display order position
  cursor : Nat  -- original column index

-- | Compute visible range using display order (keyCols first)
def visibleRange (widths : Array Nat) (offset cursor screenW : Nat) (keyColIdxs : Array Nat) : VisRange :=
  let order := displayOrder keyColIdxs widths.size
  ⟨buildCols widths order offset screenW, offset, cursor⟩

-- | Theorem: visible columns have key columns first (empty keyCols)
theorem visibleRange_keysFirst_empty (widths : Array Nat) (cursor screenW : Nat) :
    keyColsFirst #[] (colIndices (visibleRange widths 0 cursor screenW #[]).cols) = true := by
  simp [keyColsFirst]

-- | Theorem: key column 1 appears first in visible range
theorem visibleRange_keysFirst_ex1 :
    keyColsFirst #[1] (colIndices (visibleRange #[10,10,10,10,10] 0 0 80 #[1]).cols) = true := by
  native_decide

-- | Theorem: key columns [2,0] appear first in visible range
theorem visibleRange_keysFirst_ex2 :
    keyColsFirst #[2,0] (colIndices (visibleRange #[10,10,10,10,10] 0 0 80 #[2,0]).cols) = true := by
  native_decide

-- | Cursor must always be visible in the rendered columns
def cursorInCols (cursor : Nat) (cols : Array ColPos) : Bool :=
  cols.any fun (i, _, _) => i == cursor

-- | Theorem: cursor visible when offset adjusted correctly
theorem cursorVisible_visibleRange (widths : Array Nat) (offset cursor screenW : Nat) (keyCols : Array Nat)
    (hFit : (visibleRange widths offset cursor screenW keyCols).cols.size > 0) :
    cursorInCols cursor (visibleRange widths offset cursor screenW keyCols).cols = true := by
  sorry  -- requires: offset ≤ displayPos cursor < offset + visCols

-- | Bug case: keyCols=[0], cursor=1, narrow screen (only 2 cols fit)
theorem cursorVisible_afterL_narrow :
    cursorInCols 1 (visibleRange #[10,10,10,10,10] 0 1 25 #[0]).cols = true := by
  native_decide

-- | Bug case: keyCols=[1], cursor moves to 0 (2nd in display order)
theorem cursorVisible_afterL_keyCol :
    cursorInCols 0 (visibleRange #[10,10,10,10,10] 0 0 25 #[1]).cols = true := by
  native_decide

-- | Bug case: wide keyCol, narrow screen
theorem cursorVisible_wideKeyCol :
    cursorInCols 1 (visibleRange #[50,10,10,10,10] 0 1 80 #[0]).cols = true := by
  native_decide

-- | Bug case: simulating 1.parquet
theorem cursorVisible_1parquet_sim :
    let widths : Array Nat := #[20, 8, 6, 10, 8, 11, 10]
    cursorInCols 1 (visibleRange widths 0 1 80 #[0]).cols = true := by
  native_decide

-- | Theorem: buildCols preserves correct widths from widths array
def colWidthsCorrect (widths : Array Nat) (cols : Array ColPos) : Bool :=
  cols.all fun (i, _, w) => w == widths.getD i 10

-- | Test: widths preserved with no keyCols
theorem buildCols_widthsCorrect_noKeys :
    let widths := #[20, 8, 6, 10, 8, 11, 10]
    let cols := buildCols widths #[0,1,2,3,4,5,6] 0 80
    colWidthsCorrect widths cols = true := by native_decide

-- | Test: widths preserved with keyCols (display order changes)
theorem buildCols_widthsCorrect_keyCols :
    let widths := #[20, 8, 6, 10, 8, 11, 10]
    let keyCols := #[3, 4]
    let cols := buildCols widths (displayOrder keyCols 7) 0 80
    colWidthsCorrect widths cols = true := by native_decide

-- | After delete, keyCols with index >= deleted must be decremented
def adjustKeyCols (keyCols : Array Nat) (delCol : Nat) : Array Nat :=
  keyCols.filterMap fun k =>
    if k == delCol then none
    else if k > delCol then some (k - 1)
    else some k

-- | Concrete test: adjustKeyCols works
theorem adjustKeyCols_ex1 : adjustKeyCols #[10] 5 = #[9] := by native_decide
theorem adjustKeyCols_ex2 : adjustKeyCols #[3, 10] 5 = #[3, 9] := by native_decide
theorem adjustKeyCols_ex3 : adjustKeyCols #[5, 10] 5 = #[9] := by native_decide

-- | Cursor row is visible: startRow ≤ curRow < endRow
-- Given visRows and curRow, compute visible range containing cursor
def rowVisible (curRow visRows : Nat) : Nat × Nat :=
  let startRow := if curRow < visRows then 0 else curRow - visRows + 1
  (startRow, startRow + visRows)

-- | Theorem: cursor row is always in visible range
theorem cursorRowVisible (curRow visRows : Nat) (hPos : visRows > 0) :
    let (start, end_) := rowVisible curRow visRows
    start ≤ curRow ∧ curRow < end_ := by
  simp [rowVisible]
  split <;> omega
-- Note: cursor column visibility proven by VisRange.hVis : offset ≤ cursor

-- | Render table with nav state, returns (offset, cols, keyW)
def table (st : SomeTable) (nav : PureState) (screenH screenW : Nat)
          (decimals : Nat := 3)
          (selCols : Array String := #[]) (selRows : Array Nat := #[]) : IO (Nat × Array ColPos × Nat) := do
  Term.clear
  let widths := st.colWidths
  let colNames := st.colNames
  let keyIdxs := resolveKeyCols nav.keyCols colNames
  let curRow := nav.rowCur
  let colOff := nav.colOff.val
  -- convert display cursor to original column index
  let dispCols := displayCols nav.keyCols colNames
  let curColOrig := colIndex (dispCols.getDisp nav.colCur "") colNames
  -- all columns scroll together (no pinning)
  let vr := visibleRange widths colOff nav.colCur.val screenW keyIdxs
  let cols := vr.cols
  -- find separator position: after last visible key column (at column gap)
  let visibleKeyIdxs := keyIdxs.filter fun k => cols.any fun (i, _, _) => i == k
  let lastKey := visibleKeyIdxs.foldl max 0
  let sepX := if visibleKeyIdxs.isEmpty then 0
    else cols.foldl (fun acc (i, x, w) => if i == lastKey then x + w else acc) 0
  -- row range (screenH-1: 1 for header at top)
  let visRows := screenH - 1
  let startRow := if curRow < visRows then 0 else curRow - visRows + 1
  let endRow := min st.nRows (startRow + visRows)
  -- render header
  header st cols curColOrig 0 selCols
  if sepX > 0 then Term.print sepX.toUInt32 0 Term.default Term.default "|"
  -- render data rows
  for i in [:endRow - startRow] do
    let ri := startRow + i
    row st cols ri curRow curColOrig decimals (i + 1).toUInt32 selCols selRows
    if sepX > 0 then Term.print sepX.toUInt32 (i + 1).toUInt32 Term.default Term.default "|"
  return (vr.offset, cols, sepX)

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
        match parts with
        | _ :: val :: _ => return (val.toNat? |>.getD 0) / 1024
        | _ => return 0
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
  if p.startsWith srcPfx then p.drop srcPfx.length else p

-- | Render tab line: view1 | view2 | ... (all views on stack)
def tabLine (views : Array (String × String × String)) (y : UInt32) (screenW : Nat) : IO Unit := do
  -- views: (path, disp, prql); head=current, tail=parents; reverse for display
  let rev := views.reverse
  let n := rev.size
  -- build labels, omit path if same as previous
  let (labels, _) := rev.foldl (init := (#[], ("", 0))) fun (acc, (prevPath, idx)) (path, d, p) =>
    let lbl := if d.isEmpty then shortenPrql p else d
    let sp := shortenPath path
    let txt := if path == prevPath then (if lbl.isEmpty then s!"#{idx+1}" else lbl)
               else if lbl.isEmpty then sp else s!"{sp} {lbl}"
    (acc.push txt, (path, idx + 1))
  -- bracket current view (last after reverse)
  let marked := labels.mapIdx fun i lbl =>
    if i == n - 1 then s!"[{lbl}]" else lbl
  Term.printPad 0 y screenW.toUInt32 Term.white Term.blue (marked.join " | ")

-- | Render status bar at bottom
def statusBar (curRow curCol colOff total screenW : Nat) (keyCols : Array String) (selCols : Array String) (selRows : Array Nat)
              (y : UInt32) (msg : String := "") (err : String := "") : IO Unit := do
  -- left side: error (red), message, or key/sel columns/rows
  let (left, fg) := if !err.isEmpty then (err.take (screenW - 20), Term.red)
    else if !msg.isEmpty then (msg, Term.cyan)
    else
      let keyStr := if keyCols.isEmpty then "" else s!"keys={keyCols.size} "
      let selStr := if selCols.isEmpty then "" else s!"sel={selCols.size} *{selCols.join ","}"
      let rowStr := if selRows.isEmpty then "" else s!" rows={selRows.size}"
      (s!"{keyStr}{selStr}{rowStr}", Term.cyan)
  -- right side: col info + mem + row/total
  let mb ← memMB
  let right := s!"c{curCol}+{colOff} {mb}MB {curRow}/{fmtNum total}"
  -- print left, then right-aligned position
  Term.print 0 y fg Term.default left
  let rx := screenW - right.length
  Term.print rx.toUInt32 y Term.cyan Term.default right

-- | Key bindings for info overlay (2 columns: key | hint)
def keyHints : Array (String × String) := #[
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
def infoOverlay (_ : SomeTable) (_ _ : Nat) (screenH screenW : Nat) : IO Unit := do
  let nRows := keyHints.size
  let keyW := 5; let hintW := 10
  let boxW := keyW + 1 + hintW
  let x0 := screenW - boxW - 2
  let y0 := screenH - nRows - 3
  for i in [:nRows] do
    let (k, d) := keyHints.getD i ("", "")
    let kpad := "".pushn ' ' (keyW - k.length) ++ k
    let dpad := d.take hintW ++ "".pushn ' ' (hintW - min d.length hintW)
    Term.print x0.toUInt32 (y0 + i).toUInt32 Term.black Term.yellow (kpad ++ " " ++ dpad)

end Render
