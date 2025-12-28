/-
  Render specifications and theorems (not compiled into main binary)
  These theorems document expected behavior of visibility/rendering logic
  that is now implemented in C (adbc_shim.c lean_render_table)
-/
import Tv.Render

open Render

/-! ## Key Column Ordering -/

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

-- | Theorem: displayOrder always has key columns first
theorem displayOrder_keysFirst (keyCols : Array Nat) (nCols : Nat) :
    keyColsFirst keyCols (displayOrder keyCols nCols) = true := by
  simp [displayOrder, keyColsFirst]

/-! ## Column Visibility -/

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

/-! ## Cursor Visibility -/

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

/-! ## Column Width Preservation -/

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

/-! ## Key Column Adjustment -/

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

/-! ## Row Visibility -/

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
