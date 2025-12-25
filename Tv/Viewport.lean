/-
  Viewport: cursor + offset for scroll state.
  Offset tracks which column is at left edge.
-/

structure Viewport where
  cursor : Nat
  offset : Nat  -- left edge column
  deriving Repr

namespace Viewport

-- | Create at position 0
def create : Viewport := ⟨0, 0⟩

-- | Move cursor right with bounds check
def moveRight (v : Viewport) (maxIdx : Nat) : Viewport :=
  if v.cursor ≥ maxIdx - 1 then v else ⟨v.cursor + 1, v.offset⟩

-- | Move cursor left
def moveLeft (v : Viewport) : Viewport :=
  if v.cursor = 0 then v else ⟨v.cursor - 1, v.offset⟩

-- | Jump to position with bounds
def goto (pos maxIdx : Nat) : Viewport :=
  let c := min pos (if maxIdx = 0 then 0 else maxIdx - 1)
  ⟨c, c⟩  -- reset offset to cursor

-- | Go to start
def goTop : Viewport := ⟨0, 0⟩

-- | Go to end
def goEnd (maxIdx : Nat) : Viewport :=
  if maxIdx = 0 then ⟨0, 0⟩ else ⟨maxIdx - 1, maxIdx - 1⟩

-- | Page down by n
def pageDown (v : Viewport) (n maxIdx : Nat) : Viewport :=
  let c := min (v.cursor + n) (if maxIdx = 0 then 0 else maxIdx - 1)
  ⟨c, v.offset⟩

-- | Page up by n
def pageUp (v : Viewport) (n : Nat) : Viewport :=
  ⟨v.cursor - min v.cursor n, v.offset⟩

-- | Adjust offset based on visible range (called after computing visible cols)
def adjustOffset (v : Viewport) (firstVisible lastVisible : Nat) : Viewport :=
  if v.cursor > lastVisible then ⟨v.cursor, v.cursor⟩  -- scroll right
  else if v.cursor < firstVisible then ⟨v.cursor, v.cursor⟩  -- scroll left
  else v  -- cursor visible, keep offset

-- | Theorem: right key moves cursor to next column (when not at end)
theorem moveRight_inc (v : Viewport) (maxIdx : Nat) (h : v.cursor + 1 < maxIdx) :
    (v.moveRight maxIdx).cursor = v.cursor + 1 := by
  unfold moveRight
  have : ¬(v.cursor ≥ maxIdx - 1) := by omega
  simp [this]

-- | Theorem: right key stays at end
theorem moveRight_end (v : Viewport) (maxIdx : Nat) (h : v.cursor + 1 ≥ maxIdx) :
    (v.moveRight maxIdx).cursor = v.cursor := by
  unfold moveRight
  have : v.cursor ≥ maxIdx - 1 := by omega
  simp [this]

-- | Theorem: left key decrements cursor (when not at start)
theorem moveLeft_dec (v : Viewport) (h : v.cursor > 0) :
    (v.moveLeft).cursor = v.cursor - 1 := by
  unfold moveLeft
  simp [Nat.ne_of_gt h]

-- | Theorem: left key stays at start
theorem moveLeft_start (v : Viewport) (h : v.cursor = 0) :
    (v.moveLeft).cursor = 0 := by
  unfold moveLeft
  simp [h]

-- | Theorem: pageDown cursor bounded by maxIdx - 1
theorem pageDown_bound (v : Viewport) (n maxIdx : Nat) (h : maxIdx > 0) :
    (v.pageDown n maxIdx).cursor < maxIdx := by
  unfold pageDown
  simp only [Nat.min_def]
  split <;> (split <;> omega)

-- | Theorem: pageUp cursor bounded (never exceeds original)
theorem pageUp_bound (v : Viewport) (n : Nat) :
    (v.pageUp n).cursor ≤ v.cursor := by
  unfold pageUp
  exact Nat.sub_le v.cursor (min v.cursor n)

-- | Theorem: goto cursor bounded by maxIdx
theorem goto_bound (pos maxIdx : Nat) (h : maxIdx > 0) :
    (goto pos maxIdx).cursor < maxIdx := by
  unfold goto
  simp only [Nat.min_def]
  split <;> (split <;> omega)

end Viewport
