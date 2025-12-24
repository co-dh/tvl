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

end Viewport
