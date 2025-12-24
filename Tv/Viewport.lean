/-
  Viewport: just cursor position with bounds proof.
  Offset computed at render time from cursor + column widths + screen width.
-/

structure Viewport where
  cursor : Nat
  deriving Repr

namespace Viewport

-- | Create at position 0
def create : Viewport := ⟨0⟩

-- | Move cursor right with bounds check
def moveRight (v : Viewport) (maxIdx : Nat) : Viewport :=
  if v.cursor ≥ maxIdx - 1 then v else ⟨v.cursor + 1⟩

-- | Move cursor left
def moveLeft (v : Viewport) : Viewport :=
  if v.cursor = 0 then v else ⟨v.cursor - 1⟩

-- | Jump to position with bounds
def goto (pos maxIdx : Nat) : Viewport :=
  ⟨min pos (if maxIdx = 0 then 0 else maxIdx - 1)⟩

-- | Go to start
def goTop : Viewport := ⟨0⟩

-- | Go to end
def goEnd (maxIdx : Nat) : Viewport :=
  if maxIdx = 0 then ⟨0⟩ else ⟨maxIdx - 1⟩

-- | Page down by n
def pageDown (v : Viewport) (n maxIdx : Nat) : Viewport :=
  ⟨min (v.cursor + n) (if maxIdx = 0 then 0 else maxIdx - 1)⟩

-- | Page up by n
def pageUp (v : Viewport) (n : Nat) : Viewport :=
  ⟨v.cursor - min v.cursor n⟩

end Viewport
