/-
  Viewport: cursor + offset for scroll state.
-/

structure Viewport where
  cursor : Nat
  offset : Nat
  deriving Repr

namespace Viewport

-- | Create at position 0
def create : Viewport := ⟨0, 0⟩

-- | Jump to position with bounds
def goto (pos maxIdx : Nat) : Viewport :=
  let c := min pos (if maxIdx = 0 then 0 else maxIdx - 1)
  ⟨c, c⟩

end Viewport
