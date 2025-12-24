/-
  Viewport: cursor visibility proven at type level.
  Invariant: offset ≤ cursor < offset + size
-/

structure Viewport where
  cursor : Nat
  offset : Nat
  size   : Nat
  hpos   : size > 0
  hvis   : offset ≤ cursor ∧ cursor < offset + size
  deriving Repr

namespace Viewport

-- | Smart constructor: cursor at 0
def create (sz : Nat) (h : sz > 0 := by omega) : Viewport :=
  ⟨0, 0, sz, h, ⟨Nat.zero_le 0, by omega⟩⟩

-- | Move cursor right, scroll if needed
def moveRight (v : Viewport) : Viewport :=
  if h : v.cursor + 1 < v.offset + v.size then
    { v with
      cursor := v.cursor + 1
      hvis := ⟨Nat.le_trans v.hvis.1 (Nat.le_succ _), h⟩ }
  else
    { v with
      cursor := v.cursor + 1
      offset := v.offset + 1
      hvis := ⟨Nat.succ_le_succ v.hvis.1, by
        have := v.hvis.2
        omega⟩ }

-- | Move cursor left, scroll if needed
def moveLeft (v : Viewport) : Viewport :=
  if hzero : v.cursor = 0 then v
  else if h : v.cursor > v.offset then
    { v with
      cursor := v.cursor - 1
      hvis := ⟨by omega, by
        have := v.hvis.2
        have := v.hpos
        omega⟩ }
  else
    -- cursor = offset, scroll left
    { v with
      cursor := v.cursor - 1
      offset := v.cursor - 1
      hvis := ⟨Nat.le_refl _, by
        have := v.hpos
        omega⟩ }

-- | Resize viewport, adjust offset to keep cursor visible
def resize (v : Viewport) (newSz : Nat) (h : newSz > 0 := by omega) : Viewport :=
  if hvis : v.cursor < v.offset + newSz then
    { v with size := newSz, hpos := h, hvis := ⟨v.hvis.1, hvis⟩ }
  else
    let newOff := v.cursor - newSz + 1
    { cursor := v.cursor
      offset := newOff
      size := newSz
      hpos := h
      hvis := ⟨by omega, by omega⟩ }

-- | Move right with upper bound check (for table bounds)
def moveRightBounded (v : Viewport) (maxIdx : Nat) : Viewport :=
  if v.cursor ≥ maxIdx - 1 then v
  else v.moveRight

-- | Move left (already handles 0 bound)
def moveLeftBounded (v : Viewport) : Viewport := v.moveLeft

-- | Page down (move cursor by n, with bounds check)
def pageDownN (v : Viewport) (n maxIdx : Nat) : Viewport :=
  if maxIdx = 0 then v
  else
    let rec go (v : Viewport) (i : Nat) : Viewport :=
      if i = 0 then v
      else if v.cursor ≥ maxIdx - 1 then v
      else go v.moveRight (i - 1)
    go v n

-- | Page up (move cursor by n)
def pageUpN (v : Viewport) (n : Nat) : Viewport :=
  let rec go (v : Viewport) (i : Nat) : Viewport :=
    if i = 0 then v
    else go v.moveLeft (i - 1)
  go v n

-- | Go to start
def goTop (v : Viewport) : Viewport :=
  create v.size v.hpos

-- | Go to end
def goEnd (v : Viewport) (maxIdx : Nat) : Viewport :=
  if maxIdx = 0 then v
  else pageDownN (goTop v) (maxIdx - 1) maxIdx

end Viewport
