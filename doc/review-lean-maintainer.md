# Code Review as Lean 4 Library Maintainer

## Strengths

1. **Good use of `native_decide`** for concrete test theorems - efficient for decidable propositions on small inputs

2. **Pure/IO separation** - `handleNav` is pure, `runNav` is the IO wrapper. Good for testability

3. **Invariant documentation** - comments like `INVARIANT: handleKey receives DisplayInfo, not Table` explain design intent

4. **Theorem-driven development** - visibility invariants have corresponding theorems, even if incomplete

## Issues

### 1. Incomplete proofs (`sorry`)

```lean
theorem cursorVisible_visibleRange ... := by sorry
theorem adjustOffset_colVisible ... := by sorry
```

Three `sorry` statements. Either prove them or mark as `axiom` with justification.

### 2. Missing `DecidableEq` instance for `Cell`

```lean
def eq : Cell → Cell → Bool  -- manual impl
instance : BEq Cell where beq := eq
```

Should derive `DecidableEq` instead of manual `BEq`. Enables `if c1 = c2` syntax and theorem automation.

### 3. Partial functions without bounds proofs

```lean
def get (t : Table) (r c : Nat) : Cell :=
  t.rows.getD r #[] |>.getD c .null
```

Using `getD` is safe but hides the partiality. Consider:

```lean
def get? (t : Table) (r c : Nat) : Option Cell
def get (t : Table) (r : Fin t.nRows) (c : Fin t.nCols) : Cell
```

### 4. `List` vs `Array` inconsistency

```lean
keyCols : List Nat  -- in PureState
colWidths : Array Nat  -- in DisplayInfo
```

`List` for keyCols causes O(n) `contains` checks. Use `Array` or `Std.HashSet` for performance.

### 5. Redundant `nCols` parameter

```lean
def displayOrder (keyCols : List Nat) (nCols : Nat) : List Nat
def nextInDisplay (keyCols : List Nat) (nCols : Nat) (cur : Nat) : Nat
```

`nCols` should be derived from context or passed as part of a structure. Easy to pass wrong value.

### 6. Missing termination proofs

```lean
def visColCount ... : Nat :=
  let rec go (i w : Nat) : Nat := ...  -- no termination_by
```

Add explicit `termination_by` for recursive functions.

### 7. Unsafe array access

```lean
parts[1]!.toNat?  -- in memMB
acc[i]!.2         -- in freq
```

Using `!` can panic. Prefer `getD` or pattern match on bounds.

### 8. Magic column indices

```lean
match tbl.get r 4 with  -- null% is col 4
match tbl.get r 3 with  -- dist is col 3
```

Hardcoded indices are fragile. Define constants or use a schema type.

## Suggestions

1. **Add `@[simp]` lemmas** for key definitions to help automation:

```lean
@[simp] theorem displayOrder_nil : displayOrder [] n = List.range n := by ...
```

2. **Use `Subtype` for bounded indices**:

```lean
structure ColIdx (n : Nat) where
  val : Nat
  isLt : val < n
```

3. **Consider `StateT` monad** for key handlers instead of threading `State` manually

4. **Add docstrings** with `/-- ... -/` syntax for public API

5. **Move theorems to separate file** (e.g., `Render/Theorems.lean`) for cleaner organization
