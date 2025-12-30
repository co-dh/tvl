# Typeclass Refactoring Design

## Diagram

```
┌─────────────────────────────────────────────────────────┐
│                        Table                            │
│  (struct: nRows, colNames)                              │
└─────────────────────┬───────────────────────────────────┘
                      │ parameterizes
                      ▼
┌─────────────────────────────────────────────────────────┐
│                      NavState t                         │
│  ┌──────────────────────┐  ┌──────────────────────────┐ │
│  │       RowNav         │  │        ColNav            │ │
│  │  cur, off, sels      │  │  cur, off, sels, keys    │ │
│  │         │            │  │         │                │ │
│  │    Cursor class      │  │    Cursor class          │ │
│  └─────────┬────────────┘  └─────────┬────────────────┘ │
│            │                         │                  │
│            └────────┬────────────────┘                  │
│                     ▼                                   │
│              OrdSet (struct)                            │
│            OrdSetOps (class)                            │
└─────────────────────────────────────────────────────────┘
```

## Classes

```lean
-- OrdSetOps: ordered set with invert flag
-- Avoids materializing large inverted selections
class OrdSetOps (α : Type) (β : outParam Type) where
  empty  : β
  add    : Array α → β → β
  remove : Array α → β → β
  toggle : α → β → β
  clear  : β → β
  invert : β → β           -- flip flag, don't materialize
  mem    : α → β → Bool

-- Cursor: navigation operations
-- Bounded movement with search
class Cursor (α : Type) (bound : Nat) (elem : Type) where
  get    : α → Nat
  set    : Nat → α → α
  move   : Int → α → α
  search : (elem → Bool) → α → α
```

## Structures

```lean
-- Table: query result dimensions from Source
structure Table where
  nRows : Nat
  colNames : Array String

-- OrdSet: ordered set implementation
-- inv=true means "all except arr"
structure OrdSet (α : Type) [BEq α] where
  arr : Array α := #[]
  inv : Bool := false

-- RowNav: row navigation state
-- Implements Cursor for vertical movement
structure RowNav where
  cur  : Nat          -- cursor position
  off  : Nat          -- scroll offset
  sels : OrdSet Nat   -- selected row indices

-- DispIdx: type-safe display index
-- Prevents mixing with raw Nat
structure DispIdx where
  val : Nat

-- ColNav: column navigation state
-- Implements Cursor for horizontal movement
-- Display order = keys first, then rest
structure ColNav where
  cur  : DispIdx         -- cursor in display order
  off  : DispIdx         -- scroll offset in display order
  sels : OrdSet String   -- selected column names
  keys : OrdSet String   -- key columns (displayed first)

def ColNav.dispOrder (c : ColNav) (colNames : Array String) : Array String
def ColNav.colAt (c : ColNav) (colNames : Array String) (i : DispIdx) : Option String

-- NavState: full navigation state
-- Composes row and column navigation
structure NavState (t : Table) where
  row : RowNav
  col : ColNav
```

## Why ColName, not ColIdx?

Display order changes when key columns move first:
```
Data order:    [a, b, c, d]
Key "c":       [c | a, b, d]
```
Name is stable. Index is not.
