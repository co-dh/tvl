# tv-lean: Tabular Viewer in Lean 4

## Goal

Reimplement tv (CSV/Parquet browser) in Lean 4 with:
- Theorems for cursor visibility invariants
- FFI to termbox2 for TUI
- FFI to DuckDB via ADBC for queries
- PRQL for query composition

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                     Main.lean                        │
│                   init → loop → shutdown             │
└──────────────────────┬──────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────┐
│                     App.lean                         │
│  loop: poll → Key.handle → Render.table              │
└──────┬───────────────┬──────────────────────────────┘
       │               │
       ▼               ▼
┌─────────────┐ ┌─────────────────────────────────────┐
│  Key.lean   │ │            State.lean                │
│  handlers   │ │  ┌─────────────────────────────┐    │
│  h,j,k,l,   │ │  │ State                       │    │
│  M,D,F,...  │ │  │  ├── views : List View      │    │
└──────┬──────┘ │  │  ├── showInfo : Bool        │    │
       │        │  │  └── msg : String           │    │
       │        │  └─────────────────────────────┘    │
       │        │  ┌─────────────────────────────┐    │
       │        │  │ View                        │    │
       │        │  │  ├── path, prql, disp       │    │
       │        │  │  ├── rowVP, colVP : Viewport│    │
       │        │  │  ├── keyCols, selCols/Rows  │    │
       │        │  │  └── cache : Option Table   │    │
       │        │  └─────────────────────────────┘    │
       │        └─────────────────────────────────────┘
       ▼
┌─────────────────────────────────────────────────────┐
│                   Backend.lean                       │
│  ┌───────────┐  ┌───────────┐  ┌─────────────────┐  │
│  │ Prql.lean │→ │ prqlc CLI │→ │   Adbc.lean     │  │
│  │ type-safe │  │ PRQL→SQL  │  │ DuckDB via FFI  │  │
│  └───────────┘  └───────────┘  └─────────────────┘  │
│  + meta cache (.tv.meta files)                       │
└─────────────────────────────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────────────────────┐
│                   Render.lean                        │
│  displayOrder → visibleRange → header/row            │
│  keyCols first, then rest in original order          │
└──────┬──────────────────────────────────────────────┘
       │
       ▼
┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│  Term.lean  │  │ Types.lean  │  │ Fzf.lean    │
│  termbox2   │  │ Cell,Table  │  │ picker/bat  │
└─────────────┘  └─────────────┘  └─────────────┘
```

## Core Types

### Viewport (cursor + offset)

```lean
structure Viewport where
  cursor : Nat  -- current position
  offset : Nat  -- scroll offset
  deriving Repr

def Viewport.moveRight (v : Viewport) : Viewport :=
  ⟨v.cursor + 1, v.offset⟩

def Viewport.moveLeft (v : Viewport) : Viewport :=
  ⟨v.cursor - 1, v.offset⟩  -- saturating sub
```

### Table (with cached widths)

```lean
structure Table where
  cols   : Array Column
  rows   : Array (Array Cell)
  widths : Array Nat  -- cached column widths (max of header/data)
  deriving Repr

def Table.create (cols : Array Column) (rows : Array (Array Cell)) : Table :=
  ⟨cols, rows, calcWidths cols rows⟩
```

### Cell Types

```lean
inductive Cell where
  | null
  | int (v : Int)
  | float (v : Float)
  | str (v : String)
  | bool (v : Bool)
  deriving Repr
```

### Display Order (key columns first)

```lean
-- keyCols appear first, then rest in original order
def displayOrder (keyCols : List Nat) (nCols : Nat) : List Nat :=
  keyCols ++ (List.range nCols).filter (!keyCols.contains ·)

-- Adjust keyCols after column delete
def adjustKeyCols (keyCols : List Nat) (delCol : Nat) : List Nat :=
  keyCols.filterMap fun k =>
    if k == delCol then none
    else if k > delCol then some (k - 1)
    else some k
```

## FFI Bindings

### termbox2 (src/Term.lean)

```lean
@[extern "tb_init"]
opaque tbInit : IO Int32

@[extern "tb_shutdown"]
opaque tbShutdown : IO Unit

@[extern "tb_width"]
opaque tbWidth : IO Int32

@[extern "tb_height"]
opaque tbHeight : IO Int32

@[extern "tb_clear"]
opaque tbClear : IO Unit

@[extern "tb_present"]
opaque tbPresent : IO Unit

@[extern "tb_set_cell"]
opaque tbSetCell : Int32 → Int32 → UInt32 → UInt32 → UInt32 → IO Unit

@[extern "tb_peek_event"]
opaque tbPeekEvent : Int32 → IO (Option TermEvent)

structure TermEvent where
  type : UInt8
  mod  : UInt8
  key  : UInt16
  ch   : UInt32
  deriving Repr
```

### C shim (src/term_shim.c)

```c
#include <lean/lean.h>
#include <termbox2.h>

lean_obj_res lean_tb_init(lean_obj_arg world) {
    int r = tb_init();
    return lean_io_result_mk_ok(lean_box((uint32_t)r));
}
// ... other bindings
```

## File Structure

```
lean/
├── lakefile.lean      # build config
├── Main.lean          # entry point
├── Tv/
│   ├── Types.lean     # Cell, Column, Table with cached widths
│   ├── Viewport.lean  # cursor + offset scroll state
│   ├── State.lean     # View, ViewKind, State
│   ├── Term.lean      # termbox2 FFI bindings
│   ├── Adbc.lean      # DuckDB ADBC FFI bindings
│   ├── Backend.lean   # PRQL compile, query exec, meta cache
│   ├── Prql.lean      # type-safe PRQL builder
│   ├── Render.lean    # displayOrder, visibleRange, table render
│   ├── Key.lean       # all key handlers (h,j,k,l,M,D,F,...)
│   ├── Fzf.lean       # fzf picker, bat viewer integration
│   ├── Csv.lean       # simple CSV parser (fallback)
│   └── App.lean       # event loop
└── c/
    ├── term_shim.c    # termbox2 C shim
    └── adbc_shim.c    # DuckDB ADBC C shim
```

## Build

```bash
lake build
```

## Implementation Status

1. [x] Project setup (lakefile.lean)
2. [x] Viewport (cursor + offset)
3. [x] termbox2 FFI bindings
4. [x] DuckDB ADBC FFI bindings
5. [x] PRQL type-safe builder
6. [x] Table type with cached widths
7. [x] CSV parser (fallback)
8. [x] Table rendering with displayOrder
9. [x] Key columns (M view, select, return)
10. [x] Cursor visibility theorems
11. [x] Meta view cache (.tv.meta files)
12. [x] Column delete with keyCols adjustment
13. [ ] General visibility theorem proofs (sorry)

## Key Differences from Haskell

| Aspect | Haskell + LH | Lean 4 |
|--------|--------------|--------|
| Proof syntax | `{-@ ... @-}` annotations | Part of type signature |
| Proof terms | Erased, checked by z3 | First-class, `by omega` |
| Vectors | `vector-sized` + singletons | Built-in `Vector` |
| TUI | brick (high-level) | termbox2 (low-level) |
| Ecosystem | Rich | Minimal |
