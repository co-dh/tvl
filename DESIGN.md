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
│  loop: poll → Key.handle → fetch → Render.table      │
└──────┬───────────────┬──────────────────────────────┘
       │               │
       ▼               ▼
┌─────────────┐ ┌─────────────────────────────────────┐
│  Key.lean   │ │            State.lean                │
│  nav: hjkl  │ │  State { views, inputMode, err }     │
│  view: MDF  │ │  View  { path, query, nav, cache }   │
│  src: r,R,: │ │  Nav   { rowCur, colCur, keyCols }   │
└──────┬──────┘ └─────────────────────────────────────┘
       │
       ├──────────────────┐
       ▼                  ▼
┌─────────────┐   ┌─────────────┐
│ Source.lean │   │  Meta.lean  │
│ ps,df,env   │   │ column info │
│ ls,lr (find)│   │ 0,1 selects │
│ temp tables │   │ ret→keyCols │
└──────┬──────┘   └─────────────┘
       │
       ▼
┌─────────────────────────────────────────────────────┐
│                   Backend.lean                       │
│  Prql.lean → prqlc CLI → Adbc.lean (DuckDB FFI)     │
│  Query { base, ops }  Error.lean (log + status bar) │
└─────────────────────────────────────────────────────┘
       │
       ▼
┌─────────────────────────────────────────────────────┐
│                   Render.lean                        │
│  displayOrder: keyCols first, then rest             │
│  visibleRange: offset + screen height               │
└──────┬──────────────────────────────────────────────┘
       │
       ▼
┌─────────────┐  ┌─────────────┐  ┌─────────────┐
│  Term.lean  │  │ Types.lean  │  │ Fzf.lean    │
│  termbox2   │  │ SomeTable   │  │ picker/bat  │
│  FFI shim   │  │ Cell,Query  │  │ fzf/fzfMulti│
└─────────────┘  └─────────────┘  └─────────────┘
       │
       ▼
┌─────────────────────────────────────────────────────┐
│                   c/ (FFI shims)                     │
│  term_shim.c  - termbox2 bindings                   │
│  adbc_shim.c  - DuckDB ADBC, Arrow data access      │
│                 handles: int, float, str, bool,     │
│                 timestamp, time, decimal, utf8      │
└─────────────────────────────────────────────────────┘
```

### Data Flow

1. **File/Source → Query**: Path or source command creates initial Query
2. **Query → PRQL → SQL**: Prql.lean builds PRQL, prqlc compiles to SQL
3. **SQL → DuckDB → Arrow**: Adbc executes SQL, returns Arrow batches
4. **Arrow → SomeTable**: Zero-copy access via FFI (no data duplication)
5. **SomeTable → Render**: Display with keyCols first, cursor tracking

### Error Handling

All query functions return `Option` instead of `Except`:
- `Prql.compile` → `IO (Option String)` - logs error via `Error.set`
- `Backend.query` → `IO (Option SomeTable)` - early return on None
- `Backend.queryRow/queryDistinct` → `IO (Option ...)` - same pattern

Pattern: `let some x ← ioOption | return default`

Errors logged to `/tmp/tv.log` and shown on status bar (red).

### Source Types

| Source | Command | Format |
|--------|---------|--------|
| `ps`   | `ps aux` | 11 cols, TIME→HH:MM:SS |
| `df`   | `df -h`  | 6 cols with header |
| `env`  | `env`    | name, value pairs |
| `ls`   | `find -maxdepth 1` | 7 cols (perms→path) |
| `lr`   | `find` (recursive) | 7 cols (perms→path) |

Sources create DuckDB temp tables for efficient re-query.

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
├── Test.lean          # key tests (--keys mode)
├── Tv/
│   ├── Types.lean     # Cell, SomeTable, Query, PureKey
│   ├── State.lean     # View, ViewKind, Nav, State
│   ├── Error.lean     # centralized error: log to file, show on status bar
│   ├── Term.lean      # termbox2 FFI bindings
│   ├── Adbc.lean      # DuckDB ADBC FFI bindings
│   ├── Backend.lean   # PRQL compile, query exec (returns Option)
│   ├── Prql.lean      # type-safe PRQL builder (returns Option)
│   ├── Source.lean    # system sources (ps,df,env,ls,lr)
│   ├── Meta.lean      # meta view logic
│   ├── Freq.lean      # freq view logic
│   ├── Render.lean    # displayOrder, visibleRange, render
│   ├── Key.lean       # all key handlers (nav,view,agg)
│   ├── Fzf.lean       # fzf picker, bat viewer
│   └── App.lean       # event loop, input modes
└── c/
    ├── term_shim.c    # termbox2 C shim
    └── adbc_shim.c    # DuckDB ADBC C shim (Arrow types)
```

## Build

```bash
lake build
```

## Implementation Status

1. [x] Project setup (lakefile.lean)
2. [x] termbox2 FFI bindings
3. [x] DuckDB ADBC FFI bindings (Arrow zero-copy)
4. [x] PRQL type-safe builder
5. [x] SomeTable (zero-copy Arrow access)
6. [x] Table rendering with displayOrder
7. [x] Key columns (M view, select, return)
8. [x] Meta view (column stats, 0/1 select)
9. [x] System sources (ps, df, env, ls, lr)
10. [x] Freq view (group by key cols)
11. [x] Aggregate view (sum, avg, min, max)
12. [x] Column operations (D, ^, s)
13. [x] Sort ([ asc, ] desc)
14. [x] Filter (\ expr)
15. [x] Test suite (--keys mode)
16. [ ] Visibility theorem proofs (sorry)

## Key Differences from Haskell

| Aspect | Haskell + LH | Lean 4 |
|--------|--------------|--------|
| Proof syntax | `{-@ ... @-}` annotations | Part of type signature |
| Proof terms | Erased, checked by z3 | First-class, `by omega` |
| Vectors | `vector-sized` + singletons | Built-in `Vector` |
| TUI | brick (high-level) | termbox2 (low-level) |
| Ecosystem | Rich | Minimal |
