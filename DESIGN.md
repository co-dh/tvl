# tv-lean: Tabular Viewer in Lean 4

## Goal

Reimplement tv (CSV browser) in Lean 4 with:
- Dependent types for cursor visibility (not refinement types)
- FFI to termbox2 for TUI
- FFI to DuckDB for queries (future)

## Architecture

```
┌─────────────────────────────────────────────────────┐
│                     Main.lean                        │
│                   init → loop → shutdown             │
└──────────────────────┬──────────────────────────────┘
                       │
┌──────────────────────▼──────────────────────────────┐
│                     App.lean                         │
│  ┌───────────────────────────────────────────────┐  │
│  │ AppState                                       │  │
│  │  ├── table : Table n m  (n rows, m cols)      │  │
│  │  ├── rowVP : Viewport   (cursor visible)      │  │
│  │  ├── colVP : Viewport   (cursor visible)      │  │
│  │  └── msg : String                             │  │
│  └───────────────────────────────────────────────┘  │
│                                                      │
│  loop: poll → handle → render                        │
└──────┬─────────────────────┬────────────────────────┘
       │                     │
       ▼                     ▼
┌─────────────┐       ┌─────────────┐
│  Term.lean  │       │ Table.lean  │
│  (termbox2) │       │ (data)      │
└─────────────┘       └─────────────┘
```

## Core Types

### Viewport (cursor always visible)

```lean
structure Viewport where
  cursor : Nat
  offset : Nat
  size   : Nat
  hpos   : size > 0
  hvis   : offset ≤ cursor ∧ cursor < offset + size
  deriving Repr

def mkViewport (sz : Nat) (h : sz > 0) : Viewport :=
  ⟨0, 0, sz, h, ⟨Nat.zero_le 0, h⟩⟩

def Viewport.moveRight (v : Viewport) : Viewport :=
  if h : v.cursor + 1 < v.offset + v.size then
    { v with cursor := v.cursor + 1, hvis := ⟨v.hvis.1, h⟩ }
  else
    { v with
      cursor := v.cursor + 1,
      offset := v.offset + 1,
      hvis := ⟨Nat.le_succ_of_le v.hvis.1, by omega⟩ }

def Viewport.moveLeft (v : Viewport) : Viewport :=
  if v.cursor = 0 then v
  else if h : v.cursor > v.offset then
    { v with cursor := v.cursor - 1, hvis := ⟨by omega, by omega⟩ }
  else
    { v with cursor := v.cursor - 1, offset := v.cursor - 1,
      hvis := ⟨Nat.le_refl _, by omega⟩ }
```

### Table (sized)

```lean
structure Table (rows cols : Nat) where
  headers : Vector String cols
  data    : Vector (Vector String cols) rows
  deriving Repr

def Table.delCol (t : Table r c) (idx : Fin c) (h : c > 1) : Table r (c - 1) :=
  { headers := t.headers.eraseIdx idx
  , data := t.data.map (·.eraseIdx idx) }
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
│   ├── Types.lean     # Cell, basic types
│   ├── Viewport.lean  # cursor visibility proof
│   ├── Table.lean     # sized table
│   ├── Term.lean      # termbox2 FFI
│   ├── Csv.lean       # CSV parser
│   ├── Render.lean    # table rendering
│   └── App.lean       # event loop, state
└── c/
    └── term_shim.c    # C FFI shim
```

## Build

```bash
lake build
```

## Implementation Order

1. [ ] Project setup (lakefile.lean)
2. [ ] Viewport with proofs
3. [ ] termbox2 FFI bindings
4. [ ] Basic render loop (hello world)
5. [ ] Table type (sized vectors)
6. [ ] CSV parser
7. [ ] Table rendering
8. [ ] Cursor movement with bounds
9. [ ] Delete column

## Key Differences from Haskell

| Aspect | Haskell + LH | Lean 4 |
|--------|--------------|--------|
| Proof syntax | `{-@ ... @-}` annotations | Part of type signature |
| Proof terms | Erased, checked by z3 | First-class, `by omega` |
| Vectors | `vector-sized` + singletons | Built-in `Vector` |
| TUI | brick (high-level) | termbox2 (low-level) |
| Ecosystem | Rich | Minimal |
