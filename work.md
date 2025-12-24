# Lean TV Work Log

## 2024-12-24: FFI Struct Layout Fix

### Problem
Keyboard input not working - keys (hjkl, q, arrows, Ctrl+C) had no effect.

### Root Cause
Lean 4 scalar struct field memory layout was misunderstood. Fields are sorted by:
1. Size descending (UInt64 > UInt32 > UInt16 > UInt8)
2. Declaration order within same size

For `Term.Event`:
```lean
structure Event where
  type : UInt8   -- declared 1st
  mod  : UInt8   -- declared 2nd
  key  : UInt16
  ch   : UInt32  -- declared 1st UInt32
  w    : UInt32  -- declared 2nd UInt32
  h    : UInt32  -- declared 3rd UInt32
```

Memory layout (16 bytes total):
```
offset 0-3:   ch   (1st UInt32 by decl order)
offset 4-7:   w    (2nd UInt32)
offset 8-11:  h    (3rd UInt32)
offset 12-13: key  (UInt16)
offset 14:    type (1st UInt8 by decl order)
offset 15:    mod  (2nd UInt8)
```

### Fix in c/term_shim.c
```c
lean_object* obj = lean_alloc_ctor(0, 0, 16);
uint8_t* data = (uint8_t*)lean_ctor_scalar_cptr(obj);
*(uint32_t*)(data + 0) = ev.ch;
*(uint32_t*)(data + 4) = (uint32_t)ev.w;
*(uint32_t*)(data + 8) = (uint32_t)ev.h;
*(uint16_t*)(data + 12) = ev.key;
data[14] = ev.type;
data[15] = ev.mod;
```

### Debug technique
Added hex dump logging to C shim to see actual bytes written, compared with Lean-side field values.

### Other changes
- Added PageUp/Down, Home/End, g/G navigation
- Fixed horizontal scrolling (using colVP.offset)
- Added Viewport.pageDownN, pageUpN, goTop, goEnd with proofs
