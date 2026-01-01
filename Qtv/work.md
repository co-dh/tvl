# Qtv - Q Table Viewer converted to Lean

Converted from `~/repo/qtv` (Q/kdb+) to Lean.

## File Mapping

| Q file | Lean file | Features |
|--------|-----------|----------|
| `curse.q` | (uses tvl's termbox) | curses bindings |
| `fun.q` | `Types.lean`, `State.lean`, `Op.lean` | GL, st stack, del/cp/ren/xasc/xdesc/freq/meta/flt/agg |
| `te.q` | `Nav.lean`, `Render.lean`, `Key.lean` | C1/down/R/R0, align/rend/rend1/sb, key bindings |

## Key Mappings from Q

### State (fun.q → State.lean)
- `st` → `State.st` (view stack)
- `GL` → `State.gl` (r0, cr, cc, kc, typ, t)
- `reg` → `State.reg` (search registry)

### Stack Operations (fun.q → State.lean)
- `push` → `State.push`
- `q` → `State.pop`
- `D` → `State.dup`
- `S` → `State.swap`

### Table Operations (fun.q → Op.lean)
- `del` → `Tbl.delCol`
- `cp` → `Tbl.cpCol`
- `ren` → `Tbl.renCol`
- `xasc` → `Tbl.xasc`
- `xdesc` → `Tbl.xdesc`
- `F` (freq) → `Tbl.freq`
- `M` (meta) → `Tbl.toMeta`
- `flt` → `Tbl.fltLike`, `Tbl.fltEq`
- `agg` → `Tbl.aggCount`

### Key Bindings (te.q → Key.lean)
- `d` → `opDel`
- `c` → `opCp`
- `[` → `opAsc`
- `]` → `opDesc`
- `!` → `opBang` (toggle key column)
- `F` → `opFreq`
- `M` → `opMeta`
- `\` → `opFlt`
- `/` → search input
- `n/N` → `searchFwd/searchBwd`
- `*` → `searchCur`

### Navigation (te.q → Nav.lean)
- `C1` → `State.C1` (column cursor)
- `down` → `State.down` (row cursor with scroll)
- `R` → `State.R` (set row)
- `R0` → `State.R0` (set first visible row)
- `g/G` → `navTop/navBot`
- `^D/^U` → `navPgDn/navPgUp`

### Rendering (te.q → Render.lean)
- `align` → `align`
- `rend` → `State.render`
- `rend1` → `State.rend1`
- `sb` → `State.statusBar`

## Build

```bash
lake build Qtv
```
