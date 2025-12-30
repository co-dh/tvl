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
│  │  cur, off, sels      │  │  cur, off, sels, group   │ │
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

| Class     | Purpose                                      |
|-----------|----------------------------------------------|
| OrdSetOps | Ordered set ops: add, remove, toggle, invert |
| Cursor    | Navigation ops: get, set, move, search       |

## Ops

|   | . | = | + | - | ? | ^ | 0 | ~ |
|---|---|---|---|---|---|---|---|---|
| R | ✓ | ✓ | ✓ |   | ✓ |   |   |   |
| C | ✓ | ✓ | ✓ |   | ✓ |   |   |   |
| W |   |   | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| X |   |   | ✓ | ✓ | ✓ | ✓ | ✓ | ✓ |
| G |   |   | ✓ | ✓ | ✓ | ✓ | ✓ |   |

R=row.cur, C=col.cur, W=row.sels, X=col.sels, G=col.group

| Op | Cursor | OrdSet |
|----|--------|--------|
| .  | get    |        |
| =  | set    |        |
| +  | move   | add    |
| -  |        | del    |
| ?  | find   | mem    |
| ^  |        | toggle |
| 0  |        | clear  |
| ~  |        | invert |

## Structures

| Struct   | Purpose                                       |
|----------|-----------------------------------------------|
| Table    | Query result dimensions (nRows, colNames)     |
| OrdSet   | Ordered set with invert flag                  |
| DispIdx  | Type-safe display index (not raw Nat)         |
| RowNav   | Row cursor + offset + selections              |
| ColNav   | Column cursor + offset + selections + group   |
| NavState | Composes RowNav + ColNav                      |

## Key Design Decisions

**DispIdx**: Type-safe wrapper prevents mixing display index with raw Nat.

**Display order**: Group columns first, then rest. `ColNav.dispOrder` computes this.

**Invert flag**: `OrdSet.inv` avoids materializing large inverted selections.

**Cursor typeclass**: Same interface for row/col navigation with different element types.
