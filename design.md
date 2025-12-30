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


## Object

| Symbol | Target        |
|--------|---------------|
| r      | row.cur       |
| c      | col.cur       |
| R      | row.sels      |
| C      | col.sels      |
| G      | col.group     |

## Verb

| Op | r        | c         | R          | C          | G              |
|----|----------|-----------|------------|------------|----------------|
| +  | down     | right     | select     | select     | add to group   |
| -  | up       | left      | deselect   | deselect   | remove         |
| <  | page up  | page left |            |            |                |
| >  | page dn  | page right|            |            |                |
| 0  | first    | first     | clear      | clear      | clear          |
| $  | last     | last      | all        | all        | all            |
| /  | find     | find      |            |            |                |
| ^  |          |           | toggle     | toggle     | toggle         |
| ~  |          |           | invert     | invert     |                |

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
