# tv - Terminal Tabular Viewer

A fast, keyboard-driven tabular data viewer built in Lean 4.

## Features

- View CSV, Parquet, JSON files
- Vim-style navigation (`hjkl`, `g`/`G`, `Ctrl-d`/`Ctrl-u`)
- Sort (`[`/`]`), filter (`\`), select columns (`s`)
- Frequency/group-by view (`F`)
- Column metadata view (`M`)
- Key columns for grouping (`!`)
- Aggregation (`b`)
- System views: `ps`, `env`, `df`, directory listing
- PRQL-powered queries

## Quick Start

```bash
tv data.csv                    # view CSV file
tv data.parquet                # view Parquet file
tv :ps                         # view process list
tv :lr                         # browse current directory
```

## Navigation

| Key | Action |
|-----|--------|
| `j` / `↓` | Move down |
| `k` / `↑` | Move up |
| `l` / `→` | Move right |
| `h` / `←` | Move left |
| `g` | Go to first row |
| `G` | Go to last row |
| `0` | Go to first column |
| `$` | Go to last column |
| `Ctrl-d` | Page down |
| `Ctrl-u` | Page up |
| `@` | Jump to column (fzf) |

## Views

| Key | Action |
|-----|--------|
| `F` | Frequency view (group by key + cursor column) |
| `M` | Metadata view (all columns) |
| `m` | Quick stats (count, dist, min, max for selected columns) |
| `r` | List directory (recursive) |
| `R` | List directory (non-recursive) |
| `T` | Duplicate current view |
| `S` | Swap top two views |
| `q` | Close view / quit |
| `Enter` | Drill down (context-dependent) |

## Data Operations

| Key | Action |
|-----|--------|
| `[` | Sort ascending by current column |
| `]` | Sort descending by current column |
| `\` | Filter (fzf select value or PRQL expression) |
| `s` | Select columns (fzf multi-select) |
| `D` | Delete selected/current column(s) |
| `!` | Toggle key column (shown first with `|` separator) |
| `b` | Aggregate (requires key columns set with `!`) |
| `^` | Rename current column |

## Selection

| Key | Action |
|-----|--------|
| `Space` | Toggle column selection |
| `Esc` | Clear selection |

## Display

| Key | Action |
|-----|--------|
| `.` | Increase decimal places |
| `,` | Decrease decimal places |
| `I` | Toggle info overlay |

## Other

| Key | Action |
|-----|--------|
| `:` | Command mode (ps, env, df, ls, tcp) |
| `L` | Load file (fzf) |

## Filter Syntax

When pressing `\`, you can:
- Select a value from fzf list → filters to `col == 'value'`
- Type custom PRQL: `> 5`, `< 10`, `~= 'pattern'`

## Key Columns

Mark columns as "key" with `!`. Key columns:
- Display first (left side with `|` separator)
- Used for grouping in frequency view (`F`)
- Used for aggregation (`b`)

## Views Explained

### Table View (default)
Standard tabular display with sorting, filtering, selection.

### Frequency View (`F`)
Groups by key columns + cursor column, shows count and percentage bar.
Press `Enter` to filter original data to selected group.

### Metadata View (`M`)
Shows column statistics: type, count, distinct, null%, min, max.
- `0` selects columns with nulls
- `1` selects columns with single value
- `Enter` on row sets that column as key

### Folder View (`r`/`R`)
Browse filesystem. `Enter` on directory to navigate, on file to preview with `bat`.

## Dependencies

### Build

| Dependency | Install |
|------------|---------|
| [Lean 4](https://lean-lang.org/) | `elan` toolchain manager |
| [termbox2](https://github.com/termbox/termbox2) | `make && sudo make install` |

### Runtime

| Dependency | Install |
|------------|---------|
| [DuckDB](https://duckdb.org/) | System package |
| [ADBC](https://arrow.apache.org/adbc/) | DuckDB driver |
| [prqlc](https://prql-lang.org/) | `cargo install prqlc` |
| [fzf](https://github.com/junegunn/fzf) | System package |
| [bat](https://github.com/sharkdp/bat) | System package (optional, for file preview) |

## Build

```bash
lake build
```

## License

MIT
