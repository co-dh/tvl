# tv - Terminal Tabular Viewer

A fast, keyboard-driven tabular data viewer built in Lean 4.

## Dependencies

### Build Dependencies

| Dependency | Description | Install |
|------------|-------------|---------|
| [Lean 4](https://lean-lang.org/) | Programming language | `elan` toolchain manager |
| [Lake](https://github.com/leanprover/lake) | Lean build system | Included with Lean |
| [termbox2](https://github.com/termbox/termbox2) | TUI library | `make && sudo make install` |

### Runtime Dependencies

| Dependency | Description | Install |
|------------|-------------|---------|
| [DuckDB](https://duckdb.org/) | Embedded database (via ADBC) | System package or binary |
| [ADBC](https://arrow.apache.org/adbc/) | Arrow Database Connectivity | DuckDB driver required |
| [prqlc](https://prql-lang.org/) | PRQL to SQL compiler | `cargo install prqlc` or binary |
| [fzf](https://github.com/junegunn/fzf) | Fuzzy finder for selection | System package |
| [bat](https://github.com/sharkdp/bat) | File preview with syntax highlighting | System package |

### Optional Dependencies

| Dependency | Description | Install |
|------------|-------------|---------|
| [shellfs](https://github.com/Query-farm/duckdb-shellfs-extension) | DuckDB extension for shell commands | `duckdb -c "INSTALL shellfs"` |

### System Commands (for source views)

- `ls` - directory listing
- `find` - recursive file search
- `ps` - process listing
- `df` - disk usage
- `env` - environment variables
