# Lean 4 TV Implementation Plan

## Rust TV Feature Summary (~/repo/t/tv)

### Architecture
```
┌─────────────────────────────────────────────────────────────────┐
│                         AppContext                               │
│  ├── backend: DuckDB + PRQL + LRU Cache                         │
│  ├── views: StateStack (Vec<ViewState>)                         │
│  ├── plugins: Registry (table, freq, meta, folder)              │
│  └── keymap: Kakoune-style bindings                             │
└─────────────────────────────────────────────────────────────────┘
```

### Core Features

**1. Data Backend**
- DuckDB via ADBC (Arrow Database Connectivity)
- PRQL compilation (prqlc crate)
- LRU query cache (100 entries)
- File formats: CSV, Parquet, JSON, CSV.gz
- Memory tables for intermediate results

**2. PRQL Transform Chain**
Every operation appends to PRQL string, lazy evaluation:
```
"from df"
  → "|filter price > 100"
  → "|sort {-quantity}"
  → "|select {name,price}"
```

Built-in PRQL functions:
- `freq c tbl` - GROUP BY + COUNT + Pct + Bar
- `meta c tbl` - column stats (cnt, dist, min, max)
- `stats c tbl` - n, min, max, avg, std
- `uniq c tbl` - distinct values
- `cnt tbl` - row count

**3. View Stack**
- Push: freq, meta, folder, dup
- Pop: q/ESC
- Swap: S
- Each view has: path, prql, cursor, offset, key_cols, sel_cols

**4. View Kinds**

```lean
inductive ViewKind where
  | table                  -- default data view
  | freq (col : Nat)       -- GROUP BY COUNT, enter filters parent
  | meta                   -- column stats (cnt, dist, min, max)
  | folder (path : String) -- directory listing, enter opens
```

Dispatch via pattern matching - no plugin abstraction.

**5. Keybindings (Kakoune-style)**

Navigation:
- `hjkl` / arrows - move cursor
- `g/G` - first/last row
- `Ctrl-d/u` - page down/up
- `@` - jump to column (fzf)

Selection:
- `space` - toggle selection
- `s` - select columns (fzf multi)
- `ESC` - clear selection

Transforms:
- `[/]` - sort asc/desc
- `\` - filter (fzf)
- `D` - delete column
- `^` - rename column
- `c` - derive (computed column)
- `b` - aggregate (GROUP BY)
- `!` - toggle key column

Views:
- `F` - frequency table
- `M` - metadata view
- `q` - pop/quit
- `T` - duplicate view
- `S` - swap views

**6. System Sources**
- `source:ls[:dir]` - directory listing
- `source:lr[:dir]` - recursive listing
- `source:ps` - processes
- `source:tcp/udp` - network connections
- `source:env` - environment vars
- `source:df` - disk usage
- `source:mounts` - mounted filesystems
- `source:pacman` - Arch packages

**7. File Operations**
- `L` - load file (fzf)
- Save to CSV/Parquet via DuckDB COPY

---

## Lean 4 Implementation Plan

### Phase 1: ADBC FFI Bindings

Use ADBC (Arrow Database Connectivity) - database-agnostic, returns Arrow data.
DuckDB has built-in ADBC support via `duckdb_adbc_init` entry point.

**ADBC C Structures:**
```c
struct AdbcError {
  char* message;
  int32_t vendor_code;
  char sqlstate[5];
  void (*release)(struct AdbcError*);
};

struct AdbcDatabase { void* private_data; struct AdbcDriver* private_driver; };
struct AdbcConnection { void* private_data; struct AdbcDriver* private_driver; };
struct AdbcStatement { void* private_data; struct AdbcDriver* private_driver; };

// AdbcDriver contains function pointers for all operations
```

**Create `c/adbc_shim.c`:**
```c
// Load driver dynamically from libduckdb.so
lean_adbc_load_driver(path)     // dlopen + duckdb_adbc_init

// Database lifecycle
lean_adbc_database_new()
lean_adbc_database_set_option(db, key, val)
lean_adbc_database_init(db)
lean_adbc_database_release(db)

// Connection lifecycle
lean_adbc_connection_new()
lean_adbc_connection_init(conn, db)
lean_adbc_connection_release(conn)

// Query execution
lean_adbc_statement_new(conn)
lean_adbc_statement_set_sql(stmt, sql)
lean_adbc_statement_execute(stmt)  // returns ArrowArrayStream
lean_adbc_statement_release(stmt)

// Arrow result access (ArrowArrayStream)
lean_arrow_stream_get_schema(stream)
lean_arrow_stream_get_next(stream)  // returns ArrowArray batch
lean_arrow_array_length(arr)
lean_arrow_array_is_null(arr, row)
lean_arrow_array_get_int64(arr, row)
lean_arrow_array_get_double(arr, row)
lean_arrow_array_get_string(arr, row)
```

**Arrow C Data Interface:**
```c
struct ArrowSchema {
  const char* format;      // type format string
  const char* name;        // field name
  int64_t n_children;
  struct ArrowSchema** children;
  void (*release)(struct ArrowSchema*);
};

struct ArrowArray {
  int64_t length;          // row count
  int64_t null_count;
  const void* buffers[3];  // validity, offsets, data
  int64_t n_children;
  struct ArrowArray** children;
  void (*release)(struct ArrowArray*);
};

struct ArrowArrayStream {
  int (*get_schema)(struct ArrowArrayStream*, struct ArrowSchema*);
  int (*get_next)(struct ArrowArrayStream*, struct ArrowArray*);
  void (*release)(struct ArrowArrayStream*);
};
```

### Phase 2: Backend Module

`Tv/Backend.lean`:
```lean
-- DuckDB connection (global state)
opaque DuckDB.Conn : Type

-- Query execution
def query (sql : String) : IO Table

-- PRQL compilation (subprocess)
def compilePrql (prql : String) : IO String

-- File loading
def loadFile (path : String) : IO Table
-- Supports: .csv, .parquet, .json, .csv.gz
```

### Phase 3: PRQL Support

`Tv/Prql.lean`:
```lean
-- PRQL function definitions (prepended to queries)
def prqlFuncs : String := "
let freq = func c tbl -> (from tbl | group {c} ...)
let meta = func c tbl -> (from tbl | aggregate {...})
"

-- Transform builders
def appendFilter (prql : String) (expr : String) : String
def appendSort (prql : String) (col : String) (desc : Bool) : String
def appendSelect (prql : String) (cols : List String) : String
def appendDerive (prql : String) (expr : String) : String
def appendAgg (prql : String) (keys cols : List String) : String
```

### Phase 4: Enhanced State

`Tv/State.lean`:
```lean
structure ViewState where
  id       : Nat
  name     : String           -- "freq col" | "meta" | "folder:/path"
  prql     : String           -- query chain
  path     : Option String    -- source path
  table    : Table            -- cached data
  rowVP    : Viewport
  colVP    : Viewport
  keyCols  : List Nat         -- pinned columns
  selCols  : HashSet Nat      -- selected columns
  parent   : Option Nat       -- parent view ID

structure AppState where
  views    : Array ViewState  -- stack
  msg      : String           -- status message
  mode     : Mode             -- Normal | Input | Confirm
```

### Phase 5: Command System

`Tv/Command.lean`:
```lean
inductive Cmd where
  -- Navigation
  | up | down | left | right
  | pageUp | pageDown | home | end
  | gotoCol (name : String)
  -- Selection
  | toggleSel | clearSel | selAll
  -- Transforms
  | filter (expr : String)
  | sort (col : Nat) (desc : Bool)
  | select (cols : List Nat)
  | derive (expr : String)
  | delCol (col : Nat)
  | rename (col : Nat) (name : String)
  | xkey (col : Nat)
  | agg (keys : List Nat) (funcs : List (String × Nat))
  -- Views
  | freq | meta | folder
  | pop | dup | swap
  -- I/O
  | load (path : String)
  | save (path : String)
  | quit

def exec (cmd : Cmd) (s : AppState) : IO AppState
```

### Phase 6: View Kind Dispatch

Handle view-specific behavior via pattern matching in `Tv/App.lean`:
```lean
-- View-specific enter behavior
def onEnter : ViewKind → State → State
  | .freq col, s => filterParent col s
  | .folder p, s => openOrDescend p s
  | .meta, s     => applyXkey s
  | .table, s    => s

-- View-specific key overrides
def viewKey : ViewKind → Key → State → Option Cmd
  | .freq _, Key.num 0, _ => some .selNull
  | .folder _, Key.bs, _  => some .parentDir
  | _, _, _               => none
```

### Phase 7: System Sources (optional)

`Tv/Source.lean`:
```lean
-- Generate SQL for system data
def sourceSQL : String → IO String
  | "ls" => lsSQL "."
  | s    => if s.startsWith "ls:" then lsSQL (s.drop 3) else ...

def lsSQL (dir : String) : IO String := do
  let entries ← System.FilePath.readDir dir
  -- Generate INSERT statements
```

### Phase 8: Keymap

`Tv/Keymap.lean`:
```lean
-- Kakoune-style key notation
def parseKey (s : String) : Key

-- Tab hierarchy: plugin → table → common
def lookupCmd (tab : String) (key : Key) : Option Cmd

-- Load from CSV config
def loadKeymap (path : String) : IO Keymap
```

---

## File Structure

```
~/repo/hsk/lean/
├── lakefile.lean
├── Main.lean
├── Tv/
│   ├── Types.lean      -- Cell, Column, Table, ViewKind (existing)
│   ├── Adbc.lean       -- ADBC FFI declarations (NEW)
│   ├── Arrow.lean      -- Arrow data access (NEW)
│   ├── Backend.lean    -- Query exec + cache (NEW)
│   ├── Prql.lean       -- PRQL builders (NEW)
│   ├── State.lean      -- ViewState, AppState (NEW)
│   ├── Command.lean    -- Command ADT + exec (NEW)
│   ├── Source.lean     -- System sources (NEW)
│   ├── Keymap.lean     -- Key bindings (NEW)
│   ├── Viewport.lean   -- (existing)
│   ├── Term.lean       -- termbox FFI (existing)
│   ├── Render.lean     -- (existing, enhance)
│   ├── Csv.lean        -- (existing, deprecate)
│   └── App.lean        -- ViewKind dispatch (existing, refactor)
├── c/
│   ├── term_shim.c     -- termbox FFI (existing)
│   └── adbc_shim.c     -- ADBC + Arrow FFI (NEW)
└── cfg/
    └── keys.csv        -- keymap config
```

---

## Implementation Order

| Step | Files           | Description                         |
|------|-------          |-------------                        |
| 1    | c/adbc_shim.c   | ADBC + Arrow C FFI bindings         |
| 2    | Tv/Adbc.lean    | ADBC Lean interface                 |
| 3    | Tv/Backend.lean | Query execution + cache             |
| 4    | Tv/Prql.lean    | PRQL compilation (prqlc subprocess) |
| 5    | Tv/Types.lean   | Add ViewKind sum type               |
| 6    | Tv/State.lean   | Enhanced state with PRQL chain      |
| 7    | Tv/Command.lean | Command ADT                         |
| 8    | Tv/App.lean     | ViewKind dispatch + commands        |
| 9    | Tv/Source.lean  | System sources                      |
| 10   | Tv/Keymap.lean  | Configurable keybindings            |

---

## Dependencies

**System:**
- DuckDB 1.4+ (`/usr/lib/libduckdb.so`) - has ADBC entry point
- prqlc CLI (`prqlc` in PATH)
- termbox2 (existing)

**Lean packages:**
- None additional (pure FFI)

---

## ADBC Driver Loading (How Rust Does It)

```rust
// Load libduckdb.so dynamically
let driver = ManagedDriver::load_dynamic_from_filename(
    "/usr/lib/libduckdb.so",
    Some(b"duckdb_adbc_init"),  // entry point
    AdbcVersion::V110
);

// Create database (in-memory)
let opts = vec![("path", "")];
let db = driver.new_database_with_opts(opts);

// Connect and query
let conn = db.new_connection();
let stmt = conn.new_statement();
stmt.set_sql_query(sql);
let stream = stmt.execute();  // ArrowArrayStream
```

**C equivalent:**
```c
// Entry point signature
typedef AdbcStatusCode (*AdbcDriverInitFunc)(
    int version,
    void* driver,
    struct AdbcError* error
);

// Load driver
void* lib = dlopen("/usr/lib/libduckdb.so", RTLD_NOW);
AdbcDriverInitFunc init = dlsym(lib, "duckdb_adbc_init");

struct AdbcDriver driver = {0};
init(ADBC_VERSION_1_1_0, &driver, NULL);

// Use driver function pointers
driver.DatabaseNew(&db, &error);
driver.DatabaseSetOption(&db, "path", "", &error);
driver.DatabaseInit(&db, &error);
driver.ConnectionNew(&conn, &error);
driver.ConnectionInit(&conn, &db, &error);
driver.StatementNew(&conn, &stmt, &error);
driver.StatementSetSqlQuery(&stmt, sql, &error);
driver.StatementExecuteQuery(&stmt, &stream, NULL, &error);
```
