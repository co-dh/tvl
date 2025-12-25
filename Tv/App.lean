/-
  Application state and event loop
-/
import Tv.Types
import Tv.Viewport
import Tv.Term
import Tv.Render
import Tv.Backend

namespace App

-- | View kind: how to render/interact
inductive ViewKind where
  | tbl                    -- table view
  | freqV (col : String)   -- frequency view for column
  | colMeta                -- column metadata
  | fld                    -- folder browser
  | info (col row : Nat)   -- info box for cell
  deriving Inhabited

-- | Single view with PRQL query
structure View where
  path   : String        -- file path
  prql   : String        -- PRQL query (from df | ...)
  disp   : String := ""  -- display name for tab (if different from prql)
  rowVP  : Viewport
  colVP  : Viewport
  vkind  : ViewKind := .tbl
  cache  : Option Table := none  -- cached result
  keyCols : List Nat := []       -- key columns for aggregate/pivot
  selCols : List Nat := []       -- selected columns for aggregate
  total  : Option Nat := none    -- total row count (from cnt query)
  decimals : Nat := 3            -- decimal precision for floats

-- | App state with view stack
-- | Pending input for interactive commands (select, rename, filter, etc.)
inductive InputMode where
  | none                    -- normal mode
  | selectCols              -- waiting for column names
  | renameTo                -- waiting for new column name
  | filterExpr              -- waiting for filter expression
  | command                 -- command mode
  deriving Inhabited

structure State where
  views    : List View      -- head is current, tail is parent stack
  keys     : List Char := [] -- pending keys to replay
  msg      : String := ""   -- status message
  quit     : Bool := false
  testMode : Bool := false  -- exit after keys consumed
  inputMode : InputMode := .none  -- current input mode
  inputBuf  : String := ""        -- input buffer for interactive commands

-- | Default empty view
def View.empty : View := ⟨"", "from df", "", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩

-- | Current view
def State.cur (s : State) : View := s.views.headD View.empty

-- | Update current view
def State.setCur (s : State) (v : View) : State :=
  { s with views := v :: s.views.tailD [] }

-- | Push new view
def State.push (s : State) (v : View) : State :=
  { s with views := v :: s.views }

-- | Pop view (returns to parent)
def State.pop (s : State) : State :=
  { s with views := s.views.tailD [] }

-- | Swap top two views
def State.swapViews (s : State) : State :=
  match s.views with
  | v1 :: v2 :: rest => { s with views := v2 :: v1 :: rest }
  | _ => s

-- | Duplicate current view
def State.dupView (s : State) : State :=
  match s.views with
  | v :: _ => { s with views := v :: s.views }
  | [] => s

-- | Set status message
def State.setMsg (s : State) (m : String) : State := { s with msg := m }

-- | Max rows to fetch (prevent OOM on huge files)
def maxRows : Nat := 1000

-- | Fetch table for view (uses cache or queries backend)
def View.fetch (v : View) : IO (View × Table) := do
  match v.cache with
  | some t => return (v, t)
  | none =>
    match ← Backend.query (Backend.mkLimited v.prql maxRows) v.path with
    | .ok t =>
      -- also fetch total count if not cached
      let total ← match v.total with
        | some n => pure n
        | none => do
          match ← Backend.queryCount v.prql v.path with
          | .ok n => pure n
          | .error _ => pure t.nRows
      return ({ v with cache := some t, total := some total }, t)
    | .error e =>
      Backend.logError s!"Query error: {e}"
      return (v, Table.empty)

-- | Invalidate cache (after PRQL change)
def View.invalidate (v : View) : View := { v with cache := none }

-- | View.copy helper for updating PRQL and resetting viewport/cache/total
def View.copy (v : View) (prql : String := v.prql) (rowVP : Viewport := v.rowVP) : View :=
  { v with prql := prql, rowVP := rowVP, cache := none, total := none }

-- | Quote column name for PRQL (use this. prefix for stdlib conflicts)
def quoteName (s : String) : String :=
  let reserved := ["count", "sum", "avg", "min", "max", "average", "group", "sort", "filter", "select", "derive", "from", "take", "date", "time"]
  if reserved.contains s then s!"this.{s}" else s

-- | Character codes
def chJ : UInt32 := 106
def chK : UInt32 := 107
def chL : UInt32 := 108
def chH : UInt32 := 104
def chG : UInt32 := 103
def chGG : UInt32 := 71  -- 'G'
def chD : UInt32 := 68   -- 'D'
def chF : UInt32 := 70   -- 'F' for freq
def chQ : UInt32 := 113  -- 'q'
def chCtrlC : UInt32 := 3  -- Ctrl+C
def chCtrlD : UInt32 := 4  -- Ctrl+D (page down)
def chCtrlU : UInt32 := 21 -- Ctrl+U (page up)
def chLBrack : UInt32 := 91  -- '[' sort asc
def chRBrack : UInt32 := 93  -- ']' sort desc
def chM : UInt32 := 77       -- 'M' meta view
def chAt : UInt32 := 64      -- '@' column jump
def chBackslash : UInt32 := 92 -- '\' filter
def chS : UInt32 := 115      -- 's' select columns
def chI : UInt32 := 73       -- 'I' info box
def chT : UInt32 := 84       -- 'T' duplicate view
def chSS : UInt32 := 83      -- 'S' swap views
def chExcl : UInt32 := 33    -- '!' toggle key column
def chB : UInt32 := 98       -- 'b' aggregate
def chColon : UInt32 := 58   -- ':' command mode
def chLL : UInt32 := 76      -- 'L' load file
def chR : UInt32 := 114      -- 'r' list directory
def chSpace : UInt32 := 32   -- Space toggle selection
def ch0 : UInt32 := 48       -- '0' first column
def chDollar : UInt32 := 36  -- '$' last column
def chCaret : UInt32 := 94   -- '^' rename column
def chDot : UInt32 := 46     -- '.' increase decimals
def chComma : UInt32 := 44   -- ',' decrease decimals

-- | Format cell value for PRQL filter
def cellToPrql : Cell → String
  | .null => "null"
  | .int n => s!"{n}"
  | .float f => s!"{f}"
  | .str s => s!"'{s}'"
  | .bool b => if b then "true" else "false"

-- | Key handler context (common params for all handlers)
structure KeyCtx where
  s  : State         -- app state
  v  : View          -- current view
  di : DisplayInfo   -- display info (no cell access)
  nr : Nat           -- row count
  nc : Nat           -- col count
  pg : Nat           -- page size

-- | Key handler result
abbrev KeyResult := IO State

-- | Run fzf picker (suspends terminal)
def runFzf (opts : List String) (input : String) : IO (Option String) := do
  Term.shutdown
  let child ← IO.Process.spawn {
    cmd := "fzf"
    args := opts.toArray
    stdin := .piped
    stdout := .piped
  }
  child.stdin.putStr input
  child.stdin.flush
  let (_, child') ← child.takeStdin
  let out ← child'.stdout.readToEnd
  let _ ← child'.wait
  let _ ← Term.init
  let result := out.trim
  return if result.isEmpty then none else some result

-- | Run bat to display file (suspends terminal)
def runBat (path : String) : IO Unit := do
  Term.shutdown
  let _ ← IO.Process.spawn {
    cmd := "bat"
    args := #["--paging=always", path]
    stdin := .inherit
    stdout := .inherit
  } >>= (·.wait)
  let _ ← Term.init

-- | Run fzf multi-select
def runFzfMulti (opts : List String) (input : String) : IO (List String) := do
  Term.shutdown
  let child ← IO.Process.spawn {
    cmd := "fzf"
    args := ("-m" :: opts).toArray
    stdin := .piped
    stdout := .piped
  }
  child.stdin.putStr input
  child.stdin.flush
  let (_, child') ← child.takeStdin
  let out ← child'.stdout.readToEnd
  let _ ← child'.wait
  let _ ← Term.init
  return out.splitOn "\n" |>.map String.trim |>.filter (!·.isEmpty)

namespace Key

-- | j - move down
def j (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := c.v.rowVP.moveRight c.nr })

-- | k - move up
def k (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := c.v.rowVP.moveLeft })

-- | l - move right
def l (c : KeyCtx) : KeyResult := do
  let next := Render.nextInDisplay c.v.keyCols c.nc c.v.colVP.cursor
  pure (c.s.setCur { c.v with colVP := ⟨next, c.v.colVP.offset⟩ })

-- | h - move left
def h (c : KeyCtx) : KeyResult := do
  let prev := Render.prevInDisplay c.v.keyCols c.nc c.v.colVP.cursor
  pure (c.s.setCur { c.v with colVP := ⟨prev, c.v.colVP.offset⟩ })

-- | Ctrl-D - page down
def ctrlD (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := c.v.rowVP.pageDown c.pg c.nr })

-- | Ctrl-U - page up
def ctrlU (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := c.v.rowVP.pageUp c.pg })

-- | g - go to top
def g (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := Viewport.goTop })

-- | G - go to end
def G (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := Viewport.goEnd c.nr })

-- | 0 - first column
def zero (c : KeyCtx) : KeyResult := do
  let first := Render.displayOrder c.v.keyCols c.nc |>.headD 0
  pure (c.s.setCur { c.v with colVP := ⟨first, 0⟩ })

-- | $ - last column
def dollar (c : KeyCtx) : KeyResult := do
  let last := Render.displayOrder c.v.keyCols c.nc |>.getLast? |>.getD 0
  pure (c.s.setCur { c.v with colVP := ⟨last, c.v.colVP.offset⟩ })

-- | [ - sort ascending
def lbrak (c : KeyCtx) : KeyResult := do
  let colName := c.di.colNames.getD c.v.colVP.cursor "?"
  let prql := c.v.prql ++ " | sort {" ++ colName ++ "}"
  pure (c.s.setCur (c.v.copy (prql := prql)))

-- | ] - sort descending
def rbrak (c : KeyCtx) : KeyResult := do
  let colName := c.di.colNames.getD c.v.colVP.cursor "?"
  let prql := c.v.prql ++ " | sort {-" ++ colName ++ "}"
  pure (c.s.setCur (c.v.copy (prql := prql)))

-- | D - delete column(s)
def D (c : KeyCtx) : KeyResult := do
  let delCols := if c.v.selCols.isEmpty then [c.v.colVP.cursor] else c.v.selCols
  let delNames := delCols.map fun i => c.di.colNames.getD i "?"
  let allCols := c.di.colNames.toList.filter (!delNames.contains ·) |>.map quoteName
  if allCols.length > 0 then
    let selPrql := c.v.prql ++ " | select {" ++ String.intercalate ", " allCols ++ "}"
    let prevDel := if c.v.disp.startsWith "del " then c.v.disp.drop 4 else ""
    let delStr := String.intercalate "," delNames
    let newDisp := if prevDel.isEmpty then s!"del {delStr}" else s!"del {prevDel},{delStr}"
    let newColVP := if c.v.colVP.cursor ≥ c.nc - delCols.length then c.v.colVP.moveLeft else c.v.colVP
    let v' := { c.v.copy (prql := selPrql) with disp := newDisp, colVP := newColVP, selCols := [] }
    pure (c.s.setCur v'.invalidate)
  else pure c.s

-- | @ - column jump with fzf
def atSign (c : KeyCtx) : KeyResult := do
  let colNamesStr := c.di.colNames.toList |> String.intercalate "\n"
  match ← runFzf ["--prompt=Column: "] colNamesStr with
  | some col =>
    match c.di.colNames.toList.findIdx? (· == col) with
    | some idx => pure (c.s.setCur { c.v with colVP := Viewport.goto idx c.nc })
    | none => pure c.s
  | none => pure c.s

-- | \ - filter with fzf
def backslash (c : KeyCtx) : KeyResult := do
  let colName := c.di.colNames.getD c.v.colVP.cursor "?"
  match ← Backend.queryDistinct c.v.prql c.v.path colName with
  | .ok vals =>
    match ← runFzf ["--prompt=Filter " ++ colName ++ ": "] (String.intercalate "\n" vals) with
    | some val =>
      let quotedVal := "'" ++ val.replace "'" "''" ++ "'"
      let filterPrql := c.v.prql ++ " | filter " ++ colName ++ " == " ++ quotedVal
      let fv : View := ⟨c.v.path, filterPrql, s!"filter {colName}", Viewport.create, Viewport.create, .tbl, none, [], [], none, c.v.decimals⟩
      pure (c.s.push fv)
    | none => pure c.s
  | .error _ => pure c.s

-- | s - select columns
def s (c : KeyCtx) : KeyResult := do
  if c.s.testMode then
    pure { c.s with inputMode := .selectCols, inputBuf := "" }
  else
    let colNamesStr := c.di.colNames.toList |> String.intercalate "\n"
    let selected ← runFzfMulti ["--prompt=Select: "] colNamesStr
    if selected.length > 0 then
      let quoted := selected.map quoteName
      let selPrql := c.v.prql ++ " | select {" ++ String.intercalate ", " quoted ++ "}"
      pure (c.s.setCur (c.v.copy (prql := selPrql)))
    else pure c.s

-- | M - meta view
def M (c : KeyCtx) : KeyResult := do
  match ← Backend.queryMeta c.v.prql c.v.path with
  | .ok metaTbl =>
    let mv : View := ⟨c.v.path, c.v.prql, "meta", Viewport.create, Viewport.create, .colMeta, some metaTbl, [], [], some metaTbl.nRows, 3⟩
    pure (c.s.push mv)
  | .error e => pure (c.s.setMsg s!"meta error: {e}")

-- | I - info box
def I (c : KeyCtx) : KeyResult := do
  let iv : View := ⟨c.v.path, c.v.prql, "", Viewport.create, Viewport.create, .info c.v.colVP.cursor c.v.rowVP.cursor, c.v.cache, c.v.keyCols, c.v.selCols, c.v.total, c.v.decimals⟩
  pure (c.s.push iv)

-- | F - frequency view
def F (c : KeyCtx) : KeyResult := do
  let (cols, colStr) := if c.v.keyCols.isEmpty then
    let name := c.di.colNames.getD c.v.colVP.cursor "?"
    ([name], name)
  else
    let names := c.v.keyCols.map fun i => c.di.colNames.getD i "?"
    (names, String.intercalate "," names)
  let freqPrql := if cols.length == 1 then
    c.v.prql ++ " | freq " ++ cols.head!
  else
    c.v.prql ++ " | group {" ++ colStr ++ "} (aggregate {Cnt = count this}) | derive {Pct = Cnt * 100 / sum Cnt, Bar = s\"repeat('#', CAST({Pct} / 5 AS INTEGER))\"} | sort {-Cnt}"
  let freqKeys := List.range cols.length
  let fv : View := ⟨c.v.path, freqPrql, s!"freq {colStr}", Viewport.create, Viewport.create, .freqV colStr, none, freqKeys, [], none, 3⟩
  pure (c.s.push fv)

-- | ret - enter key (context-dependent)
def ret (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .freqV colNames =>
    -- freq view: push filtered view
    let cols := colNames.splitOn "," |>.map String.trim
    match ← Backend.queryRow c.v.prql c.v.path c.v.rowVP.cursor cols.length with
    | .error _ => pure c.s
    | .ok vals =>
      let filters := (List.range cols.length).zip cols |>.map fun (i, cn) =>
        let val := vals.getD i .null
        s!"{cn} == {cellToPrql val}"
      let filterExpr := String.intercalate " && " filters
      let parentPrql := match c.s.views.tail? with
        | some (pv :: _) => pv.prql
        | _ => "from df"
      let filterPrql := s!"{parentPrql} | filter {filterExpr}"
      let fv : View := ⟨c.v.path, filterPrql, s!"filter {filterExpr}", Viewport.create, Viewport.create, .tbl, none, [], [], none, c.v.decimals⟩
      pure (c.s.push fv)
  | _ =>
    -- folder view: enter folder or open file
    if c.v.path.startsWith "source:ls" then
      match ← Backend.queryRow c.v.prql c.v.path c.v.rowVP.cursor 9 with
      | .error _ => pure c.s
      | .ok vals =>
        let perms := match vals.getD 0 .null with | .str str => str | _ => ""
        let name := match vals.getD 8 .null with | .str str => str | _ => ""
        if name.isEmpty then pure c.s
        else
          let baseDir := if c.v.path == "source:ls" then "." else c.v.path.drop 10
          let fullPath := if baseDir == "." then name else s!"{baseDir}/{name}"
          if perms.startsWith "d" then
            let lsv : View := ⟨s!"source:ls:{fullPath}", "from df", s!"ls {name}", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
            pure (c.s.push lsv)
          else
            runBat fullPath
            pure c.s
    else pure c.s

-- | T - duplicate view
def T (c : KeyCtx) : KeyResult := pure c.s.dupView

-- | S - swap views
def S (c : KeyCtx) : KeyResult := pure c.s.swapViews

-- | ! - toggle key column
def excl (c : KeyCtx) : KeyResult := do
  let cols := if c.v.selCols.isEmpty then [c.v.colVP.cursor] else c.v.selCols
  let allIn := cols.all c.v.keyCols.contains
  let newKeys := if allIn then c.v.keyCols.filter (!cols.contains ·)
                 else c.v.keyCols ++ cols.filter (!c.v.keyCols.contains ·)
  pure (c.s.setCur { c.v with keyCols := newKeys, selCols := [] })

-- | Space - toggle column selection
def space (c : KeyCtx) : KeyResult := do
  let col := c.v.colVP.cursor
  let newSel := if c.v.selCols.contains col then c.v.selCols.filter (· != col)
                else c.v.selCols ++ [col]
  pure (c.s.setCur { c.v with selCols := newSel })

-- | b - aggregate by key columns
def b (c : KeyCtx) : KeyResult := do
  if c.v.keyCols.isEmpty then pure { c.s with msg := "No key columns set (use !)" }
  else
    let keyNames := c.v.keyCols.map fun i => c.di.colNames.getD i "?"
    let aggCols := if c.v.selCols.isEmpty then [c.v.colVP.cursor] else c.v.selCols
    let aggNames := aggCols.map fun i => c.di.colNames.getD i "?"
    let aggExprs := aggNames.map fun n => s!"sum_{n} = sum {n}, cnt_{n} = count {n}"
    let aggPrql := c.v.prql ++ " | group {" ++ String.intercalate ", " keyNames ++
                   "} (aggregate {" ++ String.intercalate ", " aggExprs ++ "})"
    let av : View := ⟨c.v.path, aggPrql, "agg", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
    pure (c.s.push av)

-- | : - command mode
def colon (c : KeyCtx) : KeyResult := do
  if c.s.testMode then pure { c.s with inputMode := .command, inputBuf := "" }
  else
    match ← runFzf ["--prompt=: "] "ps\nenv\ndf\nls\ntcp" with
    | some cmd =>
      let sv : View := ⟨s!"source:{cmd}", "from df", "", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
      pure (c.s.push sv)
    | none => pure c.s

-- | ^ - rename column
def caret (c : KeyCtx) : KeyResult := do
  if c.s.testMode then pure { c.s with inputMode := .renameTo, inputBuf := "" }
  else pure c.s

-- | . - increase decimals
def dot (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with decimals := c.v.decimals + 1, cache := none })

-- | , - decrease decimals
def comma (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with decimals := if c.v.decimals > 0 then c.v.decimals - 1 else 0, cache := none })

-- | L - load file
def L (c : KeyCtx) : KeyResult := do
  match ← runFzf ["--prompt=Load: "] "" with
  | some path =>
    let lv : View := ⟨path, "from df", "", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
    pure (c.s.push lv)
  | none => pure c.s

-- | r - list directory
def r (c : KeyCtx) : KeyResult := do
  let rv : View := ⟨"source:ls", "from df", "ls ./", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
  pure (c.s.push rv)

-- | q - quit/pop
def q (c : KeyCtx) : KeyResult := do
  if c.s.views.length > 1 then pure c.s.pop
  else pure { c.s with quit := true }

-- | Esc - clear selection or pop
def esc (c : KeyCtx) : KeyResult := do
  if !c.v.selCols.isEmpty then pure (c.s.setCur { c.v with selCols := [] })
  else if c.s.views.length > 1 then pure c.s.pop
  else pure { c.s with quit := true }

-- | Ctrl-C - quit
def ctrlC (_ : KeyCtx) (s : State) : KeyResult := pure { s with quit := true }

end Key

-- | Handle input modes (collecting chars until Enter)
def handleInput (s : State) (v : View) (di : DisplayInfo) (ev : Term.Event) : IO (Option State) := do
  match s.inputMode with
  | .selectCols =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let cols := s.inputBuf.splitOn "," |>.map String.trim |>.filter (!·.isEmpty)
      if cols.length > 0 then
        let quoted := cols.map quoteName
        let selPrql := v.prql ++ " | select {" ++ String.intercalate ", " quoted ++ "}"
        return some { s.setCur (v.copy (prql := selPrql)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .renameTo =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let newName := s.inputBuf.trim
      if !newName.isEmpty then
        let oldName := di.colNames.getD v.colVP.cursor "?"
        let allCols := di.colNames.toList.map fun n => if n == oldName then quoteName newName else quoteName n
        let renamePrql := v.prql ++ " | derive {" ++ quoteName newName ++ " = " ++ quoteName oldName ++
                          "} | select {" ++ String.intercalate ", " allCols ++ "}"
        return some { s.setCur (v.copy (prql := renamePrql)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .filterExpr =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let expr := s.inputBuf.trim
      if !expr.isEmpty then
        let filterPrql := v.prql ++ " | filter " ++ expr
        return some { s.setCur (v.copy (prql := filterPrql) (rowVP := Viewport.create)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .command =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let cmd := s.inputBuf.trim
      if cmd.startsWith "freq " then
        let cols := cmd.drop 5 |>.trim
        let colList := cols.splitOn "," |>.map String.trim
        let freqPrql := v.prql ++ " | group {" ++ cols ++ "} (aggregate {Cnt = count this}) | derive {Pct = Cnt * 100 / sum Cnt, Bar = s\"repeat('#', CAST({Pct} / 5 AS INTEGER))\"} | sort {-Cnt}"
        let fv : View := ⟨v.path, freqPrql, s!"freq {cols}", Viewport.create, Viewport.create, .freqV cols, none, List.range colList.length, [], none, 3⟩
        return some { s.push fv with inputMode := .none, inputBuf := "" }
      else if cmd.startsWith "lr " then
        let dir := cmd.drop 3 |>.trim
        let lrPrql := s!"from (read_csv('{dir}/**/*', union_by_name=true, filename=true))"
        let lrv : View := ⟨dir, lrPrql, "lr", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
        return some { s.push lrv with inputMode := .none, inputBuf := "" }
      else if cmd.startsWith "filter " then
        let expr := cmd.drop 7 |>.trim
        let filterPrql := v.prql ++ " | filter " ++ expr
        return some { s.setCur (v.copy (prql := filterPrql) (rowVP := Viewport.create)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "", msg := s!"unknown: {cmd}" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .none => return none  -- not in input mode

-- | Handle key event (takes DisplayInfo, not Table - no cell access)
def handleKey (s : State) (di : DisplayInfo) (ev : Term.Event) (screenH : Nat) : IO State := do
  let v := s.cur
  -- handle input mode first
  match ← handleInput s v di ev with
  | some s' => return s'
  | none =>
  -- build context for key handlers
  let c : KeyCtx := ⟨s, v, di, di.nRows, di.nCols, max 1 (screenH - 2)⟩
  -- dispatch to key handlers
  if ev.key == Term.keyArrowDown || ev.ch == chJ then Key.j c
  else if ev.key == Term.keyArrowUp || ev.ch == chK then Key.k c
  else if ev.key == Term.keyArrowRight || ev.ch == chL then Key.l c
  else if ev.key == Term.keyArrowLeft || ev.ch == chH then Key.h c
  else if ev.key == Term.keyPageDown || ev.ch == chCtrlD then Key.ctrlD c
  else if ev.key == Term.keyPageUp || ev.ch == chCtrlU then Key.ctrlU c
  else if ev.key == Term.keyHome || ev.ch == chG then Key.g c
  else if ev.key == Term.keyEnd || ev.ch == chGG then Key.G c
  else if ev.ch == ch0 then Key.zero c
  else if ev.ch == chDollar then Key.dollar c
  else if ev.ch == chLBrack then Key.lbrak c
  else if ev.ch == chRBrack then Key.rbrak c
  else if ev.ch == chD then Key.D c
  else if ev.ch == chAt then Key.atSign c
  else if ev.ch == chBackslash then Key.backslash c
  else if ev.ch == chS then Key.s c
  else if ev.ch == chM then Key.M c
  else if ev.ch == chI then Key.I c
  else if ev.ch == chF then Key.F c
  else if ev.key == Term.keyEnter || ev.ch == 13 then Key.ret c
  else if ev.ch == chT then Key.T c
  else if ev.ch == chSS then Key.S c
  else if ev.ch == chExcl then Key.excl c
  else if ev.ch == chSpace then Key.space c
  else if ev.ch == chB then Key.b c
  else if ev.ch == chColon then Key.colon c
  else if ev.ch == chCaret then Key.caret c
  else if ev.ch == chDot then Key.dot c
  else if ev.ch == chComma then Key.comma c
  else if ev.ch == chLL then Key.L c
  else if ev.ch == chR then Key.r c
  else if ev.ch == chQ then Key.q c
  else if ev.key == Term.keyEsc then Key.esc c
  else if ev.ch == chCtrlC then pure { s with quit := true }
  else pure s

-- | Main event loop
partial def loop (s : State) : IO Unit := do
  if s.quit || s.views.isEmpty then return ()
  let v := s.cur
  -- fetch table (uses cache if available)
  let (v', tbl) ← v.fetch
  let s := s.setCur v'
  -- extract display info (only way to get metadata for handleKey)
  let di := tbl.info
  -- render based on view kind (tbl only used here for rendering)
  let w ← Term.width
  let h ← Term.height
  let (newColOffset, s) ← match v'.vkind with
    | .info col row =>
      Render.infoBox tbl col row h.toNat w.toNat
      pure (v'.colVP.offset, s)
    | _ =>
      let off ← Render.table tbl v'.rowVP v'.colVP (h.toNat - 2) w.toNat v'.keyCols v'.decimals
      let disps := s.views.map fun v => (v.disp, v.prql)
      Render.tabLine v'.path disps (h - 2)
      Render.statusBar v'.rowVP.cursor (v'.total.getD di.nRows) w.toNat
                       v'.keyCols v'.selCols di.colNames (h - 1) s.msg
      Term.present
      pure (off, s)
  let v' := { v' with colVP := ⟨v'.colVP.cursor, newColOffset⟩ }
  let s := s.setCur v'
  -- test mode: exit after keys consumed
  if s.testMode && s.keys.isEmpty then
    let buf ← Term.bufferStr
    IO.print buf
    Term.shutdown
    return ()
  -- get next event: from buffer or poll
  let (ev, s) ← match s.keys with
    | c :: rest =>
      let ev : Term.Event := ⟨Term.eventKey, 0, 0, c.toNat.toUInt32, 0, 0⟩
      pure (ev, { s with keys := rest })
    | [] =>
      let ev ← Term.pollEvent
      pure (ev, s)
  -- handleKey gets DisplayInfo only - no cell access possible
  let s' ← if ev.type == Term.eventKey then handleKey s di ev h.toNat else pure s
  loop s'

-- | Run app with optional replay keys
def run (path : String) (keys : String := "") (testMode : Bool := false) : IO Unit := do
  -- init backend before terminal (debug output goes to normal screen)
  let ok ← Backend.init
  if !ok then
    Backend.logError "Failed to init backend"
    return
  let r ← Term.init
  if r < 0 then
    Backend.logError "Failed to init terminal"
    return
  let v : View := ⟨path, "from df", "", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
  let s : State := { views := [v], keys := keys.toList, testMode := testMode }
  loop s
  Backend.shutdown
  Term.shutdown

end App
