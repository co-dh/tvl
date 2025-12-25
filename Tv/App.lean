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

-- | Handle key event (takes DisplayInfo, not Table - no cell access)
def handleKey (s : State) (di : DisplayInfo) (ev : Term.Event) (screenH : Nat) : IO State := do
  let v := s.cur
  let nr := di.nRows
  let nc := di.nCols
  let pageSize := max 1 (screenH - 2)
  -- input mode: collect chars until Enter
  match s.inputMode with
  | .selectCols =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      -- execute select with collected input
      let cols := s.inputBuf.splitOn "," |>.map String.trim |>.filter (!·.isEmpty)
      if cols.length > 0 then
        let quoted := cols.map quoteName
        let selPrql := v.prql ++ " | select {" ++ String.intercalate ", " quoted ++ "}"
        return { s.setCur (v.copy (prql := selPrql)) with inputMode := .none, inputBuf := "" }
      else
        return { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then
      return { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else
      return s
  | .renameTo =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let newName := s.inputBuf.trim
      if !newName.isEmpty then
        let col := v.colVP.cursor
        let oldName := di.colNames.getD col "?"
        -- Build select with all columns, replacing old with new
        let allCols := di.colNames.toList.map fun n =>
          if n == oldName then quoteName newName else quoteName n
        let renamePrql := v.prql ++ " | derive {" ++ quoteName newName ++ " = " ++ quoteName oldName ++
                          "} | select {" ++ String.intercalate ", " allCols ++ "}"
        return { s.setCur (v.copy (prql := renamePrql)) with inputMode := .none, inputBuf := "" }
      else
        return { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then
      return { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else
      return s
  | .filterExpr =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let expr := s.inputBuf.trim
      if !expr.isEmpty then
        let filterPrql := v.prql ++ " | filter " ++ expr
        return { s.setCur (v.copy (prql := filterPrql) (rowVP := Viewport.create)) with inputMode := .none, inputBuf := "" }
      else
        return { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then
      return { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else
      return s
  | .command =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let cmd := s.inputBuf.trim
      -- parse command (freq, lr, filter, etc.)
      if cmd.startsWith "freq " then
        let cols := cmd.drop 5 |>.trim
        -- multi-column freq: group by all, count, sort
        let freqPrql := v.prql ++ " | group {" ++ cols ++ "} (aggregate {Cnt = count this}) | derive {Pct = Cnt * 100 / sum Cnt} | sort {-Cnt}"
        let fv : View := ⟨v.path, freqPrql, s!"freq {cols}", Viewport.create, Viewport.create, .freqV cols, none, [], [], none, 3⟩
        return { s.push fv with inputMode := .none, inputBuf := "" }
      else if cmd.startsWith "lr " then
        let dir := cmd.drop 3 |>.trim
        let lrPrql := s!"from (read_csv('{dir}/**/*', union_by_name=true, filename=true))"
        let lrv : View := ⟨dir, lrPrql, "lr", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
        return { s.push lrv with inputMode := .none, inputBuf := "" }
      else if cmd.startsWith "filter " then
        let expr := cmd.drop 7 |>.trim
        let filterPrql := v.prql ++ " | filter " ++ expr
        return { s.setCur (v.copy (prql := filterPrql) (rowVP := Viewport.create)) with inputMode := .none, inputBuf := "" }
      else
        return { s with inputMode := .none, inputBuf := "", msg := s!"unknown: {cmd}" }
    else if ev.ch > 0 then
      return { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else
      return s
  | .none =>
  -- movement keys
  if ev.key == Term.keyArrowDown || ev.ch == chJ then
    return s.setCur { v with rowVP := v.rowVP.moveRight nr }
  else if ev.key == Term.keyArrowUp || ev.ch == chK then
    return s.setCur { v with rowVP := v.rowVP.moveLeft }
  else if ev.key == Term.keyArrowRight || ev.ch == chL then
    let next := Render.nextInDisplay v.keyCols nc v.colVP.cursor
    return s.setCur { v with colVP := ⟨next, v.colVP.offset⟩ }
  else if ev.key == Term.keyArrowLeft || ev.ch == chH then
    let prev := Render.prevInDisplay v.keyCols nc v.colVP.cursor
    return s.setCur { v with colVP := ⟨prev, v.colVP.offset⟩ }
  -- page up/down (Ctrl-D/U: ch=4/21 in test mode, key in real terminal)
  else if ev.key == Term.keyPageDown || ev.ch == chCtrlD then
    return s.setCur { v with rowVP := v.rowVP.pageDown pageSize nr }
  else if ev.key == Term.keyPageUp || ev.ch == chCtrlU then
    return s.setCur { v with rowVP := v.rowVP.pageUp pageSize }
  -- home/end rows (g/G)
  else if ev.key == Term.keyHome || ev.ch == chG then
    return s.setCur { v with rowVP := Viewport.goTop }
  else if ev.key == Term.keyEnd || ev.ch == chGG then
    return s.setCur { v with rowVP := Viewport.goEnd nr }
  -- first/last column (0/$)
  else if ev.ch == ch0 then
    let first := Render.displayOrder v.keyCols nc |>.headD 0
    return s.setCur { v with colVP := ⟨first, 0⟩ }
  else if ev.ch == chDollar then
    let last := Render.displayOrder v.keyCols nc |>.getLast? |>.getD 0
    return s.setCur { v with colVP := ⟨last, v.colVP.offset⟩ }
  -- sort asc/desc
  else if ev.ch == chLBrack then
    let col := v.colVP.cursor
    let colName := di.colNames.getD col "?"
    let sortPrql := v.prql ++ " | sort {" ++ colName ++ "}"
    return s.setCur (v.copy (prql := sortPrql))
  else if ev.ch == chRBrack then
    let col := v.colVP.cursor
    let colName := di.colNames.getD col "?"
    let sortPrql := v.prql ++ " | sort {-" ++ colName ++ "}"
    return s.setCur (v.copy (prql := sortPrql))
  -- delete column(s): use selected cols if any, else cursor
  else if ev.ch == chD then
    let delCols := if v.selCols.isEmpty then [v.colVP.cursor] else v.selCols
    let delNames := delCols.map fun i => di.colNames.getD i "?"
    let allCols := di.colNames.toList.filter (!delNames.contains ·) |>.map quoteName
    if allCols.length > 0 then
      let selPrql := v.prql ++ " | select {" ++ String.intercalate ", " allCols ++ "}"
      -- disp shows cumulative deleted columns
      let prevDel := if v.disp.startsWith "del " then v.disp.drop 4 else ""
      let delStr := String.intercalate "," delNames
      let newDisp := if prevDel.isEmpty then s!"del {delStr}" else s!"del {prevDel},{delStr}"
      let newColVP := if v.colVP.cursor ≥ nc - delCols.length then v.colVP.moveLeft else v.colVP
      let v' := { v.copy (prql := selPrql) with disp := newDisp, colVP := newColVP, selCols := [] }
      return s.setCur v'.invalidate
    else return s
  -- column jump (@)
  else if ev.ch == chAt then
    let colNamesStr := di.colNames.toList |> String.intercalate "\n"
    match ← runFzf ["--prompt=Column: "] colNamesStr with
    | some col =>
      match di.colNames.toList.findIdx? (· == col) with
      | some idx => return s.setCur { v with colVP := Viewport.goto idx nc }
      | none => return s
    | none => return s
  -- filter (\) - query all distinct values
  else if ev.ch == chBackslash then
    let col := v.colVP.cursor
    let colName := di.colNames.getD col "?"
    match ← Backend.queryDistinct v.prql v.path colName with
    | .ok vals =>
      match ← runFzf ["--prompt=Filter " ++ colName ++ ": "] (String.intercalate "\n" vals) with
      | some val =>
        -- quote value in case it has spaces
        let quotedVal := "'" ++ val.replace "'" "''" ++ "'"
        let filterPrql := v.prql ++ " | filter " ++ colName ++ " == " ++ quotedVal
        return s.setCur (v.copy (prql := filterPrql) (rowVP := Viewport.create))
      | none => return s
    | .error _ => return s
  -- select columns (s) - use input mode in test mode
  else if ev.ch == chS then
    if s.testMode then
      return { s with inputMode := .selectCols, inputBuf := "" }
    else
      let colNamesStr := di.colNames.toList |> String.intercalate "\n"
      let selected ← runFzfMulti ["--prompt=Select: "] colNamesStr
      if selected.length > 0 then
        let quoted := selected.map quoteName
        let selPrql := v.prql ++ " | select {" ++ String.intercalate ", " quoted ++ "}"
        return s.setCur (v.copy (prql := selPrql))
      else return s
  -- meta view: query column stats from backend (full data)
  else if ev.ch == chM then
    match ← Backend.queryMeta v.prql v.path with
    | .ok metaTbl =>
      let mv : View := ⟨v.path, v.prql, "meta", Viewport.create, Viewport.create, .colMeta, some metaTbl, [], [], some metaTbl.nRows, 3⟩
      return s.push mv
    | .error e =>
      return s.setMsg s!"meta error: {e}"
  -- info box (I) - show cell/column details
  else if ev.ch == chI then
    let col := v.colVP.cursor
    let row := v.rowVP.cursor
    let iv : View := ⟨v.path, v.prql, "", Viewport.create, Viewport.create, .info col row, v.cache, v.keyCols, v.selCols, v.total, v.decimals⟩
    return s.push iv
  -- freq: push freq view with PRQL
  else if ev.ch == chF then
    let col := v.colVP.cursor
    let colName := di.colNames.getD col "?"
    let freqPrql := v.prql ++ " | freq " ++ colName
    let fv : View := ⟨v.path, freqPrql, s!"freq {colName}", Viewport.create, Viewport.create, .freqV colName, none, [], [], none, 3⟩
    return s.push fv
  -- enter: in freq view, filter parent by selected value (key=0x0D or ch=13)
  else if ev.key == Term.keyEnter || ev.ch == 13 then
    match v.vkind with
    | .freqV colNames =>
      let selRow := v.rowVP.cursor
      -- multi-column: query backend for row values
      let cols := colNames.splitOn "," |>.map String.trim
      match ← Backend.queryRow v.prql v.path selRow cols.length with
      | .error _ => return s
      | .ok vals =>
        let filters := (List.range cols.length).zip cols |>.map fun (i, c) =>
          let val := vals.getD i .null
          s!"{c} == {cellToPrql val}"
        let filterExpr := String.intercalate " && " filters
        let parent := s.pop
        match parent.views with
        | pv :: rest =>
          let filterPrql := s!"{pv.prql} | filter {filterExpr}"
          let newPV := (pv.invalidate).copy (prql := filterPrql) (rowVP := Viewport.create)
          return { parent with views := newPV :: rest }
        | [] => return s
    | _ => return s
  -- duplicate view (T)
  else if ev.ch == chT then
    return s.dupView
  -- swap top two views (S)
  else if ev.ch == chSS then
    return s.swapViews
  -- toggle key column (!) - if selected cols exist, use those; else cursor
  else if ev.ch == chExcl then
    let cols := if v.selCols.isEmpty then [v.colVP.cursor] else v.selCols
    let allIn := cols.all v.keyCols.contains
    let newKeys := if allIn
      then v.keyCols.filter (!cols.contains ·)
      else v.keyCols ++ cols.filter (!v.keyCols.contains ·)
    return s.setCur { v with keyCols := newKeys, selCols := [] }
  -- toggle column selection (Space)
  else if ev.ch == chSpace then
    let col := v.colVP.cursor
    let newSel := if v.selCols.contains col
      then v.selCols.filter (· != col)
      else v.selCols ++ [col]
    return s.setCur { v with selCols := newSel }
  -- aggregate by key columns (b)
  -- sum selected columns, count all
  else if ev.ch == chB then
    if v.keyCols.isEmpty then return { s with msg := "No key columns set (use !)" }
    let keyNames := v.keyCols.map fun i => di.colNames.getD i "?"
    let aggCols := if v.selCols.isEmpty then [v.colVP.cursor] else v.selCols
    let aggNames := aggCols.map fun i => di.colNames.getD i "?"
    let aggExprs := aggNames.map fun n => s!"sum_{n} = sum {n}, cnt_{n} = count {n}"
    let aggPrql := v.prql ++ " | group {" ++ String.intercalate ", " keyNames ++
                   "} (aggregate {" ++ String.intercalate ", " aggExprs ++ "})"
    let av : View := ⟨v.path, aggPrql, "agg", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
    return s.push av
  -- command mode (:) - use input mode in test mode
  else if ev.ch == chColon then
    if s.testMode then
      return { s with inputMode := .command, inputBuf := "" }
    else
      let cmds := "ps\nenv\ndf\nls\ntcp"
      match ← runFzf ["--prompt=: "] cmds with
      | some cmd =>
        let srcPath := s!"source:{cmd}"
        let sv : View := ⟨srcPath, "from df", "", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
        return s.push sv
      | none => return s
  -- rename column (^)
  else if ev.ch == chCaret then
    if s.testMode then
      return { s with inputMode := .renameTo, inputBuf := "" }
    else
      -- In interactive mode, could use fzf prompt for new name
      return s
  -- increase decimals (.)
  else if ev.ch == chDot then
    return s.setCur { v with decimals := v.decimals + 1, cache := none }
  -- decrease decimals (,)
  else if ev.ch == chComma then
    return s.setCur { v with decimals := if v.decimals > 0 then v.decimals - 1 else 0, cache := none }
  -- load file (L)
  else if ev.ch == chLL then
    match ← runFzf ["--prompt=Load: "] "" with
    | some path =>
      let lv : View := ⟨path, "from df", "", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
      return s.push lv
    | none => return s
  -- list directory (r)
  else if ev.ch == chR then
    let rv : View := ⟨"source:ls", "from df", "", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
    return s.push rv
  -- quit/pop: pop view or quit if at root
  else if ev.ch == chQ then
    if s.views.length > 1 then return s.pop
    else return { s with quit := true }
  -- Esc: clear selection, or pop if no selection
  else if ev.key == Term.keyEsc then
    if !v.selCols.isEmpty then return s.setCur { v with selCols := [] }
    else if s.views.length > 1 then return s.pop
    else return { s with quit := true }
  else if ev.ch == chCtrlC then
    return { s with quit := true }
  else return s

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
