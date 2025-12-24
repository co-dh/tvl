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
inductive ViewKind where | tbl | freqV (col : String) | colMeta | fld deriving Inhabited

-- | Single view with PRQL query
structure View where
  path   : String        -- file path
  prql   : String        -- PRQL query (from df | ...)
  rowVP  : Viewport
  colVP  : Viewport
  vkind  : ViewKind := .tbl
  cache  : Option Table := none  -- cached result

-- | App state with view stack
structure State where
  views  : List View      -- head is current, tail is parent stack
  keys   : List Char := [] -- pending keys to replay
  msg    : String := ""   -- status message
  quit   : Bool := false

-- | Default empty view
def View.empty : View := ⟨"", "from df", Viewport.create, Viewport.create, .tbl, none⟩

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

-- | Fetch table for view (uses cache or queries backend)
def View.fetch (v : View) : IO (View × Table) := do
  match v.cache with
  | some t => return (v, t)
  | none =>
    match ← Backend.query v.prql v.path with
    | .ok t => return ({ v with cache := some t }, t)
    | .error e =>
      IO.eprintln s!"Query error: {e}"
      return (v, Table.empty)

-- | Invalidate cache (after PRQL change)
def View.invalidate (v : View) : View := { v with cache := none }

-- | View.copy helper for updating PRQL and resetting viewport
def View.copy (v : View) (prql : String := v.prql) (rowVP : Viewport := v.rowVP) : View :=
  { v with prql := prql, rowVP := rowVP, cache := none }

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
def chQQ : UInt32 := 81  -- 'Q' dump table
def chCtrlC : UInt32 := 3  -- Ctrl+C
def chCtrlD : UInt32 := 4  -- Ctrl+D (page down)
def chCtrlU : UInt32 := 21 -- Ctrl+U (page up)
def chLBrack : UInt32 := 91  -- '[' sort asc
def chRBrack : UInt32 := 93  -- ']' sort desc
def chM : UInt32 := 77       -- 'M' meta view
def chAt : UInt32 := 64      -- '@' column jump
def chBackslash : UInt32 := 92 -- '\' filter
def chS : UInt32 := 115      -- 's' select columns

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

-- | Handle key event (takes full Event - can't forget fields)
def handleKey (s : State) (tbl : Table) (ev : Term.Event) (screenH : Nat) : IO State := do
  let v := s.cur
  let nr := tbl.nRows
  let nc := tbl.nCols
  let pageSize := max 1 (screenH - 2)
  -- movement keys
  if ev.key == Term.keyArrowDown || ev.ch == chJ then
    return s.setCur { v with rowVP := v.rowVP.moveRight nr }
  else if ev.key == Term.keyArrowUp || ev.ch == chK then
    return s.setCur { v with rowVP := v.rowVP.moveLeft }
  else if ev.key == Term.keyArrowRight || ev.ch == chL then
    return s.setCur { v with colVP := v.colVP.moveRight nc }
  else if ev.key == Term.keyArrowLeft || ev.ch == chH then
    return s.setCur { v with colVP := v.colVP.moveLeft }
  -- page up/down
  else if ev.key == Term.keyPageDown || ev.ch == chCtrlD then
    return s.setCur { v with rowVP := v.rowVP.pageDown pageSize nr }
  else if ev.key == Term.keyPageUp || ev.ch == chCtrlU then
    return s.setCur { v with rowVP := v.rowVP.pageUp pageSize }
  -- home/end (g/G)
  else if ev.key == Term.keyHome || ev.ch == chG then
    return s.setCur { v with rowVP := Viewport.goTop }
  else if ev.key == Term.keyEnd || ev.ch == chGG then
    return s.setCur { v with rowVP := Viewport.goEnd nr }
  -- sort asc/desc
  else if ev.ch == chLBrack then
    let col := v.colVP.cursor
    let colName := tbl.cols.getD col ⟨"?"⟩ |>.name
    let sortPrql := v.prql ++ " | sort {" ++ colName ++ "}"
    return s.setCur (v.copy (prql := sortPrql))
  else if ev.ch == chRBrack then
    let col := v.colVP.cursor
    let colName := tbl.cols.getD col ⟨"?"⟩ |>.name
    let sortPrql := v.prql ++ " | sort {-" ++ colName ++ "}"
    return s.setCur (v.copy (prql := sortPrql))
  -- delete column
  else if ev.ch == chD then
    let col := v.colVP.cursor
    let colName := tbl.cols.getD col ⟨"?"⟩ |>.name
    let allCols := tbl.cols.toList.map (·.name) |>.filter (· != colName)
    if allCols.length > 0 then
      let selPrql := v.prql ++ " | select {" ++ String.intercalate ", " allCols ++ "}"
      let newColVP := if v.colVP.cursor ≥ nc - 1 then v.colVP.moveLeft else v.colVP
      return s.setCur ((v.copy (prql := selPrql)).invalidate |> fun x => { x with colVP := newColVP })
    else return s
  -- column jump (@)
  else if ev.ch == chAt then
    let colNames := tbl.cols.toList.map (·.name) |> String.intercalate "\n"
    match ← runFzf ["--prompt=Column: "] colNames with
    | some col =>
      match tbl.cols.toList.findIdx? (·.name == col) with
      | some idx => return s.setCur { v with colVP := Viewport.goto idx nc }
      | none => return s
    | none => return s
  -- filter (\)
  else if ev.ch == chBackslash then
    let col := v.colVP.cursor
    let colName := tbl.cols.getD col ⟨"?"⟩ |>.name
    -- get distinct values for current column
    match ← Backend.query (v.prql ++ " | select {" ++ colName ++ "} | group {" ++ colName ++ "} (take 1)") v.path with
    | .ok valTbl =>
      let vals := (List.range valTbl.nRows).map (fun r => toString (valTbl.get r 0)) |> String.intercalate "\n"
      match ← runFzf ["--prompt=Filter " ++ colName ++ ": "] vals with
      | some val =>
        let filterPrql := v.prql ++ " | filter " ++ colName ++ " == " ++ val
        return s.setCur (v.copy (prql := filterPrql) (rowVP := Viewport.create))
      | none => return s
    | .error _ => return s
  -- select columns (s)
  else if ev.ch == chS then
    let colNames := tbl.cols.toList.map (·.name) |> String.intercalate "\n"
    let selected ← runFzfMulti ["--prompt=Select: "] colNames
    if selected.length > 0 then
      let selPrql := v.prql ++ " | select {" ++ String.intercalate ", " selected ++ "}"
      return s.setCur (v.copy (prql := selPrql))
    else return s
  -- meta view
  else if ev.ch == chM then
    let metaPrql := v.prql ++ " | meta df"
    let mv : View := ⟨v.path, metaPrql, Viewport.create, Viewport.create, .colMeta, none⟩
    return s.push mv
  -- freq: push freq view with PRQL
  else if ev.ch == chF then
    let col := v.colVP.cursor
    let colName := tbl.cols.getD col ⟨"?"⟩ |>.name
    let freqPrql := v.prql ++ " | freq " ++ colName
    let fv : View := ⟨v.path, freqPrql, Viewport.create, Viewport.create, .freqV colName, none⟩
    return s.push fv
  -- enter: in freq view, filter parent by selected value
  else if ev.key == Term.keyEnter then
    match v.vkind with
    | .freqV colName =>
      let selRow := v.rowVP.cursor
      let selVal := tbl.get selRow 0  -- first column is the value
      let parent := s.pop
      match parent.views with
      | pv :: rest =>
        let filterPrql := s!"{pv.prql} | filter {colName} == {cellToPrql selVal}"
        let newPV := (pv.invalidate).copy (prql := filterPrql) (rowVP := Viewport.create)
        return { parent with views := newPV :: rest }
      | [] => return s
    | _ => return s
  -- dump table to stdout and quit (Q)
  else if ev.ch == chQQ then
    Term.shutdown
    IO.println s!"PRQL: {v.prql}"
    IO.println s!"Rows: {tbl.nRows}, Cols: {tbl.nCols}"
    -- header
    IO.println (tbl.cols.toList.map (·.name) |> String.intercalate "\t")
    -- rows (max 50)
    for r in [:min tbl.nRows 50] do
      let row := (List.range tbl.nCols).map (fun c => toString (tbl.get r c)) |> String.intercalate "\t"
      IO.println row
    return { s with quit := true }
  -- quit/pop: pop view or quit if at root
  else if ev.key == Term.keyEsc || ev.ch == chQ then
    if s.views.length > 1 then return s.pop
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
  -- render
  let w ← Term.width
  let h ← Term.height
  let newColOffset ← Render.table tbl v'.rowVP v'.colVP h.toNat w.toNat
  Render.statusBar v'.path v'.rowVP.cursor v'.colVP.cursor
                   tbl.nRows tbl.nCols (h - 1)
  -- update column offset
  let v' := { v' with colVP := ⟨v'.colVP.cursor, newColOffset⟩ }
  let s := s.setCur v'
  -- get next event: from buffer or poll
  let (ev, s) ← match s.keys with
    | c :: rest =>
      -- create Event from char (key=0, ch=char)
      let ev : Term.Event := ⟨Term.eventKey, 0, 0, c.toNat.toUInt32, 0, 0⟩
      pure (ev, { s with keys := rest })
    | [] => do
      let ev ← Term.pollEvent
      pure (ev, s)
  let s' ← if ev.type == Term.eventKey then handleKey s tbl ev h.toNat else pure s
  loop s'

-- | Run app with optional replay keys
def run (path : String) (keys : String := "") : IO Unit := do
  -- init backend before terminal (debug output goes to normal screen)
  let ok ← Backend.init
  if !ok then
    IO.eprintln "Failed to init backend"
    return
  let r ← Term.init
  if r < 0 then
    IO.eprintln "Failed to init terminal"
    return
  let v : View := ⟨path, "from df", Viewport.create, Viewport.create, .tbl, none⟩
  let s : State := { views := [v], keys := keys.toList }
  loop s
  Backend.shutdown
  Term.shutdown

end App
