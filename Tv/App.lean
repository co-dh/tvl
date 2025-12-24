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

-- | Initialize state from file with optional replay keys
def init (path : String) (keys : String := "") : IO State := do
  let ok ← Backend.init
  if !ok then
    IO.eprintln "Failed to init backend"
    return { views := [], quit := true }
  let v : View := ⟨path, "from df", Viewport.create, Viewport.create, .tbl, none⟩
  return { views := [v], keys := keys.toList }

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

-- | Format cell value for PRQL filter
def cellToPrql : Cell → String
  | .null => "null"
  | .int n => s!"{n}"
  | .float f => s!"{f}"
  | .str s => s!"'{s}'"
  | .bool b => if b then "true" else "false"

-- | Handle key event (takes full Event - can't forget fields)
def handleKey (s : State) (tbl : Table) (ev : Term.Event) (screenH : Nat) : State :=
  let v := s.cur
  let nr := tbl.nRows
  let nc := tbl.nCols
  let pageSize := max 1 (screenH - 2)
  -- movement keys
  if ev.key == Term.keyArrowDown || ev.ch == chJ then
    s.setCur { v with rowVP := v.rowVP.moveRight nr }
  else if ev.key == Term.keyArrowUp || ev.ch == chK then
    s.setCur { v with rowVP := v.rowVP.moveLeft }
  else if ev.key == Term.keyArrowRight || ev.ch == chL then
    s.setCur { v with colVP := v.colVP.moveRight nc }
  else if ev.key == Term.keyArrowLeft || ev.ch == chH then
    s.setCur { v with colVP := v.colVP.moveLeft }
  -- page up/down
  else if ev.key == Term.keyPageDown then
    s.setCur { v with rowVP := v.rowVP.pageDown pageSize nr }
  else if ev.key == Term.keyPageUp then
    s.setCur { v with rowVP := v.rowVP.pageUp pageSize }
  -- home/end (g/G)
  else if ev.key == Term.keyHome || ev.ch == chG then
    s.setCur { v with rowVP := Viewport.goTop }
  else if ev.key == Term.keyEnd || ev.ch == chGG then
    s.setCur { v with rowVP := Viewport.goEnd nr }
  -- freq: push freq view with PRQL
  else if ev.ch == chF then
    let col := v.colVP.cursor
    let colName := tbl.cols.getD col ⟨"?"⟩ |>.name
    let freqPrql := s!"{v.prql} | freq {colName} df"
    let fv : View := ⟨v.path, freqPrql, Viewport.create, Viewport.create, .freqV colName, none⟩
    s.push fv
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
        { parent with views := newPV :: rest }
      | [] => s
    | _ => s
  -- quit/pop: pop view or quit if at root
  else if ev.key == Term.keyEsc || ev.ch == chQ then
    if s.views.length > 1 then s.pop
    else { s with quit := true }
  else if ev.ch == chCtrlC then
    { s with quit := true }
  else s

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
  let s' := if ev.type == Term.eventKey then handleKey s tbl ev h.toNat else s
  loop s'

-- | Run app with optional replay keys
def run (path : String) (keys : String := "") : IO Unit := do
  let r ← Term.init
  if r < 0 then
    IO.eprintln "Failed to init terminal"
    return
  let s ← init path keys
  loop s
  Backend.shutdown
  Term.shutdown

end App
