/-
  Application state and event loop
-/
import Tv.Types
import Tv.Viewport
import Tv.Term
import Tv.Render
import Tv.Csv

namespace App

-- | View type: table or freq
inductive ViewType | table | freq (col : Nat) deriving Repr

-- | Single view with table and viewports
structure View where
  table  : Table
  rowVP  : Viewport
  colVP  : Viewport
  vtype  : ViewType := .table
  deriving Repr

-- | App state with view stack
structure State where
  views  : List View      -- head is current, tail is parent stack
  path   : String
  quit   : Bool := false
  deriving Repr

-- | Current view
def State.cur (s : State) : View := s.views.headD ⟨Table.empty, Viewport.create, Viewport.create, .table⟩

-- | Update current view
def State.setCur (s : State) (v : View) : State :=
  { s with views := v :: s.views.tailD [] }

-- | Push new view
def State.push (s : State) (v : View) : State :=
  { s with views := v :: s.views }

-- | Pop view (returns to parent)
def State.pop (s : State) : State :=
  { s with views := s.views.tailD [] }

-- | Initialize state from file
def init (path : String) : IO State := do
  let tbl ← Csv.loadFile path
  let v := { table := tbl, rowVP := Viewport.create, colVP := Viewport.create : View }
  return { views := [v], path := path }

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

-- | Handle key event
def handleKey (s : State) (key : UInt16) (ch : UInt32) (screenH : Nat) : State :=
  let v := s.cur
  let nr := v.table.nRows
  let nc := v.table.nCols
  let pageSize := max 1 (screenH - 2)
  -- movement keys
  if key == Term.keyArrowDown || ch == chJ then
    s.setCur { v with rowVP := v.rowVP.moveRight nr }
  else if key == Term.keyArrowUp || ch == chK then
    s.setCur { v with rowVP := v.rowVP.moveLeft }
  else if key == Term.keyArrowRight || ch == chL then
    s.setCur { v with colVP := v.colVP.moveRight nc }
  else if key == Term.keyArrowLeft || ch == chH then
    s.setCur { v with colVP := v.colVP.moveLeft }
  -- page up/down
  else if key == Term.keyPageDown then
    s.setCur { v with rowVP := v.rowVP.pageDown pageSize nr }
  else if key == Term.keyPageUp then
    s.setCur { v with rowVP := v.rowVP.pageUp pageSize }
  -- home/end (g/G)
  else if key == Term.keyHome || ch == chG then
    s.setCur { v with rowVP := Viewport.goTop }
  else if key == Term.keyEnd || ch == chGG then
    s.setCur { v with rowVP := Viewport.goEnd nr }
  -- freq: push freq view for current column
  else if ch == chF then
    let col := v.colVP.cursor
    let freqTbl := v.table.freq col
    let freqView := { table := freqTbl, rowVP := Viewport.create, colVP := Viewport.create, vtype := .freq col : View }
    s.push freqView
  -- enter: in freq view, filter parent and pop
  else if key == Term.keyEnter then
    match v.vtype with
    | .freq col =>
      let selRow := v.rowVP.cursor
      let selVal := v.table.get selRow 0  -- first column is the value
      let parent := s.pop
      match parent.views with
      | pv :: rest =>
        let filtered := pv.table.filter col selVal
        let newPV := { pv with table := filtered, rowVP := Viewport.create }
        { parent with views := newPV :: rest }
      | [] => s  -- no parent, do nothing
    | .table => s  -- enter in table view does nothing
  -- delete column
  else if ch == chD then
    if nc > 1 then
      let newTbl := v.table.delCol v.colVP.cursor
      let newColVP := if v.colVP.cursor ≥ nc - 1 then v.colVP.moveLeft else v.colVP
      s.setCur { v with table := newTbl, colVP := newColVP }
    else s
  -- quit/pop: pop view or quit if at root
  else if key == Term.keyEsc || ch == chQ then
    if s.views.length > 1 then s.pop
    else { s with quit := true }
  else if ch == chCtrlC then
    { s with quit := true }
  else s

-- | Main event loop
partial def loop (s : State) : IO Unit := do
  if s.quit then return ()
  let v := s.cur
  -- render and get new column offset
  let w ← Term.width
  let h ← Term.height
  let newColOffset ← Render.table v.table v.rowVP v.colVP h.toNat w.toNat
  Render.statusBar s.path v.rowVP.cursor v.colVP.cursor
                   v.table.nRows v.table.nCols (h - 1)
  -- update column offset
  let v := { v with colVP := ⟨v.colVP.cursor, newColOffset⟩ }
  let s := s.setCur v
  -- poll event
  let ev ← Term.pollEvent
  let s' := if ev.type == Term.eventKey then
              handleKey s ev.key ev.ch h.toNat
            else s
  loop s'

-- | Run app
def run (path : String) : IO Unit := do
  let r ← Term.init
  if r < 0 then
    IO.eprintln "Failed to init terminal"
    return
  let s ← init path
  loop s
  Term.shutdown

end App
