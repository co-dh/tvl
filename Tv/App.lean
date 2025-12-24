/-
  Application state and event loop
-/
import Tv.Types
import Tv.Viewport
import Tv.Term
import Tv.Render
import Tv.Csv

namespace App

-- | App state
structure State where
  table  : Table
  rowVP  : Viewport
  colVP  : Viewport
  path   : String
  quit   : Bool := false
  deriving Repr

-- | Initialize state from file
def init (path : String) : IO State := do
  let tbl ← Csv.loadFile path
  return {
    table := tbl
    rowVP := Viewport.create
    colVP := Viewport.create
    path := path
  }

-- | Character codes
def chJ : UInt32 := 106
def chK : UInt32 := 107
def chL : UInt32 := 108
def chH : UInt32 := 104
def chG : UInt32 := 103
def chGG : UInt32 := 71  -- 'G'
def chD : UInt32 := 68   -- 'D'
def chQ : UInt32 := 113  -- 'q'
def chCtrlC : UInt32 := 3  -- Ctrl+C

-- | Handle key event
def handleKey (s : State) (key : UInt16) (ch : UInt32) (screenH : Nat) : State :=
  let nr := s.table.nRows
  let nc := s.table.nCols
  let pageSize := max 1 (screenH - 2)  -- header + status
  -- movement keys
  if key == Term.keyArrowDown || ch == chJ then
    { s with rowVP := s.rowVP.moveRight nr }
  else if key == Term.keyArrowUp || ch == chK then
    { s with rowVP := s.rowVP.moveLeft }
  else if key == Term.keyArrowRight || ch == chL then
    { s with colVP := s.colVP.moveRight nc }
  else if key == Term.keyArrowLeft || ch == chH then
    { s with colVP := s.colVP.moveLeft }
  -- page up/down
  else if key == Term.keyPageDown then
    { s with rowVP := s.rowVP.pageDown pageSize nr }
  else if key == Term.keyPageUp then
    { s with rowVP := s.rowVP.pageUp pageSize }
  -- home/end (g/G)
  else if key == Term.keyHome || ch == chG then
    { s with rowVP := Viewport.goTop }
  else if key == Term.keyEnd || ch == chGG then
    { s with rowVP := Viewport.goEnd nr }
  -- delete column
  else if ch == chD then
    if nc > 1 then
      let newTbl := s.table.delCol s.colVP.cursor
      let newColVP := if s.colVP.cursor ≥ nc - 1
                      then s.colVP.moveLeft
                      else s.colVP
      { s with table := newTbl, colVP := newColVP }
    else s
  -- quit
  else if key == Term.keyEsc || ch == chQ || ch == chCtrlC then
    { s with quit := true }
  else s

-- | Main event loop
partial def loop (s : State) : IO Unit := do
  if s.quit then return ()
  -- render and get new column offset
  let w ← Term.width
  let h ← Term.height
  let newColOffset ← Render.table s.table s.rowVP s.colVP h.toNat w.toNat
  Render.statusBar s.path s.rowVP.cursor s.colVP.cursor
                   s.table.nRows s.table.nCols (h - 1)
  -- update column offset
  let s := { s with colVP := ⟨s.colVP.cursor, newColOffset⟩ }
  -- poll event
  let ev ← Term.pollEvent
  let s' := if ev.type == Term.eventKey then
              handleKey s ev.key ev.ch h.toNat
            else s  -- resize handled automatically via screen queries
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
