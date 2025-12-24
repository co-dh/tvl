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
def init (path : String) (screenH screenW : Nat) : IO State := do
  let tbl ← Csv.loadFile path
  let rowSz := max 1 (screenH - 2)
  let colSz := max 1 (screenW / 15)  -- rough estimate
  return {
    table := tbl
    rowVP := Viewport.create rowSz
    colVP := Viewport.create colSz
    path := path
  }

-- | Handle key event
def handleKey (s : State) (key : UInt16) (ch : UInt32) : State :=
  let nr := s.table.nRows
  let nc := s.table.nCols
  -- movement keys
  if key == Term.keyArrowDown || ch == 'j'.toNat.toUInt32 then
    { s with rowVP := s.rowVP.moveRightBounded nr }
  else if key == Term.keyArrowUp || ch == 'k'.toNat.toUInt32 then
    { s with rowVP := s.rowVP.moveLeftBounded }
  else if key == Term.keyArrowRight || ch == 'l'.toNat.toUInt32 then
    { s with colVP := s.colVP.moveRightBounded nc }
  else if key == Term.keyArrowLeft || ch == 'h'.toNat.toUInt32 then
    { s with colVP := s.colVP.moveLeftBounded }
  -- delete column
  else if ch == 'D'.toNat.toUInt32 then
    if nc > 1 then
      let newTbl := s.table.delCol s.colVP.cursor
      let newColVP := if s.colVP.cursor ≥ nc - 1
                      then s.colVP.moveLeftBounded
                      else s.colVP
      { s with table := newTbl, colVP := newColVP }
    else s
  -- quit
  else if key == Term.keyEsc || ch == 'q'.toNat.toUInt32 then
    { s with quit := true }
  else s

-- | Handle resize event
def handleResize (s : State) (w h : UInt32) : State :=
  let rowSz := max 1 (h.toNat - 2)
  let colSz := max 1 (w.toNat / 15)
  { s with
    rowVP := s.rowVP.resize rowSz
    colVP := s.colVP.resize colSz }

-- | Main event loop
partial def loop (s : State) : IO Unit := do
  if s.quit then return ()
  -- render
  let h ← Term.height
  Render.table s.table s.rowVP s.colVP h.toNat
  Render.statusBar s.path s.rowVP.cursor s.colVP.cursor
                   s.table.nRows s.table.nCols (h - 1)
  -- poll event
  let ev ← Term.pollEvent
  let s' := if ev.type == Term.eventKey then
              handleKey s ev.key ev.ch
            else if ev.type == Term.eventResize then
              handleResize s ev.w ev.h
            else s
  loop s'

-- | Run app
def run (path : String) : IO Unit := do
  let r ← Term.init
  if r < 0 then
    IO.eprintln "Failed to init terminal"
    return
  let w ← Term.width
  let h ← Term.height
  let s ← init path h.toNat w.toNat
  loop s
  Term.shutdown

end App
