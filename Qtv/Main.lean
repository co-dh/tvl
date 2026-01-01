/-
  Qtv main: Q-style table viewer
  Usage: qtv <file.csv>
-/
import Qtv.Key
import Tv.Term

open Qtv

-- | Load CSV file into Tbl
def loadCsv (path : String) : IO Tbl := do
  let content ← IO.FS.readFile path
  let lines := content.splitOn "\n" |>.filter (·.length > 0)
  match lines with
  | [] => return Tbl.empty
  | hdr :: rows =>
    let colNames := hdr.splitOn ","
    let data := rows.map (·.splitOn ",")
    let cols := colNames.mapIdx fun i name =>
      let vals := data.map fun row =>
        let v := row.getD i ""
        -- try parse as int, then float, else string
        match v.toInt? with
        | some n => Cell.int n
        | none => match v.toNat? with
          | some n => Cell.int n
          | none => Cell.str v
      ⟨name, vals.toArray⟩
    return ⟨cols.toArray⟩

-- | Map Term.Event to Qtv.Key
def eventToKey (e : Term.Event) : Option Key :=
  if e.type != Term.eventKey then none
  else if e.key == Term.keyArrowDown then some .down
  else if e.key == Term.keyArrowUp then some .up
  else if e.key == Term.keyArrowLeft then some .left
  else if e.key == Term.keyArrowRight then some .right
  else if e.key == Term.keyPageDown then some .pgdn
  else if e.key == Term.keyPageUp then some .pgup
  else if e.ch == Term.ctrlD then some (.ctrl 'D')
  else if e.ch == Term.ctrlU then some (.ctrl 'U')
  else if e.ch > 0 then some (.char (Char.ofNat e.ch.toNat))
  else none

-- | Draw state to terminal
def draw (s : State) : IO Unit := do
  Term.clear
  let cells := s.render
  for c in cells do
    let fg := if c.attr &&& attrBold != 0 then Term.cyan else Term.default
    let bg := if c.attr &&& attrReverse != 0 then Term.blue else Term.default
    Term.print c.x.toUInt32 c.y.toUInt32 fg bg c.txt
  -- status bar
  let sb := s.renderStatus
  Term.print 0 sb.y.toUInt32 Term.black Term.cyan sb.txt
  Term.present

-- | Main loop
partial def loop (s : State) : IO Unit := do
  if s.quit then return
  draw s
  let e ← Term.pollEvent
  match eventToKey e with
  | some k =>
    let (s', req) := s.onKeyInput k
    match req with
    | .none => loop s'
    | _ => loop s'  -- TODO: handle input modes
  | none => loop s

-- | Run the viewer
def run (path : String) : IO Unit := do
  let t ← loadCsv path
  if t.nCols == 0 then
    IO.eprintln s!"Failed to load: {path}"
    return
  let ret ← Term.init
  if ret != 0 then
    IO.eprintln "Failed to init terminal"
    return
  let w ← Term.width
  let h ← Term.height
  let s : State := { gl := { t, typ := path }, yx := (h.toNat, w.toNat) }
  loop s
  Term.shutdown

def main (args : List String) : IO Unit := do
  match args with
  | [path] => run path
  | _ => IO.eprintln "Usage: qtv <file.csv>"
