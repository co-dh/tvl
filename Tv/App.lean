/-
  Application event loop
-/
import Tv.Types
import Tv.Term
import Tv.Render
import Tv.Backend
import Tv.State
import Tv.Fzf
import Tv.Prql
import Tv.Key
import Tv.Error

namespace App

-- | Handle input modes (collecting chars until Enter)
def handleInput (s : State) (v : View) (di : DisplayInfo) (ev : Term.Event) : IO (Option State) := do
  match s.inputMode with
  | .renameTo =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let newName := s.inputBuf.trim
      if !newName.isEmpty then
        let oldName := di.colNames.getDisp v.nav.colCur "?"
        let newCols := di.colNames.map fun n => if n == oldName then newName else n
        let query := v.query.derive1 newName (Prql.quote oldName) |>.select newCols
        return some { s.setCur (v.copy (query := query)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .none => return none  -- not in input mode

-- | Handle key event (takes DisplayInfo, not Table - no cell access)
def handleKey (s : State) (di : DisplayInfo) (ev : Term.Event) (screenH screenW : Nat) : IO State := do
  let v := s.cur
  -- handle input mode first
  match ← handleInput s v di ev with
  | some s' => return s'
  | none =>
  -- build context for key handlers
  let c : KeyCtx := ⟨v, di, max 1 (screenH - 2), screenW, none⟩
  -- dispatch to key handlers
  if      ev.key == Term.keyArrowDown || ev.ch == chJ    then pure (runKey c .j s)
  else if ev.key == Term.keyArrowUp || ev.ch == chK      then pure (runKey c .k s)
  else if ev.key == Term.keyArrowRight || ev.ch == chL   then pure (runKey c .l s)
  else if ev.key == Term.keyArrowLeft || ev.ch == chH    then pure (runKey c .h s)
  else if ev.key == Term.keyPageDown || ev.ch == chCtrlD then pure (runKey c .c_d s)
  else if ev.key == Term.keyPageUp || ev.ch == chCtrlU   then pure (runKey c .c_u s)
  else if ev.key == Term.keyHome || ev.ch == chG         then pure (runKey c .g s)
  else if ev.key == Term.keyEnd || ev.ch == chGG         then pure (runKey c .G s)
  else if ev.ch == ch0                                   then pure (runKey c ._0 s)
  else if ev.ch == ch1                                   then pure (runKey c ._1 s)
  else if ev.ch == chDollar                              then pure (runKey c .dollar s)
  else if ev.ch == chLBrack                              then pure (runKey c .asc s)
  else if ev.ch == chRBrack                              then pure (runKey c .desc s)
  else if ev.ch == chD                                   then pure (runKey c .D s)
  else if ev.ch == chI                                   then pure (runKey c .I s)
  else if ev.ch == chF                                   then pure (runKey c .F s)
  else if ev.ch == chT                                   then pure (runKey c .dup s)
  else if ev.ch == chSS                                  then pure (runKey c .swap s)
  else if ev.ch == chExcl                                then pure (runKey c .bang s)
  else if ev.ch == chSpace                               then pure (runKey c .spc s)
  else if ev.ch == chDot                                 then pure (runKey c (.incDec true) s)
  else if ev.ch == chComma                               then pure (runKey c (.incDec false) s)
  else if ev.ch == chQ                                   then pure (runKey c .q s)
  else if ev.key == Term.keyEsc                          then pure (runKey c .esc s)
  else if ev.ch == chCtrlC                               then pure { s with quit := true }
  else if ev.ch == chR                                   then Source.r s
  else if ev.ch == chRR                                  then Source.R s
  else if ev.ch == chAt                                  then Key.atSign c s
  else if ev.ch == chBackslash                           then Key.backslash c s
  else if ev.ch == chS                                   then Key.s c s
  else if ev.ch == chM                                   then Key.M c s
  else if ev.ch == chm                                   then Key.m c s
  else if ev.key == Term.keyEnter || ev.ch == 13         then Key.ret c s
  else if ev.ch == chB                                   then Key.b c s
  else if ev.ch == chColon                               then Key.colon c s
  else if ev.ch == chCaret                               then Key.caret c s
  else if ev.ch == chLL                                  then Key.L c s
  else pure s

-- | Main event loop
partial def loop (s : State) : IO Unit := do
  if s.quit then return ()
  let v := s.cur
  -- fetch table (uses cache if available)
  let (v', tbl, fetchErr) ← v.fetch
  let s := { s.setCur v' with err := fetchErr }
  -- extract display info (for handleKey metadata)
  let di := tbl.info
  -- render and update colOff
  let off ← Render.all v' s tbl di
  let v' := { v' with nav := { v'.nav with colOff := ⟨off⟩ } }
  let s := s.setCur v'
  -- test mode: exit after keys consumed
  if s.testMode && s.keys.isEmpty then
    let buf ← Term.bufferStr
    IO.print buf
    Term.shutdown
    return ()
  -- get next event: from buffer or poll
  let (ev, s) ← if h : s.keys.size > 0 then
    let c := s.keys[0]
    let ev : Term.Event := ⟨Term.eventKey, 0, 0, c.toNat.toUInt32, 0, 0⟩
    pure (ev, { s with keys := s.keys.extract 1 s.keys.size })
  else
    let ev ← Term.pollEvent
    pure (ev, s)
  -- handleKey gets DisplayInfo only - no cell access possible
  let w ← Term.width; let h ← Term.height
  let s' ← if ev.type == Term.eventKey then handleKey s di ev h.toNat w.toNat else pure s
  -- pop any errors to status bar
  let err ← Error.pop
  let s' := if err.isEmpty then s' else { s' with err := err }
  loop s'

-- | Run app with optional replay keys
def run (path : String) (keys : String := "") (testMode : Bool := false) : IO Unit := do
  -- init backend before terminal (debug output goes to normal screen)
  let ok ← Backend.init
  if !ok then
    Error.set "Failed to init backend"
    return
  let r ← Term.init
  if r < 0 then
    Error.set "Failed to init terminal"
    return
  let v : View := ⟨path, { base := Source.fromExpr path }, "", {}, .tbl, none, #[], #[], none, defDecimals⟩
  let s : State := { curView := v, keys := keys.toList.toArray, testMode := testMode }
  loop s
  Backend.shutdown
  Term.shutdown

end App
