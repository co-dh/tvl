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

namespace App

-- | Handle input modes (collecting chars until Enter)
def handleInput (s : State) (v : View) (di : DisplayInfo) (ev : Term.Event) : IO (Option State) := do
  match s.inputMode with
  | .selectCols =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let cols := s.inputBuf.splitOn "," |>.map String.trim |>.filter (!·.isEmpty) |>.toArray
      if cols.size > 0 then
        return some { s.setCur (v.copy (query := v.query.select cols)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
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
  | .filterExpr =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let expr := s.inputBuf.trim
      if !expr.isEmpty then
        return some { s.setCur { v.copy (query := v.query.filter expr) with nav := {} } with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .command =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let cmd := s.inputBuf.trim
      if cmd.startsWith "freq " then
        let cols := (cmd.drop 5).trim.splitOn "," |>.map String.trim |>.toArray
        let nav : PureState := { keyCols := cols }
        let fv : View := ⟨v.path, v.query.freq cols, s!"freq {cols.join ","}", nav, .freqV (cols.join ","), none, #[], #[], none, defDecimals⟩
        return some { s.push fv with inputMode := .none, inputBuf := "" }
      else if cmd.startsWith "lr " then
        let dir := cmd.drop 3 |>.trim
        let lrv : View := ⟨s!"source:lr:{dir}", {}, s!"lr {dir}", {}, .tbl, none, #[], #[], none, defDecimals⟩
        return some { s.push lrv with inputMode := .none, inputBuf := "" }
      else if cmd.startsWith "filter " then
        let expr := cmd.drop 7 |>.trim
        return some { s.setCur { v.copy (query := v.query.filter expr) with nav := {} } with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "", msg := s!"unknown: {cmd}" }
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
  if ev.key == Term.keyArrowDown || ev.ch == chJ then pure (runKey c .j s)
  else if ev.key == Term.keyArrowUp || ev.ch == chK then pure (runKey c .k s)
  else if ev.key == Term.keyArrowRight || ev.ch == chL then pure (runKey c .l s)
  else if ev.key == Term.keyArrowLeft || ev.ch == chH then pure (runKey c .h s)
  else if ev.key == Term.keyPageDown || ev.ch == chCtrlD then pure (runKey c .c_d s)
  else if ev.key == Term.keyPageUp || ev.ch == chCtrlU then pure (runKey c .c_u s)
  else if ev.key == Term.keyHome || ev.ch == chG then pure (runKey c .g s)
  else if ev.key == Term.keyEnd || ev.ch == chGG then pure (runKey c .G s)
  else if ev.ch == ch0 then pure (runKey c ._0 s)
  else if ev.ch == ch1 then pure (runKey c ._1 s)
  else if ev.ch == chDollar then pure (runKey c .dollar s)
  else if ev.ch == chLBrack then pure (runKey c .asc s)
  else if ev.ch == chRBrack then pure (runKey c .desc s)
  else if ev.ch == chD then pure (runKey c .D s)
  else if ev.ch == chAt then Key.atSign c s
  else if ev.ch == chBackslash then Key.backslash c s
  else if ev.ch == chS then Key.sel c s
  else if ev.ch == chM then Key.M c s
  else if ev.ch == chI then pure (runKey c .I s)
  else if ev.ch == chF then pure (runKey c .F s)
  else if ev.key == Term.keyEnter || ev.ch == 13 then Key.ret c s
  else if ev.ch == chT then pure (runKey c .dup s)
  else if ev.ch == chSS then pure (runKey c .swap s)
  else if ev.ch == chExcl then pure (runKey c .bang s)
  else if ev.ch == chSpace then pure (runKey c .spc s)
  else if ev.ch == chB then Key.b c s
  else if ev.ch == chColon then Key.colon c s
  else if ev.ch == chCaret then Key.caret c s
  else if ev.ch == chDot then pure (runKey c (.incDec true) s)
  else if ev.ch == chComma then pure (runKey c (.incDec false) s)
  else if ev.ch == chLL then Key.L c s
  else if ev.ch == chR then pure (runKey c .r s)
  else if ev.ch == chQ then pure (runKey c .q s)
  else if ev.key == Term.keyEsc then pure (runKey c .esc s)
  else if ev.ch == chCtrlC then pure { s with quit := true }
  else pure s

-- | Main event loop
partial def loop (s : State) : IO Unit := do
  if s.quit then return ()
  let v := s.cur
  -- fetch table (uses cache if available)
  let (v', tbl, fetchErr) ← v.fetch
  let s := { s.setCur v' with err := fetchErr }
  -- extract display info (only way to get metadata for handleKey)
  let di := tbl.table.info
  -- render based on view kind (tbl only used here for rendering)
  let w ← Term.width
  let h ← Term.height
  -- render table and overlays (h-3 for data, h-3 for header, h-2 for tab, h-1 for status)
  let (off, cols, keyW) ← Render.table tbl v'.nav (h.toNat - 3) w.toNat v'.decimals v'.selCols v'.selRows
  -- draw header again above tab line (convert display cursor to original index)
  let dispCols := Render.displayCols v'.nav.keyCols di.colNames
  let curColOrig := Render.colIndex (dispCols.getDisp v'.nav.colCur "") di.colNames
  Render.header tbl cols curColOrig (h - 3) v'.selCols
  if !v'.nav.keyCols.isEmpty then Term.print keyW.toUInt32 (h - 3) Term.default Term.default "|"
  let views := s.views.map fun v => (v.path, v.disp, v.query.render)
  Render.tabLine views (h - 2) w.toNat
  Render.statusBar v'.nav.rowCur v'.nav.colCur.val v'.nav.colOff.val (v'.total.getD di.nRows) w.toNat
                   v'.nav.keyCols v'.selCols v'.selRows (h - 1) s.msg s.err
  if s.showInfo then Render.infoOverlay tbl curColOrig v'.nav.rowCur h.toNat w.toNat
  Term.present
  let newColOffset := off
  let v' := { v' with nav := { v'.nav with colOff := ⟨newColOffset⟩ } }
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
  let s' ← if ev.type == Term.eventKey then handleKey s di ev h.toNat w.toNat else pure s
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
  let v : View := ⟨path, {}, "", {}, .tbl, none, #[], #[], none, defDecimals⟩
  let s : State := { curView := v, keys := keys.toList.toArray, testMode := testMode }
  loop s
  Backend.shutdown
  Term.shutdown

end App
