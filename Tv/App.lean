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
      let cols := s.inputBuf.splitOn "," |>.map String.trim |>.filter (!·.isEmpty)
      if cols.length > 0 then
        let prql := (Prql.Query.parse v.prql).select cols |>.render
        return some { s.setCur (v.copy (prql := prql)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .renameTo =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let newName := s.inputBuf.trim
      if !newName.isEmpty then
        let oldName := di.colNames.getDisp v.nav.colCur "?"
        let newCols := di.colNames.toList.map fun n => if n == oldName then newName else n
        let q := Prql.Query.parse v.prql
        let prql := q.derive1 newName (Prql.quote oldName) |>.select newCols |>.render
        return some { s.setCur (v.copy (prql := prql)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .filterExpr =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let expr := s.inputBuf.trim
      if !expr.isEmpty then
        let prql := (Prql.Query.parse v.prql).filter expr |>.render
        return some { s.setCur { v.copy (prql := prql) with nav := {} } with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .command =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let cmd := s.inputBuf.trim
      if cmd.startsWith "freq " then
        let cols := (cmd.drop 5).trim.splitOn "," |>.map String.trim
        let prql := (Prql.Query.parse v.prql).freqFull cols |>.render
        let nav : PureState := { keyCols := cols }
        let fv : View := ⟨v.path, prql, s!"freq {String.intercalate "," cols}", nav, .freqV (String.intercalate "," cols), none, [], [], none, 3⟩
        return some { s.push fv with inputMode := .none, inputBuf := "" }
      else if cmd.startsWith "lr " then
        let dir := cmd.drop 3 |>.trim
        let lrv : View := ⟨s!"source:lr:{dir}", "from df", s!"lr {dir}", {}, .tbl, none, [], [], none, 3⟩
        return some { s.push lrv with inputMode := .none, inputBuf := "" }
      else if cmd.startsWith "filter " then
        let expr := cmd.drop 7 |>.trim
        let prql := (Prql.Query.parse v.prql).filter expr |>.render
        return some { s.setCur { v.copy (prql := prql) with nav := {} } with inputMode := .none, inputBuf := "" }
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
  let c : KeyCtx := ⟨s, v, di, max 1 (screenH - 2), screenW⟩
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
  else if ev.ch == ch1 then Key.one c
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
  let selColIdxs := v'.selCols.map fun d => Render.colIndex (dispCols.getDisp d "") di.colNames
  Render.header tbl cols curColOrig (h - 3) selColIdxs
  if !v'.nav.keyCols.isEmpty then Term.print keyW.toUInt32 (h - 3) Term.default Term.default "|"
  let views := s.views.map fun v => (v.path, v.disp, v.prql)
  Render.tabLine views (h - 2) w.toNat
  Render.statusBar v'.nav.rowCur v'.nav.colCur.val v'.nav.colOff.val (v'.total.getD di.nRows) w.toNat
                   v'.nav.keyCols v'.selCols v'.selRows di.colNames (h - 1) s.msg s.err
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
  let (ev, s) ← match s.keys with
    | c :: rest =>
      let ev : Term.Event := ⟨Term.eventKey, 0, 0, c.toNat.toUInt32, 0, 0⟩
      pure (ev, { s with keys := rest })
    | [] =>
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
  let v : View := ⟨path, "from df", "", {}, .tbl, none, [], [], none, 3⟩
  let s : State := { curView := v, keys := keys.toList, testMode := testMode }
  loop s
  Backend.shutdown
  Term.shutdown

end App
