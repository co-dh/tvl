/-
  Application event loop
-/
import Tv.Types
import Tv.Viewport
import Tv.Term
import Tv.Render
import Tv.Backend
import Tv.State
import Tv.Fzf
import Tv.Key

namespace App

-- | Handle input modes (collecting chars until Enter)
def handleInput (s : State) (v : View) (di : DisplayInfo) (ev : Term.Event) : IO (Option State) := do
  match s.inputMode with
  | .selectCols =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let cols := s.inputBuf.splitOn "," |>.map String.trim |>.filter (!·.isEmpty)
      if cols.length > 0 then
        let quoted := cols.map quoteName
        let selPrql := v.prql ++ " | select {" ++ String.intercalate ", " quoted ++ "}"
        return some { s.setCur (v.copy (prql := selPrql)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .renameTo =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let newName := s.inputBuf.trim
      if !newName.isEmpty then
        let oldName := di.colNames.getD v.colVP.cursor "?"
        let allCols := di.colNames.toList.map fun n => if n == oldName then quoteName newName else quoteName n
        let renamePrql := v.prql ++ " | derive {" ++ quoteName newName ++ " = " ++ quoteName oldName ++
                          "} | select {" ++ String.intercalate ", " allCols ++ "}"
        return some { s.setCur (v.copy (prql := renamePrql)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .filterExpr =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let expr := s.inputBuf.trim
      if !expr.isEmpty then
        let filterPrql := v.prql ++ " | filter " ++ expr
        return some { s.setCur (v.copy (prql := filterPrql) (rowVP := Viewport.create)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .command =>
    if ev.key == Term.keyEnter || ev.ch == 13 then
      let cmd := s.inputBuf.trim
      if cmd.startsWith "freq " then
        let cols := cmd.drop 5 |>.trim
        let colList := cols.splitOn "," |>.map String.trim
        let freqPrql := v.prql ++ " | group {" ++ cols ++ "} (aggregate {Cnt = count this}) | derive {Pct = Cnt * 100 / sum Cnt, Bar = s\"repeat('#', CAST({Pct} / 5 AS INTEGER))\"} | sort {-Cnt}"
        let fv : View := ⟨v.path, freqPrql, s!"freq {cols}", Viewport.create, Viewport.create, .freqV cols, none, List.range colList.length, [], none, 3⟩
        return some { s.push fv with inputMode := .none, inputBuf := "" }
      else if cmd.startsWith "lr " then
        let dir := cmd.drop 3 |>.trim
        let lrv : View := ⟨s!"source:lr:{dir}", "from df", s!"lr {dir}", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
        return some { s.push lrv with inputMode := .none, inputBuf := "" }
      else if cmd.startsWith "filter " then
        let expr := cmd.drop 7 |>.trim
        let filterPrql := v.prql ++ " | filter " ++ expr
        return some { s.setCur (v.copy (prql := filterPrql) (rowVP := Viewport.create)) with inputMode := .none, inputBuf := "" }
      else return some { s with inputMode := .none, inputBuf := "", msg := s!"unknown: {cmd}" }
    else if ev.ch > 0 then return some { s with inputBuf := s.inputBuf.push (Char.ofNat ev.ch.toNat) }
    else return some s
  | .none => return none  -- not in input mode

-- | Handle key event (takes DisplayInfo, not Table - no cell access)
def handleKey (s : State) (di : DisplayInfo) (ev : Term.Event) (screenH : Nat) : IO State := do
  let v := s.cur
  -- handle input mode first
  match ← handleInput s v di ev with
  | some s' => return s'
  | none =>
  -- build context for key handlers
  let c : KeyCtx := ⟨s, v, di, di.nRows, di.nCols, max 1 (screenH - 2)⟩
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
  -- render table and overlays (h-3 for data, h-3 for header, h-2 for tab, h-1 for status)
  let (off, cols, keyW) ← Render.table tbl v'.rowVP v'.colVP (h.toNat - 3) w.toNat v'.keyCols v'.decimals v'.selCols
  -- draw header again above tab line
  Render.header tbl cols v'.colVP.cursor (h - 3) v'.selCols
  if !v'.keyCols.isEmpty then Term.print keyW.toUInt32 (h - 3) Term.white Term.black "|"
  let disps := s.views.map fun v => (v.disp, v.prql)
  Render.tabLine v'.path disps (h - 2)
  Render.statusBar v'.rowVP.cursor (v'.total.getD di.nRows) w.toNat
                   v'.keyCols v'.selCols di.colNames (h - 1) s.msg
  if s.showInfo then Render.infoOverlay tbl v'.colVP.cursor v'.rowVP.cursor h.toNat w.toNat
  Term.present
  let newColOffset := off
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
