/-
  Key handlers: all key bindings for navigation and commands
-/
import Tv.Types
import Tv.Viewport
import Tv.Render
import Tv.Backend
import Tv.State
import Tv.Fzf
import Tv.Prql

namespace App

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
def chCtrlD : UInt32 := 4  -- Ctrl+D (page down)
def chCtrlU : UInt32 := 21 -- Ctrl+U (page up)
def chLBrack : UInt32 := 91  -- '[' sort asc
def chRBrack : UInt32 := 93  -- ']' sort desc
def chM : UInt32 := 77       -- 'M' meta view
def chAt : UInt32 := 64      -- '@' column jump
def chBackslash : UInt32 := 92 -- '\' filter
def chS : UInt32 := 115      -- 's' select columns
def chI : UInt32 := 73       -- 'I' info box
def chT : UInt32 := 84       -- 'T' duplicate view
def chSS : UInt32 := 83      -- 'S' swap views
def chExcl : UInt32 := 33    -- '!' toggle key column
def chB : UInt32 := 98       -- 'b' aggregate
def chColon : UInt32 := 58   -- ':' command mode
def chLL : UInt32 := 76      -- 'L' load file
def chR : UInt32 := 114      -- 'r' list directory
def chSpace : UInt32 := 32   -- Space toggle selection
def ch0 : UInt32 := 48       -- '0' first column / meta: select null cols
def ch1 : UInt32 := 49       -- '1' meta: select single-value cols
def chDollar : UInt32 := 36  -- '$' last column
def chCaret : UInt32 := 94   -- '^' rename column
def chDot : UInt32 := 46     -- '.' increase decimals
def chComma : UInt32 := 44   -- ',' decrease decimals

-- | Meta view column indices (from Backend.queryMeta schema)
def metaColDist : Nat := 3   -- distinct count column
def metaColNull : Nat := 4   -- null% column

-- | Key handler context (common params for all handlers)
structure KeyCtx where
  s  : State         -- app state
  v  : View          -- current view
  di : DisplayInfo   -- display info (colNames, colWidths, nRows, nCols)
  pg : Nat           -- page size (visible rows)
  sw : Nat           -- screen width

-- | Key handler result
abbrev KeyResult := IO State

/-! ## Pure State Transition with Visibility Proof -/

-- | Pure state for visibility proofs (extract from View what we need)
structure PureState where
  rowCur   : Nat        -- cursor row
  colCur   : Nat        -- cursor column
  colOff   : Nat        -- column offset for scrolling
  keyCols  : List Nat   -- key columns (pinned left)
  nRows    : Nat        -- total rows
  nCols    : Nat        -- total columns

-- | Extract pure state from View
def View.toPure (v : View) (nRows nCols : Nat) : PureState :=
  ⟨v.rowVP.cursor, v.colVP.cursor, v.colVP.offset, v.keyCols, nRows, nCols⟩

-- | Apply pure state back to View
def View.applyPure (v : View) (p : PureState) : View :=
  { v with rowVP := ⟨p.rowCur, v.rowVP.offset⟩,
           colVP := ⟨p.colCur, p.colOff⟩,
           keyCols := p.keyCols }

-- | Navigation keys (pure, no IO)
inductive NavKey where
  | j | k | l | h        -- arrows
  | g | G                -- home/end row
  | zero | dollar        -- first/last col
  | ctrlD | ctrlU        -- page down/up
  | retMeta (sel : List Nat)  -- return from meta with selected cols as keys

-- | Find cursor's position in display order
def displayPos (keyCols : List Nat) (nCols cursor : Nat) : Nat :=
  let order := Render.displayOrder keyCols nCols
  order.findIdx? (· == cursor) |>.getD 0

-- | Count visible columns in display order from offset
def visColCountDisplay (widths : Array Nat) (keyCols : List Nat) (screenW offset : Nat) : Nat :=
  let order := Render.displayOrder keyCols widths.size
  let cols := order.drop offset
  let rec go (cs : List Nat) (w : Nat) (cnt : Nat) : Nat :=
    match cs with
    | [] => cnt
    | i :: rest =>
      let colW := widths.getD i 10 + 1
      if w + colW > screenW then cnt
      else go rest (w + colW) (cnt + 1)
  go cols 0 0

-- | Adjust offset to keep cursor visible (in display order space)
def adjustOffset (colOff cursor : Nat) (keyCols : List Nat) (widths : Array Nat) (screenW : Nat) : Nat :=
  let curPos := displayPos keyCols widths.size cursor
  let visCols := visColCountDisplay widths keyCols screenW colOff
  if curPos < colOff then curPos  -- scroll left: cursor at left edge
  else if curPos >= colOff + visCols then curPos  -- scroll right: cursor at left edge
  else colOff  -- already visible

-- | Theorem: adjustOffset ensures offset ≤ curPos (display position)
theorem adjustOffset_left (colOff cursor : Nat) (keyCols : List Nat) (widths : Array Nat) (screenW : Nat) :
    adjustOffset colOff cursor keyCols widths screenW ≤ displayPos keyCols widths.size cursor := by
  simp only [adjustOffset]
  split
  · omega  -- curPos < colOff: offset = curPos
  · split <;> omega  -- scroll right or keep offset


-- | Pure navigation: handle key and return new state
def handleNav (s : PureState) (key : NavKey) (widths : Array Nat) (screenW visRows : Nat) : PureState :=
  match key with
  | .j => { s with rowCur := min (s.rowCur + 1) (s.nRows - 1) }
  | .k => { s with rowCur := s.rowCur - 1 }  -- saturating sub
  | .l =>
    let next := Render.nextInDisplay s.keyCols s.nCols s.colCur
    let off := adjustOffset s.colOff next s.keyCols widths screenW
    { s with colCur := next, colOff := off }
  | .h =>
    let prev := Render.prevInDisplay s.keyCols s.nCols s.colCur
    let off := adjustOffset s.colOff prev s.keyCols widths screenW
    { s with colCur := prev, colOff := off }
  | .g => { s with rowCur := 0 }
  | .G => { s with rowCur := s.nRows - 1 }
  | .zero => { s with colCur := 0, colOff := 0 }
  | .dollar =>
    let last := s.nCols - 1
    let off := adjustOffset s.colOff last s.keyCols widths screenW
    { s with colCur := last, colOff := off }
  | .ctrlD => { s with rowCur := min (s.rowCur + visRows) (s.nRows - 1) }
  | .ctrlU => { s with rowCur := s.rowCur - min s.rowCur visRows }
  | .retMeta sel =>
    let firstKey := sel.headD 0
    let off := adjustOffset s.colOff firstKey sel widths screenW
    { s with keyCols := sel, colCur := firstKey, colOff := off }

-- | Concrete test: with keyCols [3,4], display order = [3,4,0,1,2]
theorem handleNav_cursorVisible_bug :
    let widths : Array Nat := #[20, 20, 20, 20, 20]
    let keyCols : List Nat := [3, 4]
    let s : PureState := ⟨0, 4, 0, keyCols, 10, 5⟩
    let s' := handleNav s .l widths 45 23
    Render.cursorInCols s'.colCur
      (Render.visibleRange widths s'.colOff s'.colCur 45 keyCols).cols = true := by
  native_decide

def runNav (c : KeyCtx) (key : NavKey) : State :=
  let p := c.v.toPure c.di.nRows c.di.nCols
  let p' := handleNav p key c.di.colWidths c.sw c.pg
  c.s.setCur (c.v.applyPure p')

namespace Key

-- | Navigation handlers use runNav with pure handleNav
def j (c : KeyCtx) : KeyResult := pure (runNav c .j)
def k (c : KeyCtx) : KeyResult := pure (runNav c .k)
def l (c : KeyCtx) : KeyResult := pure (runNav c .l)
def h (c : KeyCtx) : KeyResult := pure (runNav c .h)
def ctrlD (c : KeyCtx) : KeyResult := pure (runNav c .ctrlD)
def ctrlU (c : KeyCtx) : KeyResult := pure (runNav c .ctrlU)
def g (c : KeyCtx) : KeyResult := pure (runNav c .g)
def G (c : KeyCtx) : KeyResult := pure (runNav c .G)

-- | Check if null% is 100% (fully null column)
def isFullNull (s : String) : Bool := s == "100%"

-- | Theorem: "100%" matches isFullNull
theorem isFullNull_100 : isFullNull "100%" = true := rfl

-- | Theorem: "0%" does not match isFullNull
theorem isFullNull_0 : isFullNull "0%" = false := rfl

-- | Theorem: "50%" does not match isFullNull
theorem isFullNull_50 : isFullNull "50%" = false := rfl

-- | Pure: select rows where null% column is "100%"
def selectFullNull (tbl : Table) : List Nat :=
  (List.range tbl.nRows).filter fun r =>
    match tbl.get r metaColNull with
    | .str s => isFullNull s
    | _ => false

-- | Theorem: selectFullNull filters exactly rows with isFullNull (by def)
theorem selectFullNull_def (tbl : Table) :
    selectFullNull tbl = (List.range tbl.nRows).filter fun r =>
      match tbl.get r metaColNull with | .str s => isFullNull s | _ => false := rfl

-- | 0 - first column (meta: select rows with 100% null)
def zero (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .colMeta =>
    match c.v.cache with
    | some tbl => pure (c.s.setCur { c.v with selRows := selectFullNull tbl })
    | none => pure c.s
  | _ => pure (runNav c .zero)

-- | 1 - meta: select rows with dist == 1 (single-value cols)
def one (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .colMeta =>
    match c.v.cache with
    | some tbl =>
      -- select rows where dist column == 1
      let sel := (List.range tbl.nRows).filter fun r =>
        match tbl.get r metaColDist with
        | .int n => n == 1
        | _ => false
      pure (c.s.setCur { c.v with selRows := sel })
    | none => pure c.s
  | _ => pure c.s  -- no-op for non-meta views

-- | $ - last column
def dollar (c : KeyCtx) : KeyResult := pure (runNav c .dollar)

-- | [ - sort ascending
def lbrak (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .colMeta =>  -- sort cached table in-memory
    match c.v.cache with
    | some tbl =>
      let sorted := tbl.sortBy c.v.colVP.cursor true
      pure (c.s.setCur { c.v with cache := some sorted })
    | none => pure c.s
  | _ =>
    let col := c.di.colNames.getD c.v.colVP.cursor "?"
    let prql := (Prql.Query.parse c.v.prql).sortAsc col |>.render
    pure (c.s.setCur (c.v.copy (prql := prql)))

-- | ] - sort descending
def rbrak (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .colMeta =>  -- sort cached table in-memory
    match c.v.cache with
    | some tbl =>
      let sorted := tbl.sortBy c.v.colVP.cursor false
      pure (c.s.setCur { c.v with cache := some sorted })
    | none => pure c.s
  | _ =>
    let col := c.di.colNames.getD c.v.colVP.cursor "?"
    let prql := (Prql.Query.parse c.v.prql).sortDesc col |>.render
    pure (c.s.setCur (c.v.copy (prql := prql)))

-- | D - delete column(s)
def D (c : KeyCtx) : KeyResult := do
  let delCols := if c.v.selCols.isEmpty then [c.v.colVP.cursor] else c.v.selCols
  let delNames := delCols.map fun i => c.di.colNames.getD i "?"
  let keepCols := c.di.colNames.toList.filter (!delNames.contains ·)
  if keepCols.length > 0 then
    let prql := (Prql.Query.parse c.v.prql).select keepCols |>.render
    let prevDel := if c.v.disp.startsWith "del " then c.v.disp.drop 4 else ""
    let delStr := String.intercalate "," delNames
    let newDisp := if prevDel.isEmpty then s!"del {delStr}" else s!"del {prevDel},{delStr}"
    -- Adjust cursor: shift down for each deleted col before it
    let sortedDel := delCols.toArray.qsort (· > ·) |>.toList
    let newCursor := sortedDel.foldl (fun cur d =>
      if d < cur then cur - 1
      else if d == cur then min (cur - 1) (c.di.nCols - delCols.length - 1)  -- deleted cursor col
      else cur) c.v.colVP.cursor
    let newCursor := max 0 (min newCursor (keepCols.length - 1))
    -- Adjust keyCols: remove deleted cols, shift indices down
    let newKeyCols := sortedDel.foldl (fun ks d => Render.adjustKeyCols ks d) c.v.keyCols
    -- Adjust offset: shift down for each deleted col before it
    let newOffset := sortedDel.foldl (fun off d =>
      if d < off then off - 1 else off) c.v.colVP.offset
    let newOffset := min newOffset (keepCols.length - 1)
    let newColVP := ⟨newCursor, newOffset⟩
    let v' := { c.v.copy (prql := prql) with disp := newDisp, colVP := newColVP, selCols := [], keyCols := newKeyCols }
    pure (c.s.setCur v'.invalidate)
  else pure c.s

-- | @ - column jump with fzf
def atSign (c : KeyCtx) : KeyResult := do
  let colNamesStr := c.di.colNames.toList |> String.intercalate "\n"
  match ← runFzf ["--prompt=Column: "] colNamesStr with
  | some col =>
    match c.di.colNames.toList.findIdx? (· == col) with
    | some idx => pure (c.s.setCur { c.v with colVP := Viewport.goto idx c.di.nCols })
    | none => pure c.s
  | none => pure c.s

-- | \ - filter with fzf
def backslash (c : KeyCtx) : KeyResult := do
  let col := c.di.colNames.getD c.v.colVP.cursor "?"
  match ← Backend.queryDistinct c.v.prql c.v.path col with
  | .ok vals =>
    let prompt := s!"PRQL: {col} == 'x' | > 5 | ~= 'pat' > "
    match ← runFzf ["--print-query", "--prompt=" ++ prompt] (String.intercalate "\n" vals) with
    | some result =>
      let lines := result.splitOn "\n" |>.filter (!·.isEmpty)
      let query := lines.headD ""
      let fromHints := (lines.tailD []).filter vals.contains
      -- build filter expression
      let expr := if fromHints.length == 1 then s!"{col} == '{fromHints.head!}'"
        else if fromHints.length > 1 then
          "(" ++ String.intercalate " || " (fromHints.map fun v => s!"{col} == '{v}'") ++ ")"
        else if !query.isEmpty then
          if query.startsWith ">" || query.startsWith "<" || query.startsWith "=" || query.startsWith "~"
          then s!"{col} {query}" else query
        else ""
      if expr.isEmpty then pure c.s
      else
        let prql := (Prql.Query.parse c.v.prql).filter expr |>.render
        let fv : View := ⟨c.v.path, prql, s!"filter {expr}", Viewport.create, Viewport.create, .tbl, none, [], [], [], none, c.v.decimals⟩
        pure (c.s.push fv)
    | none => pure c.s
  | .error _ => pure c.s

-- | s - select columns
def s (c : KeyCtx) : KeyResult := do
  if c.s.testMode then
    pure { c.s with inputMode := .selectCols, inputBuf := "" }
  else
    let colNamesStr := c.di.colNames.toList |> String.intercalate "\n"
    let selected ← runFzfMulti ["--prompt=Select: "] colNamesStr
    if selected.length > 0 then
      let prql := (Prql.Query.parse c.v.prql).select selected |>.render
      pure (c.s.setCur (c.v.copy (prql := prql)))
    else pure c.s

-- | M - meta view (works on any view)
def M (c : KeyCtx) : KeyResult := do
  match ← Backend.queryMeta c.v.prql c.v.path with
  | .ok metaTbl =>
    let mv : View := ⟨c.v.path, c.v.prql, "meta", Viewport.create, Viewport.create, .colMeta, some metaTbl, [], [], [], some metaTbl.nRows, 3⟩
    pure (c.s.push mv)
  | .error e => pure (c.s.setMsg s!"meta error: {e}")

-- | I - toggle info overlay
def I (c : KeyCtx) : KeyResult := pure { c.s with showInfo := !c.s.showInfo }

-- | F - frequency view (works on any view)
def F (c : KeyCtx) : KeyResult := do
  let cols := if c.v.keyCols.isEmpty then [c.di.colNames.getD c.v.colVP.cursor "?"]
              else c.v.keyCols.map fun i => c.di.colNames.getD i "?"
  let colStr := String.intercalate "," cols
  let q := Prql.Query.parse c.v.prql
  let prql := if cols.length == 1 then q.freq cols.head! |>.render else q.freqFull cols |>.render
  let fv : View := ⟨c.v.path, prql, s!"freq {colStr}", Viewport.create, Viewport.create, .freqV colStr, none, List.range cols.length, [], [], none, 3⟩
  pure (c.s.push fv)

-- | ret on freqV: push filtered view based on selected row
def retFreq (c : KeyCtx) (colNames : String) : KeyResult := do
  let cols := colNames.splitOn "," |>.map String.trim
  match ← Backend.queryRow c.v.prql c.v.path c.v.rowVP.cursor cols.length with
  | .error _ => pure c.s
  | .ok vals =>
    let filters := (List.range cols.length).zip cols |>.map fun (i, cn) =>
      s!"{cn} == {cellToPrql (vals.getD i .null)}"
    let expr := String.intercalate " && " filters
    let parentPrql := match c.s.views.tail? with | some (pv :: _) => pv.prql | _ => "from df"
    let prql := (Prql.Query.parse parentPrql).filter expr |>.render
    let fv : View := ⟨c.v.path, prql, s!"filter {expr}", Viewport.create, Viewport.create, .tbl, none, [], [], [], none, c.v.decimals⟩
    pure (c.s.push fv)

-- | ret on folder (source:ls): enter folder or open file with bat
def retFld (c : KeyCtx) : KeyResult := do
  match ← Backend.queryRow c.v.prql c.v.path c.v.rowVP.cursor 9 with
  | .error _ => pure c.s
  | .ok vals =>
    let perms := match vals.getD 0 .null with | .str str => str | _ => ""
    let name := match vals.getD 8 .null with | .str str => str | _ => ""
    if name.isEmpty then pure c.s
    else
      let baseDir := if c.v.path == "source:ls" then "." else c.v.path.drop 10
      let fullPath := if baseDir == "." then name else s!"{baseDir}/{name}"
      if perms.startsWith "d" then
        let lsv : View := ⟨s!"source:ls:{fullPath}", "from df", s!"ls {name}", Viewport.create, Viewport.create, .tbl, none, [], [], [], none, 3⟩
        pure (c.s.push lsv)
      else
        runBat fullPath
        pure c.s

-- | ret on tbl: no-op (could add row details later)
def retTbl (_ : KeyCtx) (s : State) : KeyResult := pure s

-- | ret on lr (source:lr): open file with bat (lr only lists files, not dirs)
def retLr (c : KeyCtx) : KeyResult := do
  match ← Backend.queryRow c.v.prql c.v.path c.v.rowVP.cursor 7 with
  | .error _ => pure c.s
  | .ok vals =>
    let path := match vals.getD 6 .null with | .str s => s | _ => ""
    if path.isEmpty then pure c.s
    else
      runBat path
      pure c.s

-- | Theorem: j preserves row visibility
theorem handleNav_j_rowVisible (s : PureState) (widths : Array Nat) (screenW visRows : Nat) (hH : visRows > 0) :
    let s' := handleNav s .j widths screenW visRows
    Render.rowVisibleP s'.rowCur visRows = true := by
  simp [handleNav]; exact Render.rowVisibleP_always _ _ hH

-- | Theorem: k preserves row visibility
theorem handleNav_k_rowVisible (s : PureState) (widths : Array Nat) (screenW visRows : Nat) (hH : visRows > 0) :
    let s' := handleNav s .k widths screenW visRows
    Render.rowVisibleP s'.rowCur visRows = true := by
  simp [handleNav]; exact Render.rowVisibleP_always _ _ hH

-- | Theorem: l moves to next column in DISPLAY order
theorem handleNav_l_displayOrder (s : PureState) (widths : Array Nat) (screenW visRows : Nat) :
    let s' := handleNav s .l widths screenW visRows
    s'.colCur = Render.nextInDisplay s.keyCols s.nCols s.colCur := by
  simp [handleNav]

-- | Theorem: h moves to prev column in DISPLAY order
theorem handleNav_h_displayOrder (s : PureState) (widths : Array Nat) (screenW visRows : Nat) :
    let s' := handleNav s .h widths screenW visRows
    s'.colCur = Render.prevInDisplay s.keyCols s.nCols s.colCur := by
  simp [handleNav]

-- | Theorem: retMeta sets keyCols = sel, cursor = first key col
theorem handleNav_retMeta_cursor (s : PureState) (sel : List Nat) (widths : Array Nat) (screenW visRows : Nat) :
    let s' := handleNav s (.retMeta sel) widths screenW visRows
    s'.colCur = sel.headD 0 ∧ s'.keyCols = sel := by
  simp [handleNav]

-- | Main theorem: all nav keys preserve row visibility
theorem handleNav_rowVisible (s : PureState) (key : NavKey) (widths : Array Nat) (screenW visRows : Nat) (hH : visRows > 0) :
    let s' := handleNav s key widths screenW visRows
    Render.rowVisibleP s'.rowCur visRows = true := by
  cases key <;> simp [handleNav] <;> exact Render.rowVisibleP_always _ _ hH

-- | Theorem: g preserves row visibility (goes to row 0)
theorem handleNav_g_rowVisible (s : PureState) (widths : Array Nat) (screenW visRows : Nat) (hH : visRows > 0) :
    let s' := handleNav s .g widths screenW visRows
    Render.rowVisibleP s'.rowCur visRows = true := by
  simp [handleNav]; exact Render.rowVisibleP_always 0 _ hH

-- | Theorem: M 0 <ret> sets keyCols = selectFullNull (full null columns)
theorem meta0ret_keyCols (s : PureState) (tbl : Table) (widths : Array Nat) (screenW visRows : Nat) :
    let s' := handleNav s (.retMeta (selectFullNull tbl)) widths screenW visRows
    s'.keyCols = selectFullNull tbl := by
  simp [handleNav]

-- | Theorem: adjustOffset ensures cursor visible in display order
theorem adjustOffset_colVisible (colOff cursor : Nat) (keyCols : List Nat) (widths : Array Nat) (screenW : Nat)
    (hVis : visColCountDisplay widths keyCols screenW (adjustOffset colOff cursor keyCols widths screenW) > 0) :
    Render.cursorInCols cursor
      (Render.visibleRange widths (adjustOffset colOff cursor keyCols widths screenW) cursor screenW keyCols).cols = true := by
  sorry

-- | Theorem: all nav keys keep cursor column visible in display order
theorem handleNav_colVisible (s : PureState) (key : NavKey) (widths : Array Nat) (screenW visRows : Nat)
    (hVisCols : ∀ off, visColCountDisplay widths s.keyCols screenW off > 0) :
    let s' := handleNav s key widths screenW visRows
    Render.cursorInCols s'.colCur
      (Render.visibleRange widths s'.colOff s'.colCur screenW s'.keyCols).cols = true := by
  sorry

-- | Theorem: after handleNav .l, cursor is visible in rendered cols
theorem handleNav_cursorVisible (s : PureState) (widths : Array Nat) (screenW visRows : Nat)
    (hVis : (Render.visibleRange widths (handleNav s .l widths screenW visRows).colOff
             (handleNav s .l widths screenW visRows).colCur screenW s.keyCols).cols.size > 0) :
    Render.cursorInCols (handleNav s .l widths screenW visRows).colCur
      (Render.visibleRange widths (handleNav s .l widths screenW visRows).colOff
       (handleNav s .l widths screenW visRows).colCur screenW s.keyCols).cols = true := by
  sorry

-- | Pure: pop meta view and set parent's keyCols
def popMetaPure (views : List View) (selRows : List Nat) : List View :=
  match views with
  | _ :: parent :: rest =>
    let firstKey := selRows.headD 0
    let parent' := { parent with keyCols := selRows, colVP := ⟨firstKey, 0⟩ }
    parent' :: rest
  | _ => views

-- | Theorem: popMetaPure returns to parent with keyCols = selRows
theorem popMetaPure_keyCols (m parent : View) (rest : List View) (sel : List Nat) :
    let views' := popMetaPure (m :: parent :: rest) sel
    match views'.head? with
    | some v => v.keyCols = sel
    | none => False := by
  simp [popMetaPure]

-- | Theorem: M 0 <ret> returns to parent with keyCols = selectFullNull
theorem meta0ret_parent (m parent : View) (rest : List View) (tbl : Table) :
    let views' := popMetaPure (m :: parent :: rest) (selectFullNull tbl)
    match views'.head? with
    | some v => v.keyCols = selectFullNull tbl
    | none => False := by
  simp [popMetaPure]


-- | ret on colMeta: pop to parent with selected rows as keyCols
def retMeta (c : KeyCtx) : KeyResult := do
  if c.v.selRows.isEmpty then pure c.s
  else pure { c.s with views := popMetaPure c.s.views c.v.selRows }

-- | ret - enter key (dispatch by ViewKind)
def ret (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .freqV colNames => retFreq c colNames
  | .tbl =>
    if c.v.path.startsWith "source:ls" then retFld c
    else if c.v.path.startsWith "source:lr" then retLr c
    else retTbl c c.s
  | .colMeta => retMeta c
  | .fld => retFld c

-- | T - duplicate view
def T (c : KeyCtx) : KeyResult := pure c.s.dupView

-- | S - swap views
def S (c : KeyCtx) : KeyResult := pure c.s.swapViews

-- | ! - toggle key column
def excl (c : KeyCtx) : KeyResult := do
  let cols := if c.v.selCols.isEmpty then [c.v.colVP.cursor] else c.v.selCols
  let allIn := cols.all c.v.keyCols.contains
  let newKeys := if allIn then c.v.keyCols.filter (!cols.contains ·)
                 else c.v.keyCols ++ cols.filter (!c.v.keyCols.contains ·)
  pure (c.s.setCur { c.v with keyCols := newKeys, selCols := [] })

-- | Space - toggle column selection (or row selection in meta view)
def space (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .colMeta =>
    -- In meta view, toggle row selection (for setting keyCols on return)
    let row := c.v.rowVP.cursor
    let newSel := if c.v.selRows.contains row then c.v.selRows.filter (· != row)
                  else c.v.selRows ++ [row]
    pure (c.s.setCur { c.v with selRows := newSel })
  | _ =>
    let col := c.v.colVP.cursor
    let newSel := if c.v.selCols.contains col then c.v.selCols.filter (· != col)
                  else c.v.selCols ++ [col]
    pure (c.s.setCur { c.v with selCols := newSel })

-- | Parse agg function name to Prql.Agg
def parseAgg : String → Option Prql.Agg
  | "count" => some .count | "sum" => some .sum | "average" => some .avg
  | "min" => some .min | "max" => some .max | "stddev" => some .stddev | _ => none

-- | b - aggregate by key columns
def b (c : KeyCtx) : KeyResult := do
  if c.v.keyCols.isEmpty then pure { c.s with msg := "Set key columns first with !" }
  else
    let keyNames := c.v.keyCols.map fun i => c.di.colNames.getD i "?"
    let aggCols := if c.v.selCols.isEmpty then [c.v.colVP.cursor] else c.v.selCols
    let aggNames := aggCols.map fun i => c.di.colNames.getD i "?"
    if aggNames.isEmpty then pure { c.s with msg := "No columns to aggregate" }
    else
      let funcs ← if c.s.testMode then pure [Prql.Agg.sum]
        else do
          let keysStr := String.intercalate "," keyNames
          let colsStr := String.intercalate "," aggNames
          let prompt := s!"group \{{keysStr}} (agg \{? {colsStr}}) [Tab=multi]: "
          let names ← runFzfMulti ["--prompt=" ++ prompt] "count\nsum\naverage\nmin\nmax\nstddev"
          pure (names.filterMap parseAgg)
      if funcs.isEmpty then pure c.s
      else
        let prql := (Prql.Query.parse c.v.prql).agg keyNames funcs aggNames |>.render
        let av : View := ⟨c.v.path, prql, "agg", Viewport.create, Viewport.create, .tbl, none, [], [], [], none, 3⟩
        let s' := c.s.setCur { c.v with selCols := [] }
        pure (s'.push av)

-- | : - command mode
def colon (c : KeyCtx) : KeyResult := do
  if c.s.testMode then pure { c.s with inputMode := .command, inputBuf := "" }
  else
    match ← runFzf ["--prompt=: "] "ps\nenv\ndf\nls\ntcp" with
    | some cmd =>
      let sv : View := ⟨s!"source:{cmd}", "from df", "", Viewport.create, Viewport.create, .tbl, none, [], [], [], none, 3⟩
      pure (c.s.push sv)
    | none => pure c.s

-- | ^ - rename column
def caret (c : KeyCtx) : KeyResult := do
  if c.s.testMode then pure { c.s with inputMode := .renameTo, inputBuf := "" }
  else pure c.s

-- | . - increase decimals
def dot (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with decimals := c.v.decimals + 1, cache := none })

-- | , - decrease decimals
def comma (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with decimals := if c.v.decimals > 0 then c.v.decimals - 1 else 0, cache := none })

-- | L - load file
def L (c : KeyCtx) : KeyResult := do
  match ← runFzf ["--prompt=Load: "] "" with
  | some path =>
    let lv : View := ⟨path, "from df", "", Viewport.create, Viewport.create, .tbl, none, [], [], [], none, 3⟩
    pure (c.s.push lv)
  | none => pure c.s

-- | r - recursive file listing
def r (c : KeyCtx) : KeyResult := do
  let rv : View := ⟨"source:lr:.", "from df", "lr ./", Viewport.create, Viewport.create, .tbl, none, [], [], [], none, 3⟩
  pure (c.s.push rv)

-- | q - quit/pop
def q (c : KeyCtx) : KeyResult := do
  if c.s.views.length > 1 then pure c.s.pop
  else pure { c.s with quit := true }

-- | Esc - clear selections (cols or rows)
def esc (c : KeyCtx) : KeyResult := do
  if !c.v.selCols.isEmpty then pure (c.s.setCur { c.v with selCols := [] })
  else if !c.v.selRows.isEmpty then pure (c.s.setCur { c.v with selRows := [] })
  else pure c.s  -- no-op if nothing selected

-- | Ctrl-C - quit
def ctrlC (_ : KeyCtx) (s : State) : KeyResult := pure { s with quit := true }

end Key

end App
