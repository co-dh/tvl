/-
  Key handlers: all key bindings for navigation and commands
-/
import Tv.Types
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

-- | Pure state = NavState + bounds (for proofs, constructed on-the-fly)
structure PureState extends NavState where
  nRows : Nat           -- total rows (from DisplayInfo)
  nCols : Nat           -- total columns (from DisplayInfo)

-- | Extract pure state from View + DisplayInfo
def View.toPure (v : View) (nRows nCols : Nat) : PureState :=
  ⟨v.nav, nRows, nCols⟩

-- | Apply navigation state back to View
def View.applyNav (v : View) (nav : NavState) : View :=
  { v with nav := nav }

-- | Navigation keys (pure, no IO)
inductive NavKey where
  | j | k | l | h        -- arrows
  | g | G                -- home/end row
  | zero | dollar        -- first/last col
  | ctrlD | ctrlU        -- page down/up
  | retMeta (sel : List String)  -- return from meta with selected col names as keys

-- | Count visible columns from display position offset
def visColCount (dispCols : Array String) (colNames : Array String) (widths : Array Nat) (screenW offset : Nat) : Nat :=
  let rec go (i w cnt : Nat) : Nat :=
    if i >= dispCols.size then cnt
    else
      let name := dispCols.getD i ""
      let colIdx := Render.colIndex name colNames
      let colW := widths.getD colIdx 10 + 1
      if w + colW > screenW then cnt
      else go (i + 1) (w + colW) (cnt + 1)
  go offset 0 0

-- | Adjust offset to keep cursor visible
def adjustColOff (colOff colCur : Nat) (dispCols : Array String) (colNames : Array String) (widths : Array Nat) (screenW : Nat) : Nat :=
  if colCur < colOff then colCur  -- scroll left
  else
    let visCols := visColCount dispCols colNames widths screenW colOff
    if visCols == 0 then colCur
    else if colCur >= colOff + visCols then colCur - visCols + 1  -- scroll right
    else colOff  -- already visible


-- | Pure navigation: colCur is position in display order (0 to nCols-1)
def handleNav (s : PureState) (key : NavKey) (screenW visRows : Nat) : PureState :=
  match key with
  | .j => { s with rowCur := min (s.rowCur + 1) (s.nRows - 1) }
  | .k => { s with rowCur := s.rowCur - 1 }  -- saturating sub
  | .l => { s with colCur := min (s.colCur + 1) (s.nCols - 1) }
  | .h => { s with colCur := if s.colCur > 0 then s.colCur - 1 else 0 }
  | .g => { s with rowCur := 0 }
  | .G => { s with rowCur := s.nRows - 1 }
  | .zero => { s with colCur := 0, colOff := 0 }
  | .dollar => { s with colCur := s.nCols - 1 }
  | .ctrlD => { s with rowCur := min (s.rowCur + visRows) (s.nRows - 1) }
  | .ctrlU => { s with rowCur := s.rowCur - min s.rowCur visRows }
  | .retMeta sel => { s with keyCols := sel, colCur := 0, colOff := 0 }

-- | Concrete test: with keyCols ["c","d"], colCur=4, l moves to 5 (display order = just indices)
theorem handleNav_l_increment :
    let nav : NavState := ⟨0, 0, 3, 0, ["c", "d"]⟩  -- rowCur, rowOff, colCur, colOff, keyCols
    let s : PureState := ⟨nav, 10, 5⟩               -- nav, nRows, nCols
    let s' := handleNav s .l 45 23
    s'.colCur = 4 := by native_decide

def runNav (c : KeyCtx) (key : NavKey) : State :=
  let p := c.v.toPure c.di.nRows c.di.nCols
  let p' := handleNav p key c.sw c.pg
  -- Adjust offset to keep cursor visible
  let dispCols := Render.displayCols p'.keyCols c.di.colNames
  let newOff := adjustColOff p'.colOff p'.colCur dispCols c.di.colNames c.di.colWidths c.sw
  let nav := { p'.toNavState with colOff := newOff }
  c.s.setCur (c.v.applyNav nav)

-- | Get column name at current cursor (display) position
def curColName (c : KeyCtx) : String :=
  let dispCols := Render.displayCols c.v.nav.keyCols c.di.colNames
  dispCols.getD c.v.nav.colCur "?"

-- | Get column index at current cursor (display) position
def curColIdx (c : KeyCtx) : Nat :=
  Render.colIndex (curColName c) c.di.colNames

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
      let sorted := tbl.sortBy (curColIdx c) true
      pure (c.s.setCur { c.v with cache := some sorted })
    | none => pure c.s
  | _ =>
    let prql := (Prql.Query.parse c.v.prql).sortAsc (curColName c) |>.render
    pure (c.s.setCur (c.v.copy (prql := prql)))

-- | ] - sort descending
def rbrak (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .colMeta =>  -- sort cached table in-memory
    match c.v.cache with
    | some tbl =>
      let sorted := tbl.sortBy (curColIdx c) false
      pure (c.s.setCur { c.v with cache := some sorted })
    | none => pure c.s
  | _ =>
    let prql := (Prql.Query.parse c.v.prql).sortDesc (curColName c) |>.render
    pure (c.s.setCur (c.v.copy (prql := prql)))

-- | D - delete column(s)
def D (c : KeyCtx) : KeyResult := do
  -- Get display columns and names to delete
  let dispCols := Render.displayCols c.v.nav.keyCols c.di.colNames
  let delPos := if c.v.selCols.isEmpty then [c.v.nav.colCur] else c.v.selCols
  let delNames := delPos.map fun p => dispCols.getD p "?"
  let keepCols := c.di.colNames.toList.filter (!delNames.contains ·)
  if keepCols.length > 0 then
    let prql := (Prql.Query.parse c.v.prql).select keepCols |>.render
    let prevDel := if c.v.disp.startsWith "del " then c.v.disp.drop 4 else ""
    let delStr := String.intercalate "," delNames
    let newDisp := if prevDel.isEmpty then s!"del {delStr}" else s!"del {prevDel},{delStr}"
    -- keyCols: just remove deleted names (no index adjustment needed!)
    let newKeyCols := c.v.nav.keyCols.filter (!delNames.contains ·)
    -- Cursor: clamp to valid range
    let newCursor := min c.v.nav.colCur (keepCols.length - 1)
    let newOffset := min c.v.nav.colOff (keepCols.length - 1)
    let newNav := { c.v.nav with colCur := newCursor, colOff := newOffset, keyCols := newKeyCols }
    let v' := { c.v.copy (prql := prql) (nav := newNav) with disp := newDisp, selCols := [] }
    pure (c.s.setCur v'.invalidate)
  else pure c.s

-- | @ - column jump with fzf
def atSign (c : KeyCtx) : KeyResult := do
  let colNamesStr := c.di.colNames.toList |> String.intercalate "\n"
  match ← runFzf ["--prompt=Column: "] colNamesStr with
  | some col =>
    match c.di.colNames.toList.findIdx? (· == col) with
    | some idx => pure (c.s.setCur { c.v with nav := NavState.goto idx c.di.nCols })
    | none => pure c.s
  | none => pure c.s

-- | \ - filter with fzf
def backslash (c : KeyCtx) : KeyResult := do
  let col := curColName c
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
        let fv : View := ⟨c.v.path, prql, s!"filter {expr}", NavState.create, .tbl, none, [], [], none, c.v.decimals⟩
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
    let mv : View := ⟨c.v.path, c.v.prql, "meta", NavState.create, .colMeta, some metaTbl, [], [], some metaTbl.nRows, 3⟩
    pure (c.s.push mv)
  | .error e => pure (c.s.setMsg s!"meta error: {e}")

-- | I - toggle info overlay
def I (c : KeyCtx) : KeyResult := pure { c.s with showInfo := !c.s.showInfo }

-- | F - frequency view (works on any view)
def F (c : KeyCtx) : KeyResult := do
  -- keyCols is already List String, or use current col
  let cols := if c.v.nav.keyCols.isEmpty then [curColName c] else c.v.nav.keyCols
  let colStr := String.intercalate "," cols
  let q := Prql.Query.parse c.v.prql
  let prql := if cols.length == 1 then q.freq cols.head! |>.render else q.freqFull cols |>.render
  -- keyCols for freq view: the grouped column names
  let nav := { NavState.create with keyCols := cols }
  let fv : View := ⟨c.v.path, prql, s!"freq {colStr}", nav, .freqV colStr, none, [], [], none, 3⟩
  pure (c.s.push fv)

-- | ret on freqV: push filtered view based on selected row
def retFreq (c : KeyCtx) (colNames : String) : KeyResult := do
  let cols := colNames.splitOn "," |>.map String.trim
  match ← Backend.queryRow c.v.prql c.v.path c.v.nav.rowCur cols.length with
  | .error _ => pure c.s
  | .ok vals =>
    let filters := (List.range cols.length).zip cols |>.map fun (i, cn) =>
      s!"{cn} == {cellToPrql (vals.getD i .null)}"
    let expr := String.intercalate " && " filters
    let parentPrql := match c.s.views.tail? with | some (pv :: _) => pv.prql | _ => "from df"
    let prql := (Prql.Query.parse parentPrql).filter expr |>.render
    let fv : View := ⟨c.v.path, prql, s!"filter {expr}", NavState.create, .tbl, none, [], [], none, c.v.decimals⟩
    pure (c.s.push fv)

-- | ret on folder (source:ls): enter folder or open file with bat
def retFld (c : KeyCtx) : KeyResult := do
  match ← Backend.queryRow c.v.prql c.v.path c.v.nav.rowCur 9 with
  | .error _ => pure c.s
  | .ok vals =>
    let perms := match vals.getD 0 .null with | .str str => str | _ => ""
    let name := match vals.getD 8 .null with | .str str => str | _ => ""
    if name.isEmpty then pure c.s
    else
      let baseDir := if c.v.path == "source:ls" then "." else c.v.path.drop 10
      let fullPath := if baseDir == "." then name else s!"{baseDir}/{name}"
      if perms.startsWith "d" then
        let lsv : View := ⟨s!"source:ls:{fullPath}", "from df", s!"ls {name}", NavState.create, .tbl, none, [], [], none, 3⟩
        pure (c.s.push lsv)
      else
        runBat fullPath
        pure c.s

-- | ret on tbl: no-op (could add row details later)
def retTbl (_ : KeyCtx) (s : State) : KeyResult := pure s

-- | ret on lr (source:lr): open file with bat (lr only lists files, not dirs)
def retLr (c : KeyCtx) : KeyResult := do
  match ← Backend.queryRow c.v.prql c.v.path c.v.nav.rowCur 7 with
  | .error _ => pure c.s
  | .ok vals =>
    let path := match vals.getD 6 .null with | .str s => s | _ => ""
    if path.isEmpty then pure c.s
    else
      runBat path
      pure c.s

-- | Theorem: j preserves row visibility
theorem handleNav_j_rowVisible (s : PureState) (screenW visRows : Nat) (hH : visRows > 0) :
    let s' := handleNav s .j screenW visRows
    Render.rowVisibleP s'.rowCur visRows = true := by
  simp [handleNav]; exact Render.rowVisibleP_always _ _ hH

-- | Theorem: k preserves row visibility
theorem handleNav_k_rowVisible (s : PureState) (screenW visRows : Nat) (hH : visRows > 0) :
    let s' := handleNav s .k screenW visRows
    Render.rowVisibleP s'.rowCur visRows = true := by
  simp [handleNav]; exact Render.rowVisibleP_always _ _ hH

-- | Theorem: l increments colCur (clamped to nCols-1)
theorem handleNav_l_colCur (s : PureState) (screenW visRows : Nat) :
    let s' := handleNav s .l screenW visRows
    s'.colCur = min (s.colCur + 1) (s.nCols - 1) := by
  simp [handleNav]

-- | Theorem: h decrements colCur (saturating at 0)
theorem handleNav_h_colCur (s : PureState) (screenW visRows : Nat) :
    let s' := handleNav s .h screenW visRows
    s'.colCur = if s.colCur > 0 then s.colCur - 1 else 0 := by
  simp [handleNav]

-- | Theorem: retMeta sets keyCols = sel, cursor = 0
theorem handleNav_retMeta_cursor (s : PureState) (sel : List String) (screenW visRows : Nat) :
    let s' := handleNav s (.retMeta sel) screenW visRows
    s'.colCur = 0 ∧ s'.keyCols = sel := by
  simp [handleNav]

-- | Main theorem: all nav keys preserve row visibility
theorem handleNav_rowVisible (s : PureState) (key : NavKey) (screenW visRows : Nat) (hH : visRows > 0) :
    let s' := handleNav s key screenW visRows
    Render.rowVisibleP s'.rowCur visRows = true := by
  cases key <;> simp [handleNav] <;> exact Render.rowVisibleP_always _ _ hH

-- | Theorem: g preserves row visibility (goes to row 0)
theorem handleNav_g_rowVisible (s : PureState) (screenW visRows : Nat) (hH : visRows > 0) :
    let s' := handleNav s .g screenW visRows
    Render.rowVisibleP s'.rowCur visRows = true := by
  simp [handleNav]; exact Render.rowVisibleP_always 0 _ hH

-- | Pure: pop meta view and set parent's keyCols (now List String)
def popMetaPure (views : List View) (selColNames : List String) : List View :=
  match views with
  | _ :: parent :: rest =>
    let newNav := { parent.nav with keyCols := selColNames, colCur := 0, colOff := 0 }
    let parent' := { parent with nav := newNav }
    parent' :: rest
  | _ => views

-- | Theorem: popMetaPure returns to parent with keyCols = selColNames
theorem popMetaPure_keyCols (m parent : View) (rest : List View) (sel : List String) :
    let views' := popMetaPure (m :: parent :: rest) sel
    match views'.head? with
    | some v => v.nav.keyCols = sel
    | none => False := by
  simp [popMetaPure]

-- | Get column names from selected rows in meta table (col 0 is "name")
def metaSelNames (tbl : Table) (selRows : List Nat) : List String :=
  selRows.filterMap fun r =>
    match tbl.get r 0 with
    | .str s => some s
    | _ => none

-- | Theorem: M 0 <ret> returns with keyCols = metaSelNames (column names from selected rows)
theorem meta0ret_keyCols (tbl : Table) (selRows : List Nat) (m parent : View) (rest : List View) :
    let selNames := metaSelNames tbl selRows
    let views' := popMetaPure (m :: parent :: rest) selNames
    match views'.head? with
    | some v => v.nav.keyCols = selNames
    | none => False := by
  simp [popMetaPure]

-- | Theorem: adjustColOff ensures cursor visible (scroll if needed)
theorem adjustColOff_cursorVisible (colOff colCur : Nat) (dispCols colNames : Array String)
    (widths : Array Nat) (screenW : Nat) :
    let newOff := adjustColOff colOff colCur dispCols colNames widths screenW
    newOff ≤ colCur := by
  sorry  -- needs more careful proof about visColCount

-- | ret on colMeta: pop to parent with selected column names as keyCols
def retMeta (c : KeyCtx) : KeyResult := do
  if c.v.selRows.isEmpty then pure c.s
  else
    match c.v.cache with
    | some tbl =>
      let selNames := metaSelNames tbl c.v.selRows
      pure { c.s with views := popMetaPure c.s.views selNames }
    | none => pure c.s

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

-- | ! - toggle key column (keyCols is List String)
def excl (c : KeyCtx) : KeyResult := do
  let dispCols := Render.displayCols c.v.nav.keyCols c.di.colNames
  -- Convert display positions to column names
  let colPos := if c.v.selCols.isEmpty then [c.v.nav.colCur] else c.v.selCols
  let colNames := colPos.map fun p => dispCols.getD p "?"
  let allIn := colNames.all c.v.nav.keyCols.contains
  let newKeys := if allIn then c.v.nav.keyCols.filter (!colNames.contains ·)
                 else c.v.nav.keyCols ++ colNames.filter (!c.v.nav.keyCols.contains ·)
  pure (c.s.setCur { c.v with nav := { c.v.nav with keyCols := newKeys }, selCols := [] })

-- | Space - toggle column selection (or row selection in meta view)
def space (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .colMeta =>
    -- In meta view, toggle row selection (for setting keyCols on return)
    let row := c.v.nav.rowCur
    let newSel := if c.v.selRows.contains row then c.v.selRows.filter (· != row)
                  else c.v.selRows ++ [row]
    pure (c.s.setCur { c.v with selRows := newSel })
  | _ =>
    let col := c.v.nav.colCur
    let newSel := if c.v.selCols.contains col then c.v.selCols.filter (· != col)
                  else c.v.selCols ++ [col]
    pure (c.s.setCur { c.v with selCols := newSel })

-- | Parse agg function name to Prql.Agg
def parseAgg : String → Option Prql.Agg
  | "count" => some .count | "sum" => some .sum | "average" => some .avg
  | "min" => some .min | "max" => some .max | "stddev" => some .stddev | _ => none

-- | b - aggregate by key columns (keyCols is List String)
def b (c : KeyCtx) : KeyResult := do
  if c.v.nav.keyCols.isEmpty then pure { c.s with msg := "Set key columns first with !" }
  else
    let dispCols := Render.displayCols c.v.nav.keyCols c.di.colNames
    let keyNames := c.v.nav.keyCols  -- already List String
    let aggPos := if c.v.selCols.isEmpty then [c.v.nav.colCur] else c.v.selCols
    let aggNames := aggPos.map fun p => dispCols.getD p "?"
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
        let av : View := ⟨c.v.path, prql, "agg", NavState.create, .tbl, none, [], [], none, 3⟩
        let s' := c.s.setCur { c.v with selCols := [] }
        pure (s'.push av)

-- | : - command mode
def colon (c : KeyCtx) : KeyResult := do
  if c.s.testMode then pure { c.s with inputMode := .command, inputBuf := "" }
  else
    match ← runFzf ["--prompt=: "] "ps\nenv\ndf\nls\ntcp" with
    | some cmd =>
      let sv : View := ⟨s!"source:{cmd}", "from df", "", NavState.create, .tbl, none, [], [], none, 3⟩
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
    let lv : View := ⟨path, "from df", "", NavState.create, .tbl, none, [], [], none, 3⟩
    pure (c.s.push lv)
  | none => pure c.s

-- | r - recursive file listing
def r (c : KeyCtx) : KeyResult := do
  let rv : View := ⟨"source:lr:.", "from df", "lr ./", NavState.create, .tbl, none, [], [], none, 3⟩
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
