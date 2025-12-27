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

/-! ## Navigation -/

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

-- | Pure navigation on PureState
-- nRows: total rows in table (for clamping row cursor)
-- nCols: total columns in display order (for clamping col cursor)
-- visRows: visible rows on screen (for Ctrl-D/U page jumps)
def handleNav (s : PureState) (key : NavKey) (nRows nCols visRows : Nat) : PureState :=
  let lastRow := if nRows > 0 then nRows - 1 else 0
  let lastCol := if nCols > 0 then nCols - 1 else 0
  match key with
  | .j => { s with rowCur := min (s.rowCur + 1) lastRow }
  | .k => { s with rowCur := s.rowCur - 1 }  -- saturating
  | .l => { s with colCur := ⟨min (s.colCur.val + 1) lastCol⟩ }
  | .h => { s with colCur := ⟨if s.colCur.val > 0 then s.colCur.val - 1 else 0⟩ }
  | .g => { s with rowCur := 0 }
  | .G => { s with rowCur := lastRow }
  | .zero => { s with colCur := ⟨0⟩, colOff := ⟨0⟩ }
  | .dollar => { s with colCur := ⟨lastCol⟩ }
  | .ctrlD => { s with rowCur := min (s.rowCur + visRows) lastRow }
  | .ctrlU => { s with rowCur := s.rowCur - min s.rowCur visRows }
  | .retMeta sel => { s with keyCols := sel, colCur := ⟨0⟩, colOff := ⟨0⟩ }

-- | Run navigation and adjust offset
def runNav (c : KeyCtx) (key : NavKey) : State :=
  let nav' := handleNav c.v.nav key c.di.nRows c.di.nCols c.pg
  let dispCols := Render.displayCols nav'.keyCols c.di.colNames
  let newOff := adjustColOff nav'.colOff.val nav'.colCur.val dispCols c.di.colNames c.di.colWidths c.sw
  c.s.setCur { c.v with nav := { nav' with colOff := ⟨newOff⟩ } }

-- | Get column name at current cursor (display) position
def curColName (c : KeyCtx) : String :=
  let dispCols := Render.displayCols c.v.nav.keyCols c.di.colNames
  dispCols.getDisp c.v.nav.colCur "?"

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
def selectFullNull (st : SomeTable) : List Nat :=
  (List.range st.nRows).filter fun r =>
    match st.table.getIdx r metaColNull with
    | .str s => isFullNull s
    | _ => false

-- | Theorem: selectFullNull filters exactly rows with isFullNull (by def)
theorem selectFullNull_def (st : SomeTable) :
    selectFullNull st = (List.range st.nRows).filter fun r =>
      match st.table.getIdx r metaColNull with | .str s => isFullNull s | _ => false := rfl

-- | 0 - first column (meta: select rows with 100% null)
def zero (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .colMeta =>
    match c.v.cache with
    | some st => pure (c.s.setCur { c.v with selRows := selectFullNull st })
    | none => pure c.s
  | _ => pure (runNav c .zero)

-- | 1 - meta: select rows with dist == 1 (single-value cols)
def one (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .colMeta =>
    match c.v.cache with
    | some st =>
      let sel := (List.range st.nRows).filter fun r =>
        match st.table.getIdx r metaColDist with
        | .int n => n == 1
        | _ => false
      pure (c.s.setCur { c.v with selRows := sel })
    | none => pure c.s
  | _ => pure c.s  -- no-op for non-meta views

-- | $ - last column
def dollar (c : KeyCtx) : KeyResult := pure (runNav c .dollar)

-- | [ - sort ascending (via PRQL)
def lbrak (c : KeyCtx) : KeyResult := do
  let prql := (Prql.Query.parse c.v.prql).sortAsc (curColName c) |>.render
  pure (c.s.setCur (c.v.copy (prql := prql)))

-- | ] - sort descending (via PRQL)
def rbrak (c : KeyCtx) : KeyResult := do
  let prql := (Prql.Query.parse c.v.prql).sortDesc (curColName c) |>.render
  pure (c.s.setCur (c.v.copy (prql := prql)))

-- | D - delete column(s) using DuckDB EXCLUDE syntax
def D (c : KeyCtx) : KeyResult := do
  let dispCols := Render.displayCols c.v.nav.keyCols c.di.colNames
  let curIdx := c.v.nav.colCur
  let delPos := if c.v.selCols.isEmpty then [curIdx] else c.v.selCols
  let delNames := delPos.map fun p => dispCols.getDisp p "?"
  let allDelCols := c.v.nav.delCols ++ delNames.filter (!c.v.nav.delCols.contains ·)
  let keepCount := c.di.nCols - delNames.length
  if keepCount > 0 then
    let prql := (Prql.Query.new "df").exclude allDelCols |>.render
    let delStr := String.intercalate "," allDelCols
    let newDisp := s!"del {delStr}"
    let newKeyCols := c.v.nav.keyCols.filter (!delNames.contains ·)
    let maxCol := keepCount - 1
    let nav' := { c.v.nav with
      colCur := ⟨min curIdx.val maxCol⟩
      colOff := ⟨min c.v.nav.colOff.val maxCol⟩
      keyCols := newKeyCols
      delCols := allDelCols }
    let v' := { c.v.copy (prql := prql) with nav := nav', disp := newDisp, selCols := [] }
    pure (c.s.setCur v'.invalidate)
  else pure c.s

-- | @ - column jump with fzf (finds DispIdx in display order)
def atSign (c : KeyCtx) : KeyResult := do
  let colNamesStr := c.di.colNames.toList |> String.intercalate "\n"
  match ← runFzf ["--prompt=Column: "] colNamesStr with
  | some col =>
    let dispCols := Render.displayCols c.v.nav.keyCols c.di.colNames
    match dispCols.findDispIdx? (· == col) with
    | some idx => pure (c.s.setCur { c.v with nav := { c.v.nav with colCur := idx } })
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
        let fv : View := ⟨c.v.path, prql, s!"filter {expr}", {}, .tbl, none, [], [], none, c.v.decimals⟩
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
    let mv : View := ⟨c.v.path, c.v.prql, "meta", {}, .colMeta, some metaTbl, [], [], some metaTbl.nRows, 3⟩
    pure (c.s.push mv)
  | .error e => pure (c.s.setMsg s!"meta error: {e}")

-- | I - toggle info overlay
def I (c : KeyCtx) : KeyResult := pure { c.s with showInfo := !c.s.showInfo }

-- | F - frequency view (works on any view)
def F (c : KeyCtx) : KeyResult := do
  let cols := if c.v.nav.keyCols.isEmpty then [curColName c] else c.v.nav.keyCols
  let colStr := String.intercalate "," cols
  let q := Prql.Query.parse c.v.prql
  let prql := if cols.length == 1 then q.freq cols.head! |>.render else q.freqFull cols |>.render
  let nav : PureState := { keyCols := cols }
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
    let fv : View := ⟨c.v.path, prql, s!"filter {expr}", {}, .tbl, none, [], [], none, c.v.decimals⟩
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
        let lsv : View := ⟨s!"source:ls:{fullPath}", "from df", s!"ls {name}", {}, .tbl, none, [], [], none, 3⟩
        pure (c.s.push lsv)
      else
        runBat fullPath
        pure c.s

-- | ret on tbl: no-op (could add row details later)
def retTbl (_ : KeyCtx) (s : State) : KeyResult := pure s

-- | ret on lr (source:lr): open file with bat
def retLr (c : KeyCtx) : KeyResult := do
  match ← Backend.queryRow c.v.prql c.v.path c.v.nav.rowCur 7 with
  | .error _ => pure c.s
  | .ok vals =>
    let path := match vals.getD 6 .null with | .str s => s | _ => ""
    if path.isEmpty then pure c.s
    else
      runBat path
      pure c.s

-- | Theorem: j increments rowCur (clamped to lastRow)
theorem handleNav_j_rowCur (s : PureState) (nRows nCols visRows : Nat) :
    let lastRow := if nRows > 0 then nRows - 1 else 0
    let s' := handleNav s .j nRows nCols visRows
    s'.rowCur = min (s.rowCur + 1) lastRow := by simp [handleNav]

-- | Theorem: k decrements rowCur (saturating at 0)
theorem handleNav_k_rowCur (s : PureState) (nRows nCols visRows : Nat) :
    let s' := handleNav s .k nRows nCols visRows
    s'.rowCur = s.rowCur - 1 := by simp [handleNav]

-- | Theorem: l increments colCur (clamped to lastCol)
theorem handleNav_l_colCur (s : PureState) (nRows nCols visRows : Nat) :
    let lastCol := if nCols > 0 then nCols - 1 else 0
    let s' := handleNav s .l nRows nCols visRows
    s'.colCur.val = min (s.colCur.val + 1) lastCol := by simp [handleNav]

-- | Theorem: retMeta sets keyCols = sel, cursor = 0
theorem handleNav_retMeta_cursor (s : PureState) (sel : List String) (nRows nCols visRows : Nat) :
    let s' := handleNav s (.retMeta sel) nRows nCols visRows
    s'.colCur.val = 0 ∧ s'.keyCols = sel := by simp [handleNav]

-- | Pure: pop meta view and set parent's keyCols
def popMetaState (s : State) (selColNames : List String) : State :=
  match s.parents with
  | parent :: rest =>
    let nav' := { parent.nav with keyCols := selColNames, colCur := ⟨0⟩, colOff := ⟨0⟩ }
    let parent' := { parent with nav := nav' }
    { s with curView := parent', parents := rest }
  | [] => s  -- no parent, stay on current

-- | Theorem: popMetaState returns to parent with keyCols = selColNames
theorem popMetaState_keyCols (cur parent : View) (rest : List View) (sel : List String) :
    let s : State := { curView := cur, parents := parent :: rest }
    let s' := popMetaState s sel
    s'.curView.nav.keyCols = sel := by simp [popMetaState]

-- | Get column names from selected rows in meta table (col 0 is "name")
def metaSelNames (st : SomeTable) (selRows : List Nat) : List String :=
  selRows.filterMap fun r =>
    match st.table.getIdx r 0 with
    | .str s => some s
    | _ => none

-- | ret on colMeta: pop to parent with selected column names as keyCols
def retMeta (c : KeyCtx) : KeyResult := do
  if c.v.selRows.isEmpty then pure c.s
  else
    match c.v.cache with
    | some st =>
      let selNames := metaSelNames st c.v.selRows
      pure (popMetaState c.s selNames)
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
  -- Get cursor column NAME before changing keys
  let curName := dispCols.getDisp c.v.nav.colCur "?"
  -- Convert display positions to column names
  let colPos := if c.v.selCols.isEmpty then [c.v.nav.colCur] else c.v.selCols
  let colNames := colPos.map fun p => dispCols.getDisp p "?"
  let allIn := colNames.all c.v.nav.keyCols.contains
  let newKeys := if allIn then c.v.nav.keyCols.filter (!colNames.contains ·)
                 else c.v.nav.keyCols ++ colNames.filter (!c.v.nav.keyCols.contains ·)
  -- Find cursor's new position in new display order
  let newDispCols := Render.displayCols newKeys c.di.colNames
  match newDispCols.findDispIdx? (· == curName) with
  | some newCur =>
    let nav' := { c.v.nav with keyCols := newKeys, colCur := newCur }
    pure (c.s.setCur { c.v with nav := nav', selCols := [] })
  | none => pure c.s

-- | Space - toggle column selection (or row selection in meta view)
def space (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .colMeta =>
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

-- | b - aggregate by key columns
def b (c : KeyCtx) : KeyResult := do
  if c.v.nav.keyCols.isEmpty then pure { c.s with msg := "Set key columns first with !" }
  else
    let dispCols := Render.displayCols c.v.nav.keyCols c.di.colNames
    let keyNames := c.v.nav.keyCols
    let aggPos := if c.v.selCols.isEmpty then [c.v.nav.colCur] else c.v.selCols
    let aggNames := aggPos.map fun p => dispCols.getDisp p "?"
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
        let av : View := ⟨c.v.path, prql, "agg", {}, .tbl, none, [], [], none, 3⟩
        let s' := c.s.setCur { c.v with selCols := [] }
        pure (s'.push av)

-- | : - command mode
def colon (c : KeyCtx) : KeyResult := do
  if c.s.testMode then pure { c.s with inputMode := .command, inputBuf := "" }
  else
    match ← runFzf ["--prompt=: "] "ps\nenv\ndf\nls\ntcp" with
    | some cmd =>
      let sv : View := ⟨s!"source:{cmd}", "from df", "", {}, .tbl, none, [], [], none, 3⟩
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
    let lv : View := ⟨path, "from df", "", {}, .tbl, none, [], [], none, 3⟩
    pure (c.s.push lv)
  | none => pure c.s

-- | r - recursive file listing
def r (c : KeyCtx) : KeyResult := do
  let rv : View := ⟨"source:lr:.", "from df", "lr ./", {}, .tbl, none, [], [], none, 3⟩
  pure (c.s.push rv)

-- | q - quit/pop
def q (c : KeyCtx) : KeyResult := do
  if c.s.views.length > 1 then pure c.s.pop
  else pure { c.s with quit := true }

-- | Esc - clear selections (cols or rows)
def esc (c : KeyCtx) : KeyResult := do
  if !c.v.selCols.isEmpty then pure (c.s.setCur { c.v with selCols := [] })
  else if !c.v.selRows.isEmpty then pure (c.s.setCur { c.v with selRows := [] })
  else pure c.s

-- | Ctrl-C - quit
def ctrlC (_ : KeyCtx) (s : State) : KeyResult := pure { s with quit := true }

end Key

end App
