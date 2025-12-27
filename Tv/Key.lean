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

-- | Key handler context (view info for handlers, State passed separately)
structure KeyCtx where
  v  : View          -- current view
  di : DisplayInfo   -- display info (colNames, colWidths, nRows, nCols)
  pg : Nat           -- page size (visible rows)
  sw : Nat           -- screen width

-- | Key handler result
abbrev KeyResult := IO State

/-! ## Pure keys (no IO) -/

-- | All pure key operations
inductive PureKey where
  -- navigation
  | j | k | l | h | g | G | zero | one | dollar | ctrlD | ctrlU
  | retMeta (sel : List String) | colJump (idx : DispIdx)
  -- view transforms
  | sortAsc | sortDesc | del | toggleInfo | dup | swap
  | toggleKey | toggleSel | incDec (inc : Bool) | quit | clearSel
  -- views (push new view)
  | freq | lr | pushFilter (expr : String) | selectCols (cols : List String)
  | pushMeta (metaTbl : SomeTable) | pushFreqFilter (expr : String) (parentQuery : Prql.Query)
  | pushFld (path : String) (name : String) | pushSource (cmd : String) | pushFile (path : String)
  -- input modes
  | inputRename | inputCmd
  -- agg
  | pushAgg (keys : List String) (funcs : List Prql.Agg) (cols : List String)
  -- meta view: pop to parent with keyCols
  | popMeta
  -- enter key (pure cases only)
  | ret

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

-- | Get column name at current cursor (display) position
def curColName (c : KeyCtx) : String :=
  let dispCols := Render.displayCols c.v.nav.keyCols c.di.colNames
  dispCols.getDisp c.v.nav.colCur "?"

/-! ## Pure view/state transformations -/

-- | Sort view by column
def sortBy (v : View) (col : String) (asc : Bool) : View :=
  v.copy (query := if asc then v.query.sortAsc col else v.query.sortDesc col)

-- | Delete columns from view (returns none if no cols left)
def delCols (v : View) (di : DisplayInfo) : Option View :=
  let dispCols := Render.displayCols v.nav.keyCols di.colNames
  let curIdx := v.nav.colCur
  let delPos := if v.selCols.isEmpty then [curIdx] else v.selCols
  let delNames := delPos.map fun p => dispCols.getDisp p "?"
  let allDelCols := v.nav.delCols ++ delNames.filter (!v.nav.delCols.contains ·)
  let keepCols := di.colNames.toList.filter (!delNames.contains ·)
  if keepCols.length > 0 then
    let maxCol := keepCols.length - 1
    let nav' := { v.nav with
      colCur := ⟨min curIdx.val maxCol⟩
      colOff := ⟨min v.nav.colOff.val maxCol⟩
      keyCols := v.nav.keyCols.filter (!delNames.contains ·)
      delCols := allDelCols }
    some { v.copy (query := v.query.select keepCols) with
           nav := nav', disp := s!"del {allDelCols.length}", selCols := [] }.invalidate
  else none

-- | Toggle key column(s)
def toggleKeyCols (v : View) (di : DisplayInfo) : View :=
  let dispCols := Render.displayCols v.nav.keyCols di.colNames
  let curName := dispCols.getDisp v.nav.colCur "?"
  let colPos := if v.selCols.isEmpty then [v.nav.colCur] else v.selCols
  let colNames := colPos.map fun p => dispCols.getDisp p "?"
  let allIn := colNames.all v.nav.keyCols.contains
  let newKeys := if allIn then v.nav.keyCols.filter (!colNames.contains ·)
                 else v.nav.keyCols ++ colNames.filter (!v.nav.keyCols.contains ·)
  let newDispCols := Render.displayCols newKeys di.colNames
  match newDispCols.findDispIdx? (· == curName) with
  | some newCur => { v with nav := { v.nav with keyCols := newKeys, colCur := newCur }, selCols := [] }
  | none => v

-- | Toggle column/row selection
def toggleSel (v : View) : View :=
  match v.vkind with
  | .colMeta =>
    let row := v.nav.rowCur
    let newSel := if v.selRows.contains row then v.selRows.filter (· != row) else v.selRows ++ [row]
    { v with selRows := newSel }
  | _ =>
    let col := v.nav.colCur
    let newSel := if v.selCols.contains col then v.selCols.filter (· != col) else v.selCols ++ [col]
    { v with selCols := newSel }

-- | Clear selections
def clearSel (v : View) : Option View :=
  if !v.selCols.isEmpty then some { v with selCols := [] }
  else if !v.selRows.isEmpty then some { v with selRows := [] }
  else none

-- | Adjust decimals
def adjDecimals (v : View) (inc : Bool) : View :=
  let d := if inc then v.decimals + 1 else if v.decimals > 0 then v.decimals - 1 else 0
  { v with decimals := d, cache := none }

-- | Quit or pop view
def quitOrPop (s : State) : State :=
  if s.views.length > 1 then s.pop else { s with quit := true }

-- | Meta view column indices (from Backend.queryMeta schema)
def metaColDist' : Nat := 3   -- distinct count column
def metaColNull' : Nat := 4   -- null% column

-- | Check if null% is 100% (fully null column)
def isFullNull (str : String) : Bool := str == "100%"

-- | Pure: select rows where null% column is "100%"
def selectFullNull (st : SomeTable) : List Nat :=
  (List.range st.nRows).filter fun r =>
    match st.table.getIdx r metaColNull' with
    | .str str => isFullNull str
    | _ => false

-- | Pure: select rows where dist == 1 (single-value cols)
def selectSingleVal (st : SomeTable) : List Nat :=
  (List.range st.nRows).filter fun r =>
    match st.table.getIdx r metaColDist' with
    | .int n => n == 1
    | _ => false

-- | Adjust column offset to keep cursor visible
def adjOff (c : KeyCtx) (nav : PureState) : PureState :=
  let dispCols := Render.displayCols nav.keyCols c.di.colNames
  let off := adjustColOff nav.colOff.val nav.colCur.val dispCols c.di.colNames c.di.colWidths c.sw
  { nav with colOff := ⟨off⟩ }

-- | adjOff only changes colOff, preserves everything else
@[simp] theorem adjOff_rowCur (c : KeyCtx) (nav : PureState) : (adjOff c nav).rowCur = nav.rowCur := rfl
@[simp] theorem adjOff_colCur (c : KeyCtx) (nav : PureState) : (adjOff c nav).colCur = nav.colCur := rfl
@[simp] theorem adjOff_keyCols (c : KeyCtx) (nav : PureState) : (adjOff c nav).keyCols = nav.keyCols := rfl

-- | Pure: pop meta view and set parent's keyCols
def popMetaState (s : State) (selColNames : List String) : State :=
  match s.parents with
  | parent :: rest =>
    let nav' := { parent.nav with keyCols := selColNames, colCur := ⟨0⟩, colOff := ⟨0⟩ }
    let parent' := { parent with nav := nav' }
    { s with curView := parent', parents := rest }
  | [] => s  -- no parent, stay on current

-- | Get column names from selected rows in meta table (col 0 is "name")
def metaSelNames (st : SomeTable) (selRows : List Nat) : List String :=
  selRows.filterMap fun r =>
    match st.table.getIdx r 0 with
    | .str s => some s
    | _ => none

-- | Run pure key: single match on (vkind, key)
def runKey (c : KeyCtx) (key : PureKey) (s : State) : State :=
  let lastRow := if c.di.nRows > 0 then c.di.nRows - 1 else 0
  let lastCol := if c.di.nCols > 0 then c.di.nCols - 1 else 0
  let n := c.v.nav
  match c.v.vkind, key with
  -- colMeta special: 0 selects 100% null, 1 selects single-value
  | .colMeta, .zero => c.v.cache.map (fun st => s.setCur { c.v with selRows := selectFullNull st }) |>.getD s
  | .colMeta, .one => c.v.cache.map (fun st => s.setCur { c.v with selRows := selectSingleVal st }) |>.getD s
  -- navigation: j/k/l/h/g/G/0/$/ctrlD/ctrlU/retMeta/colJump
  | _, .j => s.setCur { c.v with nav := adjOff c { n with rowCur := min (n.rowCur + 1) lastRow } }
  | _, .k => s.setCur { c.v with nav := adjOff c { n with rowCur := n.rowCur - 1 } }
  | _, .l => s.setCur { c.v with nav := adjOff c { n with colCur := ⟨min (n.colCur.val + 1) lastCol⟩ } }
  | _, .h => s.setCur { c.v with nav := adjOff c { n with colCur := ⟨if n.colCur.val > 0 then n.colCur.val - 1 else 0⟩ } }
  | _, .g => s.setCur { c.v with nav := adjOff c { n with rowCur := 0 } }
  | _, .G => s.setCur { c.v with nav := adjOff c { n with rowCur := lastRow } }
  | _, .zero => s.setCur { c.v with nav := adjOff c { n with colCur := ⟨0⟩, colOff := ⟨0⟩ } }
  | _, .one => s  -- non-colMeta: no-op
  | _, .dollar => s.setCur { c.v with nav := adjOff c { n with colCur := ⟨lastCol⟩ } }
  | _, .ctrlD => s.setCur { c.v with nav := adjOff c { n with rowCur := min (n.rowCur + c.pg) lastRow } }
  | _, .ctrlU => s.setCur { c.v with nav := adjOff c { n with rowCur := n.rowCur - min n.rowCur c.pg } }
  | _, .retMeta sel => s.setCur { c.v with nav := adjOff c { n with keyCols := sel, colCur := ⟨0⟩, colOff := ⟨0⟩ } }
  | _, .colJump idx => s.setCur { c.v with nav := adjOff c { n with colCur := idx } }
  -- view transforms
  | _, .sortAsc => s.setCur (sortBy c.v (curColName c) true)
  | _, .sortDesc => s.setCur (sortBy c.v (curColName c) false)
  | _, .del => delCols c.v c.di |>.map s.setCur |>.getD s
  | _, .toggleInfo => { s with showInfo := !s.showInfo }
  | _, .dup => s.dupView
  | _, .swap => s.swapViews
  | _, .toggleKey => s.setCur (toggleKeyCols c.v c.di)
  | _, .toggleSel => s.setCur (toggleSel c.v)
  | _, .incDec inc => s.setCur (adjDecimals c.v inc)
  | _, .quit => quitOrPop s
  | _, .clearSel => clearSel c.v |>.map s.setCur |>.getD s
  -- push views
  | _, .freq => let cols := n.keyCols ++ [curColName c]
                let colStr := String.intercalate "," cols
                s.push ⟨c.v.path, c.v.query.freq cols, s!"freq {colStr}", { keyCols := cols }, .freqV colStr, none, [], [], none, 3⟩
  | _, .lr => s.push ⟨"source:lr:.", {}, "lr ./", {}, .tbl, none, [], [], none, 3⟩
  | _, .pushFilter expr => s.push ⟨c.v.path, c.v.query.filter expr, s!"filter {expr}", {}, .tbl, none, [], [], none, c.v.decimals⟩
  | _, .selectCols cols => if cols.isEmpty then s else s.setCur (c.v.copy (query := c.v.query.select cols))
  | _, .pushMeta metaTbl => s.push ⟨c.v.path, c.v.query, "meta", {}, .colMeta, some metaTbl, [], [], some metaTbl.nRows, 3⟩
  | _, .pushFreqFilter expr pq => s.push ⟨c.v.path, pq.filter expr, s!"filter {expr}", {}, .tbl, none, [], [], none, c.v.decimals⟩
  | _, .pushFld path name => s.push ⟨s!"source:ls:{path}", {}, s!"ls {name}", {}, .tbl, none, [], [], none, 3⟩
  | _, .pushSource cmd => s.push ⟨s!"source:{cmd}", {}, "", {}, .tbl, none, [], [], none, 3⟩
  | _, .pushFile path => s.push ⟨path, {}, "", {}, .tbl, none, [], [], none, 3⟩
  | _, .inputRename => { s with inputMode := .renameTo, inputBuf := "" }
  | _, .inputCmd => { s with inputMode := .command, inputBuf := "" }
  | _, .pushAgg keys funcs cols => s.setCur { c.v with selCols := [] } |>.push ⟨c.v.path, c.v.query.agg keys funcs cols, "agg", {}, .tbl, none, [], [], none, 3⟩
  | _, .popMeta => if c.v.selRows.isEmpty then s
                   else c.v.cache.map (fun st => popMetaState s (metaSelNames st c.v.selRows)) |>.getD s
  -- ret: pure cases (colMeta -> popMeta, others -> no-op for pure, IO handled separately)
  | .colMeta, .ret => if c.v.selRows.isEmpty then s
                      else c.v.cache.map (fun st => popMetaState s (metaSelNames st c.v.selRows)) |>.getD s
  | .tbl, .ret => s  -- plain table: no-op (special sources handled in IO)
  | .fld, .ret => s  -- folder: handled in IO (retFld)
  | .freqV _, .ret => s  -- freqV: handled in IO (retFreq)

namespace Key

-- | Theorems for isFullNull
theorem isFullNull_100 : isFullNull "100%" = true := rfl
theorem isFullNull_0 : isFullNull "0%" = false := rfl
theorem isFullNull_50 : isFullNull "50%" = false := rfl

-- | @ - column jump with fzf
def atSign (c : KeyCtx) (s : State) : KeyResult :=
  fzfIdx ["--prompt=Column: "] (Render.displayCols c.v.nav.keyCols c.di.colNames) s.testMode
    <&> (·.map (fun idx => runKey c (.colJump idx) s) |>.getD s)

-- | Build filter expression from fzf result
def buildFilterExpr (col : String) (vals : List String) (result : String) : String :=
  let lines := result.splitOn "\n" |>.filter (!·.isEmpty)
  let input := lines.headD ""
  let fromHints := (lines.tailD []).filter vals.contains
  if fromHints.length == 1 then s!"{col} == '{fromHints.head!}'"
  else if fromHints.length > 1 then "(" ++ String.intercalate " || " (fromHints.map fun v => s!"{col} == '{v}'") ++ ")"
  else if !input.isEmpty then
    if input.startsWith ">" || input.startsWith "<" || input.startsWith "=" || input.startsWith "~"
    then s!"{col} {input}" else input
  else ""

-- | \ - filter with fzf
def backslash (c : KeyCtx) (s : State) : KeyResult := do
  let col := curColName c
  let vals ← Backend.queryDistinct c.v.query.render c.v.path col |>.map (·.toOption.getD [])
  let prompt := s!"PRQL: {col} == 'x' | > 5 | ~= 'pat' > "
  (← fzf ["--print-query", "--prompt=" ++ prompt] (String.intercalate "\n" vals) s.testMode)
    |>.map (buildFilterExpr col vals) |>.filter (!·.isEmpty)
    |>.map (fun expr => runKey c (.pushFilter expr) s) |>.getD s |> pure

-- | s - select columns
def sel (c : KeyCtx) (s : State) : KeyResult :=
  fzfMulti ["--prompt=Select: "] (c.di.colNames.toList |> String.intercalate "\n") s.testMode
    <&> fun cols => runKey c (.selectCols cols) s

-- | M - meta view (works on any view)
def M (c : KeyCtx) (s : State) : KeyResult :=
  Backend.queryMeta c.v.query.render c.v.path
    <&> fun r => r.toOption.map (fun t => runKey c (.pushMeta t) s) |>.getD s

-- | Build filter expression from cell values
def buildCellFilter (cols : List String) (vals : Array Cell) : String :=
  (List.range cols.length).zip cols |>.map (fun (i, cn) => s!"{cn} == {cellToPrql (vals.getD i .null)}")
    |> String.intercalate " && "

-- | ret on freqV: push filtered view based on selected row
def retFreq (c : KeyCtx) (colNames : String) (s : State) : KeyResult :=
  let cols := colNames.splitOn "," |>.map String.trim
  let pq := s.views.tail?.bind (·.head?) |>.map (·.query) |>.getD {}
  Backend.queryRow c.v.query.render c.v.path c.v.nav.rowCur cols.length
    <&> fun r => r.toOption.map (fun v => runKey c (.pushFreqFilter (buildCellFilter cols v) pq) s) |>.getD s

-- | ret on folder (source:ls): enter folder or open file with bat
def retFld (c : KeyCtx) (s : State) : KeyResult := do
  match ← Backend.queryRow c.v.query.render c.v.path c.v.nav.rowCur 9 with
  | .error _ => pure s
  | .ok vals =>
    let perms := match vals.getD 0 .null with | .str str => str | _ => ""
    let name := match vals.getD 8 .null with | .str str => str | _ => ""
    if name.isEmpty then pure s
    else
      let baseDir := if c.v.path == "source:ls" then "." else c.v.path.drop 10
      let fullPath := if baseDir == "." then name else s!"{baseDir}/{name}"
      if perms.startsWith "d" then pure (runKey c (.pushFld fullPath name) s)
      else runBat fullPath *> pure s

-- | ret on tbl: no-op (could add row details later)
def retTbl (_ : KeyCtx) (s : State) : KeyResult := pure s

-- | Extract string from cell
def cellStr : Cell → Option String | .str s => some s | _ => none

-- | lr schema: 7 columns, path at index 6
def lrColCount : Nat := 7
def lrPathIdx : Nat := 6

-- | ret on lr (source:lr): open file with bat
-- 1. queryRow - fetches current row (7 columns from lr output)
-- 2. toOption - converts Except to Option (discards error)
-- 3. getD lrPathIdx - gets column 6 (file path), cellStr extracts string
-- 4. filter - discards if path is empty
-- 5. runBat - if path exists, open file in bat pager
-- 6. getD (pure s) - if any step failed, return unchanged state
def retLr (c : KeyCtx) (s : State) : KeyResult :=
  Backend.queryRow c.v.query.render c.v.path c.v.nav.rowCur lrColCount >>= fun r =>
    r.toOption.bind (·.getD lrPathIdx .null |> cellStr) |>.filter (!·.isEmpty)
      |>.map (runBat · *> pure s) |>.getD (pure s)

-- | Theorem: j increments rowCur (clamped to lastRow)
theorem runKey_j_rowCur (c : KeyCtx) (s : State) :
    let lastRow := if c.di.nRows > 0 then c.di.nRows - 1 else 0
    (runKey c .j s).curView.nav.rowCur = min (c.v.nav.rowCur + 1) lastRow := by
  simp [runKey, State.setCur]

-- | Theorem: k decrements rowCur (saturating at 0)
theorem runKey_k_rowCur (c : KeyCtx) (s : State) :
    (runKey c .k s).curView.nav.rowCur = c.v.nav.rowCur - 1 := by
  simp [runKey, State.setCur]

-- | Theorem: l increments colCur (clamped to lastCol)
theorem runKey_l_colCur (c : KeyCtx) (s : State) :
    let lastCol := if c.di.nCols > 0 then c.di.nCols - 1 else 0
    (runKey c .l s).curView.nav.colCur.val = min (c.v.nav.colCur.val + 1) lastCol := by
  simp [runKey, State.setCur]

-- | Theorem: retMeta sets keyCols = sel, cursor = 0
theorem runKey_retMeta_cursor (c : KeyCtx) (sel : List String) (s : State) :
    (runKey c (.retMeta sel) s).curView.nav.colCur.val = 0 ∧
    (runKey c (.retMeta sel) s).curView.nav.keyCols = sel := by
  simp [runKey, State.setCur]


-- | ret - enter key (dispatch by ViewKind, IO cases)
def ret (c : KeyCtx) (s : State) : KeyResult :=
  match c.v.vkind with
  | .freqV colNames => retFreq c colNames s
  | .tbl =>
    if c.v.path.startsWith "source:ls" then retFld c s
    else if c.v.path.startsWith "source:lr" then retLr c s
    else pure (runKey c .ret s)
  | .colMeta => pure (runKey c .ret s)
  | .fld => retFld c s

-- | Parse agg function name to Prql.Agg
def parseAgg : String → Option Prql.Agg
  | "count" => some .count | "sum" => some .sum | "average" => some .avg
  | "min" => some .min | "max" => some .max | "stddev" => some .stddev | _ => none

-- | Get agg functions via fzf multi-select
def getAggFuncs (s : State) (keyNames aggNames : List String) : IO (List Prql.Agg) := do
  let keysStr := String.intercalate "," keyNames
  let colsStr := String.intercalate "," aggNames
  let prompt := s!"group \{{keysStr}} (agg \{? {colsStr}}) [Tab=multi]: "
  let names ← fzfMulti ["--prompt=" ++ prompt] "count\nsum\naverage\nmin\nmax\nstddev" s.testMode
  pure (names.filterMap parseAgg)

-- | b - aggregate by key columns
def b (c : KeyCtx) (s : State) : KeyResult := do
  if c.v.nav.keyCols.isEmpty then pure { s with msg := "Set key columns first with !" }
  else
    let dispCols := Render.displayCols c.v.nav.keyCols c.di.colNames
    let keyNames := c.v.nav.keyCols
    let aggPos := if c.v.selCols.isEmpty then [c.v.nav.colCur] else c.v.selCols
    let aggNames := aggPos.map fun p => dispCols.getDisp p "?"
    if aggNames.isEmpty then pure { s with msg := "No columns to aggregate" }
    else
      let funcs ← getAggFuncs s keyNames aggNames
      pure (if funcs.isEmpty then s else runKey c (.pushAgg keyNames funcs aggNames) s)

-- | : - command mode (fzf select source)
def colon (c : KeyCtx) (s : State) : KeyResult :=
  fzf ["--prompt=: "] "ps\nenv\ndf\nls\ntcp" s.testMode
    <&> (·.map (fun cmd => runKey c (.pushSource cmd) s) |>.getD s)

-- | ^ - rename column
def caret (c : KeyCtx) (s : State) : KeyResult :=
  pure (runKey c .inputRename s)

-- | L - load file
def L (c : KeyCtx) (s : State) : KeyResult :=
  fzf ["--prompt=Load: "] "" s.testMode
    <&> (·.map (fun p => runKey c (.pushFile p) s) |>.getD s)

end Key

end App
