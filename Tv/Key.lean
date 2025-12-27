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
  | j | k | l | h | g | G | _0 | _1 | dollar | c_d | c_u
  | colJump (idx : DispIdx)
  -- view transforms
  | asc | desc | D | I | dup | swap
  | bang | spc | incDec (inc : Bool) | quit | esc
  -- views (push new view)
  | freq | lr | pushFilter (expr : String) | selectCols (cols : Array String)
  | pushMeta (metaTbl : SomeTable) | pushFreqFilter (expr : String) (parentQuery : Prql.Query)
  | pushFld (path : String) (name : String) | pushSource (cmd : String) | pushFile (path : String)
  -- input modes
  | inputRename | colon
  -- agg
  | pushAgg (keys : Array String) (funcs : Array Prql.Agg) (cols : Array String)
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

-- | Sort view by key columns + current column (all same direction)
def sortBy (v : View) (col : String) (asc : Bool) : View :=
  let cols := if v.nav.keyCols.contains col then v.nav.keyCols else v.nav.keyCols.push col
  v.copy (query := v.query.pipe (.sort (cols.map (·, asc))))

-- | Get selected columns + current column (unique, cur always last)
def selColNames (v : View) (di : DisplayInfo) : Array String :=
  let dispCols := Render.displayCols v.nav.keyCols di.colNames
  let cur := dispCols.getDisp v.nav.colCur "?"
  v.selCols.filter (· != cur) |>.push cur

-- | Delete columns from view (returns none if no cols left)
def delCols (v : View) (di : DisplayInfo) : Option View :=
  let curIdx := v.nav.colCur
  let delNames := selColNames v di
  let allDelCols := v.nav.delCols ++ delNames.filter (!v.nav.delCols.contains ·)
  let keepCols := di.colNames.filter (!delNames.contains ·)
  if keepCols.size > 0 then
    let maxCol := keepCols.size - 1
    let nav' := { v.nav with
      colCur := ⟨min curIdx.val maxCol⟩
      colOff := ⟨min v.nav.colOff.val maxCol⟩
      keyCols := v.nav.keyCols.filter (!delNames.contains ·)
      delCols := allDelCols }
    some { v.copy (query := v.query.select keepCols) with
           nav := nav', disp := s!"del {allDelCols.size}", selCols := #[] }.invalidate
  else none

-- | Toggle key column(s)
def toggleKeyCols (v : View) (di : DisplayInfo) : View :=
  let dispCols := Render.displayCols v.nav.keyCols di.colNames
  let curName := dispCols.getDisp v.nav.colCur "?"
  let colNames := selColNames v di
  let allIn := colNames.all v.nav.keyCols.contains
  let newKeys := if allIn then v.nav.keyCols.filter (!colNames.contains ·)
                 else v.nav.keyCols ++ colNames.filter (!v.nav.keyCols.contains ·)
  let newDispCols := Render.displayCols newKeys di.colNames
  match newDispCols.findDispIdx? (· == curName) with
  | some newCur => { v with nav := { v.nav with keyCols := newKeys, colCur := newCur }, selCols := #[] }
  | none => v

-- | Toggle column/row selection
def toggleSel (c : KeyCtx) : View :=
  match c.v.vkind with
  | .colMeta => { c.v with selRows := c.v.selRows.toggle c.v.nav.rowCur }
  | _        => { c.v with selCols := c.v.selCols.toggle (curColName c) }

-- | Clear selections
def clearSel (v : View) : Option View :=
  if !v.selCols.isEmpty then some { v with selCols := #[] }
  else if !v.selRows.isEmpty then some { v with selRows := #[] }
  else none

-- | Adjust decimals
def adjDecimals (v : View) (inc : Bool) : View :=
  let d := if inc then v.decimals + 1 else if v.decimals > 0 then v.decimals - 1 else 0
  { v with decimals := d, cache := none }

-- | Quit or pop view
def quitOrPop (s : State) : State :=
  if s.views.size > 1 then s.pop else { s with quit := true }

-- | Meta view column indices (from Backend.queryMeta schema)
def metaColDist' : Nat := 3   -- distinct count column
def metaColNull' : Nat := 4   -- null% column

-- | Select rows where cell at column satisfies predicate
def selectRows (st : SomeTable) (col : Nat) (pred : Cell → Bool) : Array Nat :=
  (Array.range st.nRows).filter fun r => pred (st.table.getIdx r col)

-- | Select 100% null columns
def selectFullNull (st : SomeTable) : Array Nat :=
  selectRows st metaColNull' (·.str?.any (· == "100%"))

-- | Select single-value columns (distinct == 1)
def selectSingleVal (st : SomeTable) : Array Nat :=
  selectRows st metaColDist' (·.int?.any (· == 1))

-- | Adjust column offset to keep cursor visible
def adjOff (c : KeyCtx) (nav : PureState) : PureState :=
  let dispCols := Render.displayCols nav.keyCols c.di.colNames
  let off := adjustColOff nav.colOff.val nav.colCur.val dispCols c.di.colNames c.di.colWidths c.sw
  { nav with colOff := ⟨off⟩ }

-- | adjOff only changes colOff, preserves everything else
@[simp] theorem adjOff_rowCur (c : KeyCtx) (nav : PureState) : (adjOff c nav).rowCur = nav.rowCur := rfl
@[simp] theorem adjOff_colCur (c : KeyCtx) (nav : PureState) : (adjOff c nav).colCur = nav.colCur := rfl
@[simp] theorem adjOff_keyCols (c : KeyCtx) (nav : PureState) : (adjOff c nav).keyCols = nav.keyCols := rfl

-- | Pure: pop meta view and set parent's keyCols + selCols
def popMetaState (s : State) (selColNames : Array String) : State :=
  if h : s.parents.size > 0 then
    let parent := s.parents[0]
    let rest := s.parents.extract 1 s.parents.size
    let nav' := { parent.nav with keyCols := selColNames, colCur := ⟨0⟩, colOff := ⟨0⟩ }
    let parent' := { parent with nav := nav', selCols := selColNames }
    { s with curView := parent', parents := rest }
  else s

-- | Get column names from selected rows in meta table (col 0 is "name")
def metaSelNames (st : SomeTable) (selRows : Array Nat) : Array String :=
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
  | .colMeta, ._0 => c.v.cache.map (fun st => s.setCur { c.v with selRows := selectFullNull st }) |>.getD s
  | .colMeta, ._1 => c.v.cache.map (fun st => s.setCur { c.v with selRows := selectSingleVal st }) |>.getD s
  -- navigation: j/k/l/h/g/G/0/$/ctrlD/ctrlU/retMeta/colJump
  | _, .j => s.setCur { c.v with nav := adjOff c { n with rowCur := min (n.rowCur + 1) lastRow } }
  | _, .k => s.setCur { c.v with nav := adjOff c { n with rowCur := n.rowCur - 1 } }
  | _, .l => s.setCur { c.v with nav := adjOff c { n with colCur := ⟨min (n.colCur.val + 1) lastCol⟩ } }
  | _, .h => s.setCur { c.v with nav := adjOff c { n with colCur := ⟨if n.colCur.val > 0 then n.colCur.val - 1 else 0⟩ } }
  | _, .g => s.setCur { c.v with nav := adjOff c { n with rowCur := 0 } }
  | _, .G => s.setCur { c.v with nav := adjOff c { n with rowCur := lastRow } }
  | _, ._0 => s.setCur { c.v with nav := adjOff c { n with colCur := ⟨0⟩, colOff := ⟨0⟩ } }
  | _, ._1 => s  -- non-colMeta: no-op
  | _, .dollar => s.setCur { c.v with nav := adjOff c { n with colCur := ⟨lastCol⟩ } }
  | _, .c_d => s.setCur { c.v with nav := adjOff c { n with rowCur := min (n.rowCur + c.pg) lastRow } }
  | _, .c_u => s.setCur { c.v with nav := adjOff c { n with rowCur := n.rowCur - min n.rowCur c.pg } }
  | _, .colJump idx => s.setCur { c.v with nav := adjOff c { n with colCur := idx } }
  -- view transforms
  | _, .asc => s.setCur (sortBy c.v (curColName c) true)
  | _, .desc => s.setCur (sortBy c.v (curColName c) false)
  | _, .D => delCols c.v c.di |>.map s.setCur |>.getD s
  | _, .I => { s with showInfo := !s.showInfo }
  | _, .dup => s.dupView
  | _, .swap => s.swapViews
  | _, .bang => s.setCur (toggleKeyCols c.v c.di)
  | _, .spc => s.setCur (toggleSel c)
  | _, .incDec inc => s.setCur (adjDecimals c.v inc)
  | _, .quit => quitOrPop s
  | _, .esc => clearSel c.v |>.map s.setCur |>.getD s
  -- push views (freq: add curCol only if not already in keyCols)
  | _, .freq => let cur := curColName c
                let cols := if n.keyCols.contains cur then n.keyCols else n.keyCols.push cur
                let colStr := cols.join ","
                s.push ⟨c.v.path, c.v.query.freq cols, s!"freq {colStr}", { keyCols := cols }, .freqV colStr, none, #[], #[], none, defDecimals⟩
  | _, .lr => s.push ⟨"source:lr:.", {}, "lr ./", {}, .tbl, none, #[], #[], none, defDecimals⟩
  | _, .pushFilter expr => s.push ⟨c.v.path, c.v.query.filter expr, s!"filter {expr}", {}, .tbl, none, #[], #[], none, c.v.decimals⟩
  | _, .selectCols cols => if cols.isEmpty then s else s.setCur (c.v.copy (query := c.v.query.select cols))
  | _, .pushMeta metaTbl => s.push ⟨c.v.path, c.v.query, "meta", {}, .colMeta, some metaTbl, #[], #[], some metaTbl.nRows, defDecimals⟩
  | _, .pushFreqFilter expr pq => s.push ⟨c.v.path, pq.filter expr, s!"filter {expr}", {}, .tbl, none, #[], #[], none, c.v.decimals⟩
  | _, .pushFld path name => s.push ⟨s!"source:ls:{path}", {}, s!"ls {name}", {}, .tbl, none, #[], #[], none, defDecimals⟩
  | _, .pushSource cmd => s.push ⟨s!"source:{cmd}", {}, "", {}, .tbl, none, #[], #[], none, defDecimals⟩
  | _, .pushFile path => s.push ⟨path, {}, "", {}, .tbl, none, #[], #[], none, defDecimals⟩
  | _, .inputRename => { s with inputMode := .renameTo, inputBuf := "" }
  | _, .colon => { s with inputMode := .command, inputBuf := "" }
  | _, .pushAgg keys funcs cols => s.setCur { c.v with selCols := #[] } |>.push ⟨c.v.path, c.v.query.agg keys funcs cols, "agg", {}, .tbl, none, #[], #[], none, defDecimals⟩
  -- ret: pure cases (colMeta -> pop to parent, others -> no-op for pure, IO handled separately)
  | .colMeta, .ret => if c.v.selRows.isEmpty then s
                      else c.v.cache.map (fun st => popMetaState s (metaSelNames st c.v.selRows)) |>.getD s
  | .tbl, .ret => s  -- plain table: no-op (special sources handled in IO)
  | .fld, .ret => s  -- folder: handled in IO (retFld)
  | .freqV _, .ret => s  -- freqV: handled in IO (retFreq)

namespace Key

-- | @ - column jump with fzf
def atSign (c : KeyCtx) (s : State) : KeyResult :=
  fzfIdx #["--prompt=Column: "] (Render.displayCols c.v.nav.keyCols c.di.colNames) s.testMode
    <&> (·.map (fun idx => runKey c (.colJump idx) s) |>.getD s)

-- | Build filter expression from fzf result
def buildFilterExpr (col : String) (vals : Array String) (result : String) : String :=
  let lines := result.splitOn "\n" |>.filter (!·.isEmpty) |>.toArray
  let input := lines.getD 0 ""
  let fromHints := (lines.extract 1 lines.size).filter vals.contains
  if fromHints.size == 1 then s!"{col} == '{fromHints.getD 0 ""}'"
  else if fromHints.size > 1 then "(" ++ (fromHints.map fun v => s!"{col} == '{v}'").join " || " ++ ")"
  else if !input.isEmpty then
    if input.startsWith ">" || input.startsWith "<" || input.startsWith "=" || input.startsWith "~"
    then s!"{col} {input}" else input
  else ""

-- | \ - filter with fzf
def backslash (c : KeyCtx) (s : State) : KeyResult := do
  let col := curColName c
  let vals ← Backend.queryDistinct c.v.query.render c.v.path col |>.map (·.toOption.getD #[])
  let prompt := s!"PRQL: {col} == 'x' | > 5 | ~= 'pat' > "
  (← fzf #["--print-query", "--prompt=" ++ prompt] (vals.join "\n") s.testMode)
    |>.map (buildFilterExpr col vals) |>.filter (!·.isEmpty)
    |>.map (fun expr => runKey c (.pushFilter expr) s) |>.getD s |> pure

-- | s - select columns
def sel (c : KeyCtx) (s : State) : KeyResult :=
  fzfMulti #["--prompt=Select: "] (c.di.colNames.join "\n") s.testMode
    <&> fun cols => runKey c (.selectCols cols) s

-- | M - meta view (works on any view)
def M (c : KeyCtx) (s : State) : KeyResult :=
  Backend.queryMeta c.v.query.render c.v.path
    <&> fun r => r.toOption.map (fun t => runKey c (.pushMeta t) s) |>.getD s

-- | Build PRQL filter from column names and cell values
-- Purpose: When Enter on freq row, filter parent to matching rows
-- Inputs: cols=#["a","b"], vals=#[.int 1, .str "x"]
-- Steps: mapIdx pairs col name with val, cellToPrql formats value
-- Expected: "a == 1 && b == 'x'"
def buildCellFilter (cols : Array String) (vals : Array Cell) : String :=
  cols.mapIdx (fun i cn => s!"{Prql.quote cn} == {cellToPrql (vals.getD i .null)}")
    |>.toList |> String.intercalate " && "

-- | ret on freqV: push filtered view based on selected row
def retFreq (c : KeyCtx) (colNames : String) (s : State) : KeyResult :=
  let cols := colNames.splitOn "," |>.map String.trim |>.toArray
  let pq := s.parents.getD 0 c.v |>.query  -- parent view's query
  Backend.queryRow c.v.query.render c.v.path c.v.nav.rowCur cols.size
    <&> fun r => r.toOption.map (fun v => runKey c (.pushFreqFilter (buildCellFilter cols v) pq) s) |>.getD s

-- | ret on source (ls/lr): enter directory or open file with bat
def retSource (c : KeyCtx) (s : State) (pfx : String) : KeyResult :=
  let mkPath := fun name => let base := c.v.path.drop pfx.length
                            if base == "." then name else s!"{base}/{name}"
  Backend.queryRow c.v.query.render c.v.path c.v.nav.rowCur srcColCount >>= fun r =>
    r.toOption.bind (fun vals =>
      vals.getD srcColPath .null |>.str?.filter (!·.isEmpty) |>.map fun name =>
        let perms := vals.getD srcColPerms .null |>.str?.getD ""
        if perms.startsWith "d" then pure (runKey c (.pushFld (mkPath name) name) s)
        else runBat (mkPath name) *> pure s
    ) |>.getD (pure s)

-- | ret on folder (source:ls)
def retFld (c : KeyCtx) (s : State) : KeyResult := retSource c s srcLs

-- | ret on lr (source:lr)
def retLr (c : KeyCtx) (s : State) : KeyResult := retSource c s srcLr

-- | ret on tbl: no-op
def retTbl (_ : KeyCtx) (s : State) : KeyResult := pure s

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

-- | Theorem: popMetaState sets cursor = 0 and keyCols = sel
theorem popMetaState_cursor (s : State) (sel : Array String) (h : s.parents.size > 0) :
    (popMetaState s sel).curView.nav.colCur.val = 0 ∧
    (popMetaState s sel).curView.nav.keyCols = sel := by
  simp [popMetaState, h]


-- | ret - enter key (dispatch by ViewKind, IO cases)
def ret (c : KeyCtx) (s : State) : KeyResult :=
  match c.v.vkind with
  | .freqV colNames => retFreq c colNames s
  | .tbl =>
    if c.v.path.startsWith srcLs then retFld c s
    else if c.v.path.startsWith srcLr then retLr c s
    else pure (runKey c .ret s)
  | .colMeta => pure (runKey c .ret s)
  | .fld => retFld c s

-- | Parse agg function name to Prql.Agg
def parseAgg : String → Option Prql.Agg
  | "count" => some .count | "sum" => some .sum | "average" => some .avg
  | "min" => some .min | "max" => some .max | "stddev" => some .stddev | _ => none

-- | Get agg functions via fzf multi-select
def getAggFuncs (s : State) (keyNames aggNames : Array String) : IO (Array Prql.Agg) := do
  let keysStr := keyNames.join ","
  let colsStr := aggNames.join ","
  let prompt := s!"group \{{keysStr}} (agg \{? {colsStr}}) [Tab=multi]: "
  let names ← fzfMulti #["--prompt=" ++ prompt] "count\nsum\naverage\nmin\nmax\nstddev" s.testMode
  pure (names.filterMap parseAgg)

-- | b - aggregate by key columns
def b (c : KeyCtx) (s : State) : KeyResult := do
  if c.v.nav.keyCols.isEmpty then pure { s with msg := "Set key columns first with !" }
  else
    let keyNames := c.v.nav.keyCols
    let aggNames := selColNames c.v c.di
    let funcs ← getAggFuncs s keyNames aggNames
    pure (if funcs.isEmpty then s else runKey c (.pushAgg keyNames funcs aggNames) s)

-- | : - command mode (fzf select source)
def colon (c : KeyCtx) (s : State) : KeyResult :=
  fzf #["--prompt=: "] "ps\nenv\ndf\nls\ntcp" s.testMode
    <&> (·.map (fun cmd => runKey c (.pushSource cmd) s) |>.getD s)

-- | ^ - rename column
def caret (c : KeyCtx) (s : State) : KeyResult :=
  pure (runKey c .inputRename s)

-- | L - load file
def L (c : KeyCtx) (s : State) : KeyResult :=
  fzf #["--prompt=Load: "] "" s.testMode
    <&> (·.map (fun p => runKey c (.pushFile p) s) |>.getD s)

end Key

end App
