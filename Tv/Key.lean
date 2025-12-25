/-
  Key handlers: all key bindings for navigation and commands
-/
import Tv.Types
import Tv.Viewport
import Tv.Render
import Tv.Backend
import Tv.State
import Tv.Fzf

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
def ch0 : UInt32 := 48       -- '0' first column
def chDollar : UInt32 := 36  -- '$' last column
def chCaret : UInt32 := 94   -- '^' rename column
def chDot : UInt32 := 46     -- '.' increase decimals
def chComma : UInt32 := 44   -- ',' decrease decimals

-- | Key handler context (common params for all handlers)
structure KeyCtx where
  s  : State         -- app state
  v  : View          -- current view
  di : DisplayInfo   -- display info (no cell access)
  nr : Nat           -- row count
  nc : Nat           -- col count
  pg : Nat           -- page size

-- | Key handler result
abbrev KeyResult := IO State

namespace Key

-- | j - move down
def j (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := c.v.rowVP.moveRight c.nr })

-- | k - move up
def k (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := c.v.rowVP.moveLeft })

-- | l - move right
def l (c : KeyCtx) : KeyResult := do
  let next := Render.nextInDisplay c.v.keyCols c.nc c.v.colVP.cursor
  pure (c.s.setCur { c.v with colVP := ⟨next, c.v.colVP.offset⟩ })

-- | h - move left
def h (c : KeyCtx) : KeyResult := do
  let prev := Render.prevInDisplay c.v.keyCols c.nc c.v.colVP.cursor
  pure (c.s.setCur { c.v with colVP := ⟨prev, c.v.colVP.offset⟩ })

-- | Ctrl-D - page down
def ctrlD (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := c.v.rowVP.pageDown c.pg c.nr })

-- | Ctrl-U - page up
def ctrlU (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := c.v.rowVP.pageUp c.pg })

-- | g - go to top
def g (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := Viewport.goTop })

-- | G - go to end
def G (c : KeyCtx) : KeyResult := pure (c.s.setCur { c.v with rowVP := Viewport.goEnd c.nr })

-- | 0 - first column
def zero (c : KeyCtx) : KeyResult := do
  let first := Render.displayOrder c.v.keyCols c.nc |>.headD 0
  pure (c.s.setCur { c.v with colVP := ⟨first, 0⟩ })

-- | $ - last column
def dollar (c : KeyCtx) : KeyResult := do
  let last := Render.displayOrder c.v.keyCols c.nc |>.getLast? |>.getD 0
  pure (c.s.setCur { c.v with colVP := ⟨last, c.v.colVP.offset⟩ })

-- | [ - sort ascending
def lbrak (c : KeyCtx) : KeyResult := do
  let colName := c.di.colNames.getD c.v.colVP.cursor "?"
  let prql := c.v.prql ++ " | sort {" ++ colName ++ "}"
  pure (c.s.setCur (c.v.copy (prql := prql)))

-- | ] - sort descending
def rbrak (c : KeyCtx) : KeyResult := do
  let colName := c.di.colNames.getD c.v.colVP.cursor "?"
  let prql := c.v.prql ++ " | sort {-" ++ colName ++ "}"
  pure (c.s.setCur (c.v.copy (prql := prql)))

-- | D - delete column(s)
def D (c : KeyCtx) : KeyResult := do
  let delCols := if c.v.selCols.isEmpty then [c.v.colVP.cursor] else c.v.selCols
  let delNames := delCols.map fun i => c.di.colNames.getD i "?"
  let allCols := c.di.colNames.toList.filter (!delNames.contains ·) |>.map quoteName
  if allCols.length > 0 then
    let selPrql := c.v.prql ++ " | select {" ++ String.intercalate ", " allCols ++ "}"
    let prevDel := if c.v.disp.startsWith "del " then c.v.disp.drop 4 else ""
    let delStr := String.intercalate "," delNames
    let newDisp := if prevDel.isEmpty then s!"del {delStr}" else s!"del {prevDel},{delStr}"
    let newColVP := if c.v.colVP.cursor ≥ c.nc - delCols.length then c.v.colVP.moveLeft else c.v.colVP
    let v' := { c.v.copy (prql := selPrql) with disp := newDisp, colVP := newColVP, selCols := [] }
    pure (c.s.setCur v'.invalidate)
  else pure c.s

-- | @ - column jump with fzf
def atSign (c : KeyCtx) : KeyResult := do
  let colNamesStr := c.di.colNames.toList |> String.intercalate "\n"
  match ← runFzf ["--prompt=Column: "] colNamesStr with
  | some col =>
    match c.di.colNames.toList.findIdx? (· == col) with
    | some idx => pure (c.s.setCur { c.v with colVP := Viewport.goto idx c.nc })
    | none => pure c.s
  | none => pure c.s

-- | \ - filter with fzf
def backslash (c : KeyCtx) : KeyResult := do
  let colName := c.di.colNames.getD c.v.colVP.cursor "?"
  match ← Backend.queryDistinct c.v.prql c.v.path colName with
  | .ok vals =>
    -- prompt shows PRQL examples
    let prompt := s!"PRQL: {colName} == 'x' | > 5 | ~= 'pat' > "
    match ← runFzf ["--print-query", "--prompt=" ++ prompt] (String.intercalate "\n" vals) with
    | some result =>
      -- with --print-query, first line is query, rest are selections
      let lines := result.splitOn "\n" |>.filter (!·.isEmpty)
      let query := lines.headD ""
      let sels := lines.tailD []
      -- check if selections are from hints
      let fromHints := sels.filter vals.contains
      let expr := if fromHints.length == 1 then
        -- single hint → equality
        let v := fromHints.head!
        s!"{colName} == '{v}'"
      else if fromHints.length > 1 then
        -- multiple hints → OR chain
        let clauses := fromHints.map fun v => s!"{colName} == '{v}'"
        "(" ++ String.intercalate " || " clauses ++ ")"
      else if !query.isEmpty then
        -- raw PRQL expression (prepend colName if just operator)
        if query.startsWith ">" || query.startsWith "<" || query.startsWith "=" || query.startsWith "~" then
          s!"{colName} {query}"
        else query
      else ""
      if expr.isEmpty then pure c.s
      else
        let filterPrql := c.v.prql ++ " | filter " ++ expr
        let fv : View := ⟨c.v.path, filterPrql, s!"filter {expr}", Viewport.create, Viewport.create, .tbl, none, [], [], none, c.v.decimals⟩
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
      let quoted := selected.map quoteName
      let selPrql := c.v.prql ++ " | select {" ++ String.intercalate ", " quoted ++ "}"
      pure (c.s.setCur (c.v.copy (prql := selPrql)))
    else pure c.s

-- | M - meta view (works on any view)
def M (c : KeyCtx) : KeyResult := do
  match ← Backend.queryMeta c.v.prql c.v.path with
  | .ok metaTbl =>
    let mv : View := ⟨c.v.path, c.v.prql, "meta", Viewport.create, Viewport.create, .colMeta, some metaTbl, [], [], some metaTbl.nRows, 3⟩
    pure (c.s.push mv)
  | .error e => pure (c.s.setMsg s!"meta error: {e}")

-- | I - toggle info overlay
def I (c : KeyCtx) : KeyResult := pure { c.s with showInfo := !c.s.showInfo }

-- | F - frequency view (works on any view)
def F (c : KeyCtx) : KeyResult := do
  let (cols, colStr) := if c.v.keyCols.isEmpty then
    let name := c.di.colNames.getD c.v.colVP.cursor "?"
    ([name], name)
  else
    let names := c.v.keyCols.map fun i => c.di.colNames.getD i "?"
    (names, String.intercalate "," names)
  let freqPrql := if cols.length == 1 then
    c.v.prql ++ " | freq " ++ cols.head!
  else
    c.v.prql ++ " | group {" ++ colStr ++ "} (aggregate {Cnt = count this}) | derive {Pct = Cnt * 100 / sum Cnt, Bar = s\"repeat('#', CAST({Pct} / 5 AS INTEGER))\"} | sort {-Cnt}"
  let freqKeys := List.range cols.length
  let fv : View := ⟨c.v.path, freqPrql, s!"freq {colStr}", Viewport.create, Viewport.create, .freqV colStr, none, freqKeys, [], none, 3⟩
  pure (c.s.push fv)

-- | ret on freqV: push filtered view based on selected row
def retFreq (c : KeyCtx) (colNames : String) : KeyResult := do
  let cols := colNames.splitOn "," |>.map String.trim
  match ← Backend.queryRow c.v.prql c.v.path c.v.rowVP.cursor cols.length with
  | .error _ => pure c.s
  | .ok vals =>
    let filters := (List.range cols.length).zip cols |>.map fun (i, cn) =>
      let val := vals.getD i .null
      s!"{cn} == {cellToPrql val}"
    let filterExpr := String.intercalate " && " filters
    let parentPrql := match c.s.views.tail? with
      | some (pv :: _) => pv.prql
      | _ => "from df"
    let filterPrql := s!"{parentPrql} | filter {filterExpr}"
    let fv : View := ⟨c.v.path, filterPrql, s!"filter {filterExpr}", Viewport.create, Viewport.create, .tbl, none, [], [], none, c.v.decimals⟩
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
        let lsv : View := ⟨s!"source:ls:{fullPath}", "from df", s!"ls {name}", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
        pure (c.s.push lsv)
      else
        runBat fullPath
        pure c.s

-- | ret on tbl: no-op (could add row details later)
def retTbl (_ : KeyCtx) (s : State) : KeyResult := pure s

-- | ret - enter key (dispatch by ViewKind)
def ret (c : KeyCtx) : KeyResult := do
  match c.v.vkind with
  | .freqV colNames => retFreq c colNames
  | .tbl => if c.v.path.startsWith "source:ls" then retFld c else retTbl c c.s
  | .colMeta => pure c.s  -- TODO: could set key cols from selected
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

-- | Space - toggle column selection
def space (c : KeyCtx) : KeyResult := do
  let col := c.v.colVP.cursor
  let newSel := if c.v.selCols.contains col then c.v.selCols.filter (· != col)
                else c.v.selCols ++ [col]
  pure (c.s.setCur { c.v with selCols := newSel })

-- | b - aggregate by key columns
def b (c : KeyCtx) : KeyResult := do
  if c.v.keyCols.isEmpty then pure { c.s with msg := "Set key columns first with !" }
  else
    let keyNames := c.v.keyCols.map fun i => c.di.colNames.getD i "?"
    let aggCols := if c.v.selCols.isEmpty then [c.v.colVP.cursor] else c.v.selCols
    let aggNames := aggCols.map fun i => c.di.colNames.getD i "?"
    if aggNames.isEmpty then pure { c.s with msg := "No columns to aggregate" }
    else if c.s.testMode then
      -- test mode: use sum as default
      let funcs := ["sum"]
      let aggExprs := funcs.flatMap fun f => aggNames.map fun n => s!"{f}_{n} = {f} {n}"
      let aggPrql := c.v.prql ++ " | group {" ++ String.intercalate ", " keyNames ++
                     "} (aggregate {" ++ String.intercalate ", " aggExprs ++ "})"
      let av : View := ⟨c.v.path, aggPrql, "agg", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
      let s' := c.s.setCur { c.v with selCols := [] }
      pure (s'.push av)
    else
      -- fzf multi-select for agg functions
      let keysStr := String.intercalate "," keyNames
      let colsStr := String.intercalate "," aggNames
      let prompt := "group {" ++ keysStr ++ "} (agg {? " ++ colsStr ++ "}) [Tab=multi]: "
      let funcs ← runFzfMulti ["--prompt=" ++ prompt] "count\nsum\naverage\nmin\nmax\nstddev"
      if funcs.isEmpty then pure c.s
      else
        -- apply each func to each col: func_col = func col
        let aggExprs := funcs.flatMap fun f => aggNames.map fun n => s!"{f}_{n} = {f} {n}"
        let aggPrql := c.v.prql ++ " | group {" ++ String.intercalate ", " keyNames ++
                       "} (aggregate {" ++ String.intercalate ", " aggExprs ++ "})"
        let av : View := ⟨c.v.path, aggPrql, "agg", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
        -- clear selection in current view and push agg view
        let s' := c.s.setCur { c.v with selCols := [] }
        pure (s'.push av)

-- | : - command mode
def colon (c : KeyCtx) : KeyResult := do
  if c.s.testMode then pure { c.s with inputMode := .command, inputBuf := "" }
  else
    match ← runFzf ["--prompt=: "] "ps\nenv\ndf\nls\ntcp" with
    | some cmd =>
      let sv : View := ⟨s!"source:{cmd}", "from df", "", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
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
    let lv : View := ⟨path, "from df", "", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
    pure (c.s.push lv)
  | none => pure c.s

-- | r - list directory
def r (c : KeyCtx) : KeyResult := do
  let rv : View := ⟨"source:ls", "from df", "ls ./", Viewport.create, Viewport.create, .tbl, none, [], [], none, 3⟩
  pure (c.s.push rv)

-- | q - quit/pop
def q (c : KeyCtx) : KeyResult := do
  if c.s.views.length > 1 then pure c.s.pop
  else pure { c.s with quit := true }

-- | Esc - clear selection or pop
def esc (c : KeyCtx) : KeyResult := do
  if !c.v.selCols.isEmpty then pure (c.s.setCur { c.v with selCols := [] })
  else if c.s.views.length > 1 then pure c.s.pop
  else pure { c.s with quit := true }

-- | Ctrl-C - quit
def ctrlC (_ : KeyCtx) (s : State) : KeyResult := pure { s with quit := true }

end Key

end App
