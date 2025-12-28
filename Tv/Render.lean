/-
  Table rendering to terminal
-/
import Tv.Types
import Tv.State
import Tv.Term
import Tv.Source

open App

namespace Render

-- | Style indices (match C STYLE_* defines)
-- 0: cursor, 1: sel row, 2: sel col+cur row, 3: sel col, 4: cur row, 5: cur col, 6: default
def defaultStyles : Array UInt32 := #[
  Term.black, Term.white,     -- cursor: black on white
  Term.black, Term.green,     -- selected row
  Term.black, Term.magenta,   -- selected col + cursor row
  Term.magenta, Term.default, -- selected col
  Term.default, Term.default, -- cursor row
  Term.yellow, Term.default,  -- cursor col
  Term.default, Term.default  -- default
]

-- | Column position: (colIdx, xPos, width)
abbrev ColPos := Nat × Nat × Nat

/-! ## Pure Visibility Model -/

-- | Display order: key columns first, then rest (as names)
def displayCols (keyCols : Array String) (colNames : Array String) : Array String :=
  let validKeys := keyCols.filter fun k => colNames.any (· == k)
  let rest := colNames.filter fun c => !validKeys.any (· == c)
  validKeys ++ rest

-- | Get column index from name (O(n) linear scan)
def colIndex (name : String) (colNames : Array String) : Nat :=
  colNames.findIdx? (· == name) |>.getD 0

-- | Resolve key column names to indices
def resolveKeyCols (keyCols : Array String) (colNames : Array String) : Array Nat :=
  keyCols.filterMap fun name => colNames.findIdx? (· == name)

-- | Display order: key columns first, then rest
def displayOrder (keyCols : Array Nat) (nCols : Nat) : Array Nat :=
  keyCols ++ (Array.range nCols).filter (!keyCols.contains ·)

-- | Render table with nav state, returns (offset, cols, keyW)
def table (st : SomeTable) (nav : PureState) (screenH : Nat)
          (decimals : Nat := 3)
          (selColIdxs : Array Nat := #[]) (selRows : Array Nat := #[]) : IO (Nat × Array ColPos × Nat) := do
  Term.clear
  let colNames := st.colNames
  let keyIdxs := resolveKeyCols nav.keyCols colNames
  let curRow := nav.rowCur
  -- convert display cursor to original column index
  let dispCols := displayCols nav.keyCols colNames
  let curColOrig := colIndex (dispCols.getDisp nav.colCur "") colNames
  -- build column indices in display order (key cols first, then rest)
  let colIdxs := displayOrder keyIdxs st.nCols
  -- row range (screenH-2: 1 for header top, 1 for header bottom)
  let visRows := screenH - 2
  let startRow := if curRow < visRows then 0 else curRow - visRows + 1
  let endRow := min st.nRows (startRow + visRows)
  -- render via C (header + data + separator)
  let cols ← st.render colIdxs keyIdxs.size nav.colOff.val startRow endRow curRow curColOrig
                       selColIdxs selRows defaultStyles (50 : UInt8) (20 : UInt8) decimals.toUInt8
  -- find separator position from returned cols
  let visKeys := min keyIdxs.size cols.size
  let sepX := if visKeys == 0 then 0
    else let (_, x, w) := cols.getD (visKeys - 1) (0, 0, 0); x + w
  return (nav.colOff.val, cols, sepX)

-- | Format number with comma separators (1000000 -> "1,000,000")
def fmtNum (n : Nat) : String :=
  let s := toString n
  if s.length <= 3 then s
  else
    let rec go (cs : List Char) (i : Nat) : List Char :=
      match cs with
      | [] => []
      | c :: rest =>
        if i > 0 && i % 3 == 0 then ',' :: c :: go rest (i + 1)
        else c :: go rest (i + 1)
    (go s.toList.reverse 0).reverse |> String.ofList

-- | Get memory usage in MB from /proc/self/status
def memMB : IO Nat := do
  try
    let s ← IO.FS.readFile "/proc/self/status"
    -- find "VmRSS:" line, parse kB value
    for line in s.splitOn "\n" do
      if line.startsWith "VmRSS:" then
        let parts := line.splitOn " " |>.filter (!·.isEmpty)
        match parts with
        | _ :: val :: _ => return (val.toNat? |>.getD 0) / 1024
        | _ => return 0
    return 0
  catch _ => return 0

-- | Shorten PRQL: "from `path` | freq {a}" -> "freq a"
def shortenPrql (prql : String) : String :=
  -- Find " | " separator between from clause and operations
  match prql.splitOn " | " with
  | [_] => ""  -- base query only, no operations
  | _ :: rest =>
    let ops := " | ".intercalate rest
    -- simplify common patterns
    if ops.startsWith "freq {" then
      let col := ops.drop 6 |>.takeWhile (· != '}')
      s!"freq {col}"
    else if ops.startsWith "filter " then
      s!"filter {ops.drop 7}"
    else if ops.startsWith "sort " then
      s!"sort {ops.drop 5}"
    else ops
  | [] => ""

-- | Shorten path for display (strip source: prefix)
def shortenPath (p : String) : String :=
  if p.startsWith Source.pfx then p.drop Source.pfx.length else p

-- | Render tab line: view1 | view2 | ... (all views on stack)
def tabLine (views : Array (String × String × String)) (y : UInt32) (screenW : Nat) : IO Unit := do
  -- views: (path, disp, prql); head=current, tail=parents; reverse for display
  let rev := views.reverse
  let n := rev.size
  -- build labels, omit path if same as previous
  let (labels, _) := rev.foldl (init := (#[], ("", 0))) fun (acc, (prevPath, idx)) (path, d, p) =>
    let lbl := if d.isEmpty then shortenPrql p else d
    let sp := shortenPath path
    let txt := if path == prevPath then (if lbl.isEmpty then s!"#{idx+1}" else lbl)
               else if lbl.isEmpty then sp else s!"{sp} {lbl}"
    (acc.push txt, (path, idx + 1))
  -- bracket current view (last after reverse)
  let marked := labels.mapIdx fun i lbl =>
    if i == n - 1 then s!"[{lbl}]" else lbl
  Term.printPad 0 y screenW.toUInt32 Term.white Term.blue (marked.join " | ")

-- | Render status bar at bottom
def statusBar (curRow curCol colOff total screenW : Nat) (keyCols : Array String) (nSelCols : Nat) (selRows : Array Nat)
              (y : UInt32) (msg : String := "") (err : String := "") : IO Unit := do
  -- left side: error (red), message, or key cols/sel info
  let (left, fg) := if !err.isEmpty then (err.take (screenW - 20), Term.red)
    else if !msg.isEmpty then (msg, Term.cyan)
    else
      let keyStr := if keyCols.isEmpty then "" else s!"keys={keyCols.size} "
      let selStr := if nSelCols == 0 then "" else s!"sel={nSelCols} "
      let rowStr := if selRows.isEmpty then "" else s!"rows={selRows.size}"
      (s!"{keyStr}{selStr}{rowStr}", Term.cyan)
  -- right side: col info + mem + row/total
  let mb ← memMB
  let right := s!"c{curCol}+{colOff} {mb}MB {curRow}/{fmtNum total}"
  -- print left, then right-aligned position
  Term.print 0 y fg Term.default left
  let rx := screenW - right.length
  Term.print rx.toUInt32 y Term.cyan Term.default right

-- | Key bindings for info overlay (2 columns: key | hint)
def keyHints : Array (String × String) := #[
  ("j/k", "up/down"), ("h/l", "left/right"),
  ("g/G", "top/end"), ("^D/^U", "page"),
  ("0/$", "first/last"), ("[/]", "sort"),
  ("\\", "filter"), ("F", "freq"),
  ("M", "meta"), ("D", "delete"),
  ("s", "select"), ("!", "key col"),
  ("b", "agg"), ("T", "dup"),
  ("S", "swap"), (":", "cmd"),
  ("r", "lr"), ("q", "quit")
]

-- | Render info overlay at bottom-right (key | hint)
def infoOverlay (_ : SomeTable) (_ _ : Nat) (screenH screenW : Nat) : IO Unit := do
  let nRows := keyHints.size
  let keyW := 5; let hintW := 10
  let boxW := keyW + 1 + hintW
  let x0 := screenW - boxW - 2
  let y0 := screenH - nRows - 3
  for i in [:nRows] do
    let (k, d) := keyHints.getD i ("", "")
    let kpad := "".pushn ' ' (keyW - k.length) ++ k
    let dpad := d.take hintW ++ "".pushn ' ' (hintW - min d.length hintW)
    Term.print x0.toUInt32 (y0 + i).toUInt32 Term.black Term.yellow (kpad ++ " " ++ dpad)

-- | Render full screen: table, tabs, status bar, overlays. Returns new colOff.
def all (v : View) (s : State) (tbl : SomeTable) (di : DisplayInfo) : IO Nat := do
  let w ← Term.width; let h ← Term.height
  let selColIdxs := v.selCols.filterMap fun name => di.colNames.findIdx? (· == name)
  let (off, _, _) ← table tbl v.nav (h.toNat - 3) v.decimals selColIdxs v.selRows
  let views := s.views.map fun v => (v.path, v.disp, v.query.render)
  tabLine views (h - 2) w.toNat
  statusBar v.nav.rowCur v.nav.colCur.val v.nav.colOff.val (v.total.getD di.nRows) w.toNat
            v.nav.keyCols v.selCols.size v.selRows (h - 1) s.msg s.err
  if s.showInfo then infoOverlay tbl 0 0 h.toNat w.toNat
  Term.present
  return off

end Render
