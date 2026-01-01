/-
  Qtv.Op - Table operations matching Q version
  Q: del, cp, ren, to, xasc, xdesc, !, F, flt, agg, M
-/
import Qtv.Nav

namespace Qtv

-- | Check if pat is substring of s
def hasSub (s pat : String) : Bool :=
  (s.splitOn pat).length > 1

-- | zipWithIndex for Array
def Array.zipIdx (arr : Array α) : Array (α × Nat) :=
  arr.mapIdx fun i x => (x, i)

-- | Find index in list
def List.idxOf [BEq α] (xs : List α) (x : α) : Nat :=
  match xs.findIdx? (· == x) with
  | some i => i
  | none => 0

namespace Tbl

-- | Sort by column ascending
-- Q: xasc:{`x xasc y}
def xasc (t : Tbl) (c : Nat) : Tbl :=
  if t.nRows == 0 then t else
  let col := t.cols.getD c ⟨"", #[]⟩
  let idxs := Array.range t.nRows
  let sorted := idxs.qsort fun i j =>
    match col.data.getD i .null, col.data.getD j .null with
    | .int a, .int b => a < b
    | .float a, .float b => a < b
    | .str a, .str b => a < b
    | .sym a, .sym b => a < b
    | _, _ => false
  let cols := t.cols.map fun col =>
    { col with data := sorted.map (col.data.getD · .null) }
  ⟨cols⟩

-- | Sort by column descending
-- Q: xdesc:{`x xdesc y}
def xdesc (t : Tbl) (c : Nat) : Tbl :=
  if t.nRows == 0 then t else
  let col := t.cols.getD c ⟨"", #[]⟩
  let idxs := Array.range t.nRows
  let sorted := idxs.qsort fun i j =>
    match col.data.getD i .null, col.data.getD j .null with
    | .int a, .int b => a > b
    | .float a, .float b => a > b
    | .str a, .str b => a > b
    | .sym a, .sym b => a > b
    | _, _ => false
  let cols := t.cols.map fun col =>
    { col with data := sorted.map (col.data.getD · .null) }
  ⟨cols⟩

-- | Count occurrences (simple O(n²) approach)
def countVals (arr : Array String) : Array (String × Nat) :=
  let uniq := arr.foldl (init := #[]) fun acc v =>
    if acc.contains v then acc else acc.push v
  uniq.map fun v => (v, arr.filter (· == v) |>.size)

-- | Frequency count
-- Q: .kf.F:{u:0!desc?[t;(); d!d:enlist CC[];enlist[`Cnt]!enlist(count;`i)]
--          ; push[`Freq]update Bar: `$floor[Pct]#\:"#" from update Pct: 100*Cnt%sum u`Cnt from u}
def freq (t : Tbl) (c : Nat) : Tbl :=
  let col := t.cols.getD c ⟨"", #[]⟩
  let strs := col.data.map Cell.toRaw
  let counts := countVals strs |>.qsort (·.2 > ·.2)
  let total := t.nRows
  let vals := counts.map (·.1)
  let cnts := counts.map (fun p => Cell.int p.2)
  let pcts := counts.map fun p =>
    Cell.float (p.2.toFloat * 100.0 / total.toFloat)
  let bars := counts.map fun p =>
    let n := p.2 * 50 / total
    Cell.str (String.ofList (List.replicate n '#'))
  ⟨#[
    ⟨col.name, vals.map Cell.str⟩,
    ⟨"Cnt", cnts⟩,
    ⟨"Pct", pcts⟩,
    ⟨"Bar", bars⟩
  ]⟩

-- | Meta table
-- Q: .kf.M:{push[`m]0!meta t}
def toMeta (t : Tbl) : Tbl :=
  let names := t.colNames.map Cell.str
  let types := t.cols.map fun col =>
    let c0 := col.data.getD 0 .null
    Cell.str (match c0 with
      | .int _ => "i"
      | .float _ => "f"
      | .str _ => "s"
      | .sym _ => "S"
      | .bool _ => "b"
      | .null => "?")
  let lens := t.cols.map fun col => Cell.int col.data.size
  ⟨#[⟨"Col", names⟩, ⟨"Type", types⟩, ⟨"Len", lens⟩]⟩

-- | Filter rows where column matches pattern
-- Q: flt:{?[y; parse each ","vs x;0b;()]}
def fltLike (t : Tbl) (c : Nat) (pat : String) : Tbl :=
  let col := t.cols.getD c ⟨"", #[]⟩
  let mask := col.data.map fun v => hasSub v.toRaw pat
  let idxs := (mask.zipIdx).filterMap fun (b, i) => if b then some i else none
  let cols := t.cols.map fun col =>
    { col with data := idxs.map (col.data.getD · .null) }
  ⟨cols⟩

-- | Filter rows where column equals value
def fltEq (t : Tbl) (c : Nat) (val : String) : Tbl :=
  let col := t.cols.getD c ⟨"", #[]⟩
  let idxs := (col.data.zipIdx).filterMap fun (v, i) =>
    if v.toRaw == val then some i else none
  let cols := t.cols.map fun col =>
    { col with data := idxs.map (col.data.getD · .null) }
  ⟨cols⟩

-- | Simple aggregation (count only)
-- Q: agg:{f:value fzf string `count`sum`avg`dev`min`max; ?[t;();b!b:kc#cols t; enlist[c]!enlist(f;c:CC`)]}
def aggCount (t : Tbl) (keys : Array Nat) : Tbl :=
  let keyStrs := Array.range t.nRows |>.map fun r =>
    keys.map fun k => (t.get r k).toRaw
  let counts := countVals (keyStrs.map fun ks => String.intercalate "|" ks.toList)
  let keyCols := keys.map fun k =>
    let col := t.cols.getD k ⟨"", #[]⟩
    { col with data := counts.map fun (kv, _) =>
        Cell.str ((kv.splitOn "|").getD (List.idxOf keys.toList k) "") }
  let cntCol : Col := ⟨"Cnt", counts.map (Cell.int ·.2)⟩
  ⟨keyCols ++ #[cntCol]⟩

end Tbl

namespace State

-- | Delete current column
-- Q: .kf.d:{C1 fct`del}
def opDel (s : State) : State :=
  if s.nCols <= 1 then s else
  let t := s.t.delCol s.cc
  let cc := min s.cc (t.nCols - 1)
  { s with gl := { s.gl with t, cc } }

-- | Copy current column
-- Q: .kf.c:{fct`cp}
def opCp (s : State) : State :=
  s.setT (s.t.cpCol s.cc)

-- | Rename current column
-- Q: .kf[`$"^"]:{fact[`ren]`$input`}
def opRen (s : State) (nm : String) : State :=
  s.setT (s.t.renCol s.cc nm)

-- | Sort ascending
-- Q: .kf[`$"["]:{fct`xasc}
def opAsc (s : State) : State :=
  s.setT (Tbl.xasc s.t s.cc)

-- | Sort descending
-- Q: .kf[`$"]"]:{fct`xdesc}
def opDesc (s : State) : State :=
  s.setT (Tbl.xdesc s.t s.cc)

-- | Toggle key column
-- Q: .kf[`$"!"]:{n:$[(c:CC`) in k:kc#cols t;k except c;k,c]; kc::count n; t::n xcols t}
def opBang (s : State) : State :=
  let nm := s.CC
  let keys := s.t.colNames.take s.kc
  let newKeys := if keys.contains nm
    then keys.filter (· != nm)
    else keys.push nm
  let t := s.t.xcols newKeys
  { s with gl := { s.gl with t, kc := newKeys.size } }

-- | Push frequency view
-- Q: .kf.F:{...;push[`Freq]...}
def opFreq (s : State) : State :=
  s.push "Freq" (Tbl.freq s.t s.cc)

-- | Push meta view
-- Q: .kf.M:{push[`m]0!meta t}
def opMeta (s : State) : State :=
  s.push "m" (Tbl.toMeta s.t)

-- | Filter by pattern
-- Q: .kf[`$"\\"]:{fat[`flt]enlist input`}
def opFlt (s : State) (pat : String) : State :=
  let t := Tbl.fltLike s.t s.cc pat
  s.push "flt" t

-- | Search forward for pattern
-- Q: Se:{f:(>;<)!(first;last);R ?[t; (((\:;~);CC`;ctype[]$reg"/");(x;`i;cr));();(f x;`i)]; down 0}
-- Q: .kf.n:{Se[>]}
def searchFwd (s : State) (pat : String) : State :=
  let col := s.t.cols.getD s.cc ⟨"", #[]⟩
  let start := s.cr + 1
  match col.data.toList.drop start |>.findIdx? (fun c => hasSub c.toRaw pat) with
  | some i => s.R (start + i)
  | none => s

-- | Search backward
-- Q: .kf.N:{Se[<]}
def searchBwd (s : State) (pat : String) : State :=
  let col := s.t.cols.getD s.cc ⟨"", #[]⟩
  let items := col.data.toList.take s.cr |>.reverse
  match items.findIdx? (fun c => hasSub c.toRaw pat) with
  | some i => s.R (s.cr - 1 - i)
  | none => s

-- | Search for current cell value
-- Q: .kf[`$"*"]:{reg["/"]:CV`;Se[>]}
def searchCur (s : State) : State :=
  let v := s.CV.toRaw
  (s.setReg '/' v).searchFwd v

end State

end Qtv
