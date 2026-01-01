/-
  Qtv.Render - Table rendering matching Q version
  Q: align, rend, rend1, sb, Addstr
-/
import Qtv.Op

namespace Qtv

-- | Render cell: (y, x, text, attr)
structure RCell where
  y : Nat
  x : Nat
  txt : String
  attr : UInt32
  deriving Repr

-- | Align strings to max width in each column
-- Q: align:{{(|/#:''x)$/:x}(,$!x),$[#*r:.Q.s2'. x:.Q.sw@+x;+r;()]}
def align (t : Tbl) (r0 r1 : Nat) : Array (Array String) :=
  t.cols.map fun col =>
    let cells := #[col.name] ++ (col.data.extract r0 r1).map Cell.toString
    let maxW := cells.foldl (fun m s => max m s.length) 0
    cells.map fun s =>
      if s.length < maxW then s ++ String.ofList (List.replicate (maxW - s.length) ' ')
      else s

-- | Attribute flags (matches termbox)
def attrUnderline : UInt32 := 0x0100
def attrReverse   : UInt32 := 0x0200
def attrBold      : UInt32 := 0x0400

namespace State

-- | Render single cell
-- Q: rend1:{[t;xs;cr;cc;r;c]
--      ; a:$[0=r;A`A_UNDERLINE;0]
--      ; a:bor[a;$[cr=r-1; .cl.row;0]]
--      ; a:bor[a;$[c=cc;.cl.col;0]]
--      ; cell: t[r;c],$[c=kc-1;"|";""]
--      ; (r;xs c;cell;a)}
def rend1 (s : State) (aligned : Array (Array String)) (xs : Array Nat)
    (r c cr cc : Nat) : RCell :=
  let isHeader := r == 0
  let isRowCur := r > 0 && r - 1 == cr
  let isColCur := c == cc
  let txt := (aligned.getD c #[]).getD r ""
  let txt := if c < s.kc && c + 1 == s.kc then txt ++ "|" else txt
  let attr := (if isHeader then attrUnderline else 0)
            ||| (if isRowCur then attrReverse else 0)
            ||| (if isColCur then attrBold else 0)
  ⟨r, xs.getD c 0, txt, attr⟩

-- | Compute column x positions with horizontal scroll
-- Q: xs:-1_0,(+\)1+count each t 0
--    xs,: last[xs]+count last t[0]
--    if[xs[cc+1]>yx 1; shift:xs first where xs>xs[cc+1]-yx 1; xs-: shift]
def colXs (s : State) (widths : Array Nat) : Array Nat × Nat × Nat :=
  let w := s.width
  let xs := widths.foldl (init := #[0]) fun acc w => acc.push (acc.back! + w + 1)
  let ccX := xs.getD (s.cc + 1) w
  let shift := if ccX > w then
    xs.findIdx? (· > ccX - w) |>.getD 0 |> fun i => xs.getD i 0
  else 0
  let xs := xs.map (· - shift)
  let c0 := xs.findIdx? (· >= 0) |>.getD 0
  let c1 := xs.findIdx? (· > w) |>.getD widths.size
  (xs, c0, c1)

-- | Render table
-- Q: rend:{[t;cr;cc]
--      ; xs:-1_0,(+\)1+count each t 0
--      ; ...
--      ; raze til[count t]rend1[t;xs;cr;cc]/:\:til count t 0}
def render (s : State) : Array RCell :=
  let r0 := s.r0
  let r1 := min (r0 + s.height - 2) s.CT
  let aligned := align s.t r0 r1
  let widths := aligned.map fun col => (col.getD 0 "").length
  let (xs, c0, c1) := s.colXs widths
  let cr := s.cr - r0
  let nRows := r1 - r0 + 1
  let rows := Array.range nRows
  let cols := (Array.range (c1 - c0)).map (· + c0)
  rows.flatMap fun r => cols.map fun c => s.rend1 aligned xs r c cr s.cc

-- | Status bar text
-- Q: sb:{s:neg[-10+yx 1]$msg, "|",("/"sv -3 sublist "/"vs fn,"|"),string[kn], " ",string[cr],"/",commify CT`; Addstr[yx[0]-1;0;s;.cl.st]; refresh[]}
def statusBar (s : State) : String :=
  let sep := "|"
  let pos := s!"{s.cr + 1}/{Cell.fmtInt s.CT}"
  let col := s.CC
  let typ := s.gl.typ
  let stack := s!"{s.st.size + 1}"
  s!"{s.msg}{sep}{typ}{sep}{col}{sep}{pos}{sep}S:{stack}"

-- | Render status bar
def renderStatus (s : State) : RCell :=
  ⟨s.height - 1, 0, s.statusBar, attrReverse⟩

end State

end Qtv
