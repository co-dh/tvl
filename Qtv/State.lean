/-
  Qtv.State - State management matching Q version
  Q: st (stack), push, q (pop), D (dup), S (swap)
-/
import Qtv.Types

namespace Qtv

-- | App state
-- Q: st + GL globals
structure State where
  st   : ViewStack := #[]     -- stack of parent views
  gl   : GL := {}             -- current view globals
  reg  : Reg := #[]           -- search registry
  msg  : String := ""         -- status message
  yx   : Nat × Nat := (24,80) -- terminal size (rows, cols)
  quit : Bool := false
  deriving Repr

namespace State

-- | Current table
-- Q: t
def t (s : State) : Tbl := s.gl.t

-- | Row count
-- Q: CT:{count t}
def CT (s : State) : Nat := s.t.nRows

-- | Column count
def nCols (s : State) : Nat := s.t.nCols

-- | Current column index
-- Q: cc
def cc (s : State) : Nat := s.gl.cc

-- | Current row index
-- Q: cr
def cr (s : State) : Nat := s.gl.cr

-- | First visible row
-- Q: r0
def r0 (s : State) : Nat := s.gl.r0

-- | Key column count
-- Q: kc
def kc (s : State) : Nat := s.gl.kc

-- | Current column name
-- Q: CC:{(!+t)cc}
def CC (s : State) : String :=
  s.t.colNames.getD s.cc ""

-- | Current cell value
-- Q: CV:{t[CC`]cr}
def CV (s : State) : Cell :=
  s.t.get s.cr s.cc

-- | Screen height
def height (s : State) : Nat := s.yx.1

-- | Screen width
def width (s : State) : Nat := s.yx.2

-- | Update GL
def setGL (s : State) (gl : GL) : State := { s with gl }

-- | Update table in current view
def setT (s : State) (t : Tbl) : State :=
  { s with gl := { s.gl with t } }

-- | Push new view onto stack
-- Q: push:{st::enlist[GL!value each GL],st;GL set'(0;0;0;0;x;y)}
def push (s : State) (typ : String) (t : Tbl) : State :=
  { s with
    st := #[s.gl] ++ s.st
    gl := { r0 := 0, cr := 0, cc := 0, kc := 0, typ, t } }

-- | Pop view from stack
-- Q: .kf.q:{if[count st; `r0`cr`cc`kc`type`t set' st[0]; st::1_ st]; -1+count st}
def pop (s : State) : State :=
  if h : s.st.size > 0 then
    { s with gl := s.st[0], st := s.st.extract 1 s.st.size }
  else
    { s with quit := true }

-- | Swap top two views
-- Q: .kf.S:{if[1>count st;:()];n:st[0]; st::enlist[GL!value each GL],1_st; GL set'n}
def swap (s : State) : State :=
  if h : s.st.size > 0 then
    { s with gl := s.st[0], st := #[s.gl] ++ s.st.extract 1 s.st.size }
  else s

-- | Duplicate current view
-- Q: .kf.D:{push[`]t}
def dup (s : State) : State :=
  { s with st := #[s.gl] ++ s.st }

-- | Set search pattern
-- Q: reg["/"]:pattern
def setReg (s : State) (k : Char) (v : String) : State :=
  let r := s.reg.filter (·.1 != k)
  { s with reg := r.push (k, v) }

-- | Get search pattern
def getReg (s : State) (k : Char) : Option String :=
  s.reg.find? (·.1 == k) |>.map (·.2)

end State

end Qtv
