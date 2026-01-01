/-
  Qtv.Nav - Navigation matching Q version
  Q: C1, down, R, R0, G, g, arrows, ^D, ^U
-/
import Qtv.State

namespace Qtv

namespace State

-- | Set cursor row clamped
-- Q: R:{cr::0|x&CT[]-1}
def R (s : State) (x : Int) : State :=
  let cr := (max 0 (min x (s.CT - 1))).toNat
  { s with gl := { s.gl with cr } }

-- | Set first visible row clamped
-- Q: R0:{r0::0|x&CT[]-yx[0]-2}
def R0 (s : State) (x : Int) : State :=
  let maxR0 := s.CT - (s.height - 2)
  let r0 := (max 0 (min x maxR0)).toNat
  { s with gl := { s.gl with r0 } }

-- | Move column cursor
-- Q: C1:{cc::(-1+count cols t)&0|x+cc}
def C1 (s : State) (d : Int) : State :=
  let cc := (max 0 (min (s.cc + d) (s.nCols - 1))).toNat
  { s with gl := { s.gl with cc } }

-- | Move row cursor with scroll adjustment
-- Q: down:{mr:yx[0]-3;R cr+x; $[0>s:cr-r0;R0 r0+s; s>mr;R0 r0+s-mr]}
def down (s : State) (d : Int) : State :=
  let mr := s.height - 3
  let s := s.R (s.cr + d)
  let diff := s.cr - s.r0
  if diff < 0 then s.R0 (s.r0 + diff)
  else if diff > mr then s.R0 (s.r0 + diff - mr)
  else s

-- | Page size for ^D/^U
def pageSize (s : State) : Nat := s.height - 2

-- | Navigation commands
-- Q: .kf.KEY_DOWN:{down 1}
def navDown  (s : State) : State := s.down 1
-- Q: .kf.KEY_UP:{down -1}
def navUp    (s : State) : State := s.down (-1)
-- Q: .kf.KEY_LEFT:{C1 -1}
def navLeft  (s : State) : State := s.C1 (-1)
-- Q: .kf.KEY_RIGHT:{C1 1}
def navRight (s : State) : State := s.C1 1
-- Q: .kf[`$"^D"]:{down yx[0]-2}
def navPgDn  (s : State) : State := s.down s.pageSize
-- Q: .kf[`$"^U"]:{down neg yx[0]-2}
def navPgUp  (s : State) : State := s.down (- s.pageSize)
-- Q: .kf.g:{down neg CT[]}
def navTop   (s : State) : State := s.down (- s.CT)
-- Q: .kf.G:{down CT[]}
def navBot   (s : State) : State := s.down s.CT

end State

end Qtv
