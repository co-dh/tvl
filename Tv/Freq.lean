/-
  Freq view operations (frequency/group by view)
-/
import Tv.Types
import Tv.State
import Tv.Backend
import Tv.Prql

namespace App.Freq

-- | Frequency query with percentage bar
def query (q : Prql.Query) (cols : Array String) : Prql.Query :=
  let grp : Prql.Op := .group cols #[(.count, "Cnt", "this")]
  let pct : Prql.Op := .derive #[("Pct", "Cnt * 100 / std.sum Cnt"),
                                  ("Bar", "s\"repeat('#', CAST({Pct} / 5 AS INTEGER))\"")]
  let srt : Prql.Op := .sort #[("Cnt", false)]  -- desc
  q.pipe grp |>.pipe pct |>.pipe srt

-- | Create freq view from parent view and columns
def mkView (v : View) (cols : Array String) : View :=
  ⟨v.path, query v.query cols, s!"freq {cols.join ","}", { keyCols := cols }, .freqV cols, none, #[], #[], none, defDecimals⟩

-- | Handle freq-specific pure keys. Returns none if not handled.
def runKey (v : View) (s : State) (cols : Array String) (row : Option (Array Cell)) (key : PureKey) : Option State :=
  match key with
  | .ret => row.map fun vals =>
      let pq := s.parents.getD 0 v |>.query
      let expr := Prql.buildFilter cols vals
      s.push ⟨v.path, pq.filter expr, s!"filter {expr}", {}, .tbl, none, #[], #[], none, v.decimals⟩
  | _ => none

-- | ret on freqV: query row, then apply pure runKey
def ret (prql : String) (rowCur : Nat) (cols : Array String) (v : View) (s : State) : IO State :=
  Backend.queryRow prql rowCur cols.size <&> fun
    | some vals => runKey v s cols (some vals) .ret |>.getD s
    | none => s

-- | Simp lemmas for keys Freq doesn't handle
@[simp] theorem runKey_j : runKey v s cols row .j = none := rfl
@[simp] theorem runKey_k : runKey v s cols row .k = none := rfl
@[simp] theorem runKey_l : runKey v s cols row .l = none := rfl
@[simp] theorem runKey_h : runKey v s cols row .h = none := rfl

end App.Freq
