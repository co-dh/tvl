/-
  Meta view operations (column metadata view)
-/
import Tv.Types
import Tv.State

namespace App.Meta

-- | Column indices in meta table (from Backend.queryMeta schema)
def colDist : Nat := 3   -- distinct count column
def colNull : Nat := 4   -- null% column

-- | Select rows where cell at column satisfies predicate
def selectRows (st : SomeTable) (col : Nat) (pred : Cell → Bool) : Array Nat :=
  (Array.range st.nRows).filter fun r => pred (st.table.getIdx r col)

-- | Select 100% null columns
def selNull (st : SomeTable) : Array Nat :=
  selectRows st colNull (·.str?.any (· == "100%"))

-- | Select single-value columns (distinct == 1)
def selSingle (st : SomeTable) : Array Nat :=
  selectRows st colDist (·.int?.any (· == 1))

-- | Get column names from selected rows (col 0 is "name")
def selNames (st : SomeTable) (selRows : Array Nat) : Array String :=
  selRows.filterMap fun r =>
    match st.table.getIdx r 0 with
    | .str s => some s
    | _ => none

-- | Pop meta view and set parent's keyCols + selCols
def popState (s : State) (selColNames : Array String) : State :=
  if h : s.parents.size > 0 then
    let parent := s.parents[0]
    let rest := s.parents.extract 1 s.parents.size
    let nav' := { parent.nav with keyCols := selColNames, colCur := ⟨0⟩, colOff := ⟨0⟩ }
    let parent' := { parent with nav := nav', selCols := selColNames }
    { s with curView := parent', parents := rest }
  else s

-- | Theorem: popState sets cursor = 0 and keyCols = sel
theorem popState_cursor (s : State) (sel : Array String) (h : s.parents.size > 0) :
    (popState s sel).curView.nav.colCur.val = 0 ∧
    (popState s sel).curView.nav.keyCols = sel := by
  simp [popState, h]

end App.Meta
