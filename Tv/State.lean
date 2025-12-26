/-
  State types: NavState, View, ViewKind, InputMode, State
-/
import Tv.Types
import Tv.Backend

namespace App

-- | Core navigation state (cursor + offset for row/col, key columns)
structure NavState where
  rowCur  : Nat := 0       -- row cursor
  rowOff  : Nat := 0       -- row offset (first visible)
  colCur  : Nat := 0       -- column cursor
  colOff  : Nat := 0       -- column offset (first visible in display order)
  keyCols : List Nat := [] -- key columns (pinned left)
  deriving Repr

namespace NavState
def create : NavState := ⟨0, 0, 0, 0, []⟩
def goto (col nCols : Nat) : NavState := ⟨0, 0, min col (nCols - 1), 0, []⟩
end NavState

-- | View kind: how to render/interact
inductive ViewKind where
  | tbl                    -- table view
  | freqV (col : String)   -- frequency view for column
  | colMeta                -- column metadata
  | fld                    -- folder browser
  deriving Inhabited

-- | Single view with PRQL query
structure View where
  path    : String         -- file path
  prql    : String         -- PRQL query (from df | ...)
  disp    : String := ""   -- display name for tab
  nav     : NavState := NavState.create
  vkind   : ViewKind := .tbl
  cache   : Option Table := none
  selCols : List Nat := []
  selRows : List Nat := []
  total   : Option Nat := none
  decimals : Nat := 3

-- | Pending input for interactive commands
inductive InputMode where
  | none                    -- normal mode
  | selectCols              -- waiting for column names
  | renameTo                -- waiting for new column name
  | filterExpr              -- waiting for filter expression
  | command                 -- command mode
  deriving Inhabited

-- | App state with view stack
structure State where
  views    : List View      -- head is current, tail is parent stack
  keys     : List Char := [] -- pending keys to replay
  msg      : String := ""   -- status message
  quit     : Bool := false
  testMode : Bool := false  -- exit after keys consumed
  inputMode : InputMode := .none  -- current input mode
  inputBuf  : String := ""        -- input buffer for interactive commands
  showInfo : Bool := false        -- show info overlay (toggle with I)

-- | Default empty view
def View.empty : View := ⟨"", "from df", "", NavState.create, .tbl, none, [], [], none, 3⟩

-- | Current view
def State.cur (s : State) : View := s.views.headD View.empty

-- | Update current view
def State.setCur (s : State) (v : View) : State :=
  { s with views := v :: s.views.tailD [] }

-- | Push new view
def State.push (s : State) (v : View) : State :=
  { s with views := v :: s.views }

-- | Pop view (returns to parent)
def State.pop (s : State) : State :=
  { s with views := s.views.tailD [] }

-- | Swap top two views
def State.swapViews (s : State) : State :=
  match s.views with
  | v1 :: v2 :: rest => { s with views := v2 :: v1 :: rest }
  | _ => s

-- | Duplicate current view
def State.dupView (s : State) : State :=
  match s.views with
  | v :: _ => { s with views := v :: s.views }
  | [] => s

-- | Set status message
def State.setMsg (s : State) (m : String) : State := { s with msg := m }

-- | Max rows to fetch (prevent OOM on huge files)
def maxRows : Nat := 1000

-- | Fetch table for view (uses cache or queries backend)
def View.fetch (v : View) : IO (View × Table) := do
  match v.cache with
  | some t => return (v, t)
  | none =>
    match ← Backend.query (Backend.mkLimited v.prql maxRows) v.path with
    | .ok t =>
      let total ← match v.total with
        | some n => pure n
        | none => do
          match ← Backend.queryCount v.prql v.path with
          | .ok n => pure n
          | .error _ => pure t.nRows
      return ({ v with cache := some t, total := some total }, t)
    | .error e =>
      Backend.logError s!"Query error: {e}"
      return (v, Table.empty)

-- | Invalidate cache (after PRQL change)
def View.invalidate (v : View) : View := { v with cache := none }

-- | View.copy helper for updating PRQL and resetting cache/total
def View.copy (v : View) (prql : String := v.prql) (nav : NavState := v.nav) : View :=
  { v with prql := prql, nav := nav, cache := none, total := none }

-- | Format cell value for PRQL filter
def cellToPrql : Cell → String
  | .null => "null"
  | .int n => s!"{n}"
  | .float f => s!"{f}"
  | .str s => s!"'{s}'"
  | .bool b => if b then "true" else "false"

end App
