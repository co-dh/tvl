/-
  State types: PureState, View, ViewKind, InputMode, State
  All navigation through PureState with DispIdx for column cursors
-/
import Tv.Types
import Tv.Backend
import Tv.Prql

namespace App

-- | Pure navigation state (all handlers work on this)
-- colCur/colOff are DispIdx to enforce display order indexing
structure PureState where
  rowCur  : Nat := 0              -- row cursor
  rowOff  : Nat := 0              -- row offset (first visible)
  colCur  : DispIdx := ⟨0⟩        -- column cursor in DISPLAY order
  colOff  : DispIdx := ⟨0⟩        -- column offset in DISPLAY order
  keyCols : List String := []     -- key column names (stable across delete)
  delCols : List String := []     -- deleted column names (for EXCLUDE)
  deriving Repr

-- | View kind: how to render/interact
inductive ViewKind where
  | tbl                    -- table view
  | freqV (col : String)   -- frequency view for column
  | colMeta                -- column metadata
  | fld                    -- folder browser
  deriving Inhabited

-- | Single view with PRQL query
structure View where
  path     : String              -- file path
  query    : Prql.Query := {}    -- PRQL query (type-safe)
  disp     : String := ""        -- display name for tab
  nav      : PureState := {}     -- navigation state
  vkind    : ViewKind := .tbl
  cache    : Option SomeTable := none
  selCols  : List DispIdx := []  -- selected columns (display order)
  selRows  : List Nat := []
  total    : Option Nat := none
  decimals : Nat := 3

-- | Pending input for interactive commands
inductive InputMode where
  | none                    -- normal mode
  | selectCols              -- waiting for column names
  | renameTo                -- waiting for new column name
  | filterExpr              -- waiting for filter expression
  | command                 -- command mode
  deriving Inhabited

-- | App state with non-empty view stack (curView always exists)
structure State where
  curView   : View            -- current view (always exists)
  parents   : List View := [] -- parent views (can be empty)
  keys      : List Char := [] -- pending keys to replay
  msg       : String := ""    -- status message
  err       : String := ""    -- error message (shown in red)
  quit      : Bool := false
  testMode  : Bool := false   -- exit after keys consumed
  inputMode : InputMode := .none  -- current input mode
  inputBuf  : String := ""        -- input buffer for interactive commands
  showInfo  : Bool := false       -- show info overlay (toggle with I)

-- | Current view (just curView, no headD needed)
def State.cur (s : State) : View := s.curView

-- | All views as list (for compatibility)
def State.views (s : State) : List View := s.curView :: s.parents

-- | Update current view
def State.setCur (s : State) (v : View) : State := { s with curView := v }

-- | Push new view (current becomes parent)
def State.push (s : State) (v : View) : State :=
  { s with curView := v, parents := s.curView :: s.parents }

-- | Pop view (returns to parent, stays if no parent)
def State.pop (s : State) : State :=
  match s.parents with
  | p :: rest => { s with curView := p, parents := rest }
  | [] => s  -- can't pop last view

-- | Swap top two views
def State.swapViews (s : State) : State :=
  match s.parents with
  | p :: rest => { s with curView := p, parents := s.curView :: rest }
  | [] => s  -- no parent to swap with

-- | Duplicate current view
def State.dupView (s : State) : State :=
  { s with parents := s.curView :: s.parents }

-- | Set status message
def State.setMsg (s : State) (m : String) : State := { s with msg := m }

-- | Max rows to fetch (prevent OOM on huge files)
def maxRows : Nat := 1000

-- | Fetch table for view (uses cache or queries backend). Returns error msg if any.
def View.fetch (v : View) : IO (View × SomeTable × String) := do
  match v.cache with
  | some st => return (v, st, "")
  | none =>
    let prql := v.query.render
    match ← Backend.query (Backend.mkLimited prql maxRows) v.path with
    | .ok st =>
      let total ← match v.total with
        | some n => pure n
        | none => match ← Backend.queryCount prql v.path with
          | .ok n => pure n
          | .error _ => pure st.nRows
      return ({ v with cache := some st, total := some total }, st, "")
    | .error e =>
      Backend.logError s!"Query error: {e}"
      let short := e.splitOn "───" |>.head? |>.getD e |>.take 80
      return (v, ⟨0, Table.empty⟩, short)

-- | Invalidate cache (after PRQL change)
def View.invalidate (v : View) : View := { v with cache := none }

-- | View.copy helper for updating query and resetting cache/total
def View.copy (v : View) (query : Prql.Query := v.query) : View :=
  { v with query := query, cache := none, total := none }

-- | Format cell value for PRQL filter
def cellToPrql : Cell → String
  | .null => "null"
  | .int n => s!"{n}"
  | .float f => s!"{f}"
  | .str s => s!"'{s}'"
  | .bool b => if b then "true" else "false"

end App
