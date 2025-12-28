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
  keyCols : Array String := #[]   -- key column names (stable across delete)
  delCols : Array String := #[]   -- deleted column names (for EXCLUDE)
  deriving Repr

-- | View kind: how to render/interact
inductive ViewKind where
  | tbl                         -- table view
  | freqV (cols : Array String) -- frequency view for columns
  | colMeta                     -- column metadata
  | fld                         -- folder browser
  deriving Inhabited

-- | Default decimal places for float display
def defDecimals : Nat := 3

-- | Single view with PRQL query
structure View where
  path     : String              -- file path (for display/caching)
  query    : Prql.Query := {}    -- PRQL query (base has from clause)
  disp     : String := ""        -- display name for tab
  nav      : PureState := {}     -- navigation state
  vkind    : ViewKind := .tbl
  cache    : Option SomeTable := none  -- zero-copy cached query result
  selCols  : Array String := #[]   -- selected column names
  selRows  : Array Nat := #[]
  total    : Option Nat := none
  decimals : Nat := defDecimals

-- | Pending input for interactive commands
inductive InputMode where
  | none                    -- normal mode
  | renameTo                -- waiting for new column name
  deriving Inhabited

-- | App state with non-empty view stack (curView always exists)
structure State where
  curView   : View             -- current view (always exists)
  parents   : Array View := #[] -- parent views (can be empty)
  keys      : Array Char := #[] -- pending keys to replay
  msg       : String := ""     -- status message
  err       : String := ""     -- error message (shown in red)
  quit      : Bool := false
  testMode  : Bool := false    -- exit after keys consumed
  inputMode : InputMode := .none  -- current input mode
  inputBuf  : String := ""        -- input buffer for interactive commands
  showInfo  : Bool := false       -- show info overlay (toggle with I)

-- | Current view (just curView, no headD needed)
def State.cur (s : State) : View := s.curView

-- | All views as array
def State.views (s : State) : Array View := #[s.curView] ++ s.parents

-- | Update current view
def State.setCur (s : State) (v : View) : State := { s with curView := v }

-- | Push new view (current becomes parent)
def State.push (s : State) (v : View) : State :=
  { s with curView := v, parents := #[s.curView] ++ s.parents }

-- | Pop view (returns to parent, stays if no parent)
def State.pop (s : State) : State :=
  if h : s.parents.size > 0 then
    { s with curView := s.parents[0], parents := s.parents.extract 1 s.parents.size }
  else s

-- | Swap top two views
def State.swapViews (s : State) : State :=
  if h : s.parents.size > 0 then
    { s with curView := s.parents[0], parents := #[s.curView] ++ s.parents.extract 1 s.parents.size }
  else s

-- | Duplicate current view
def State.dupView (s : State) : State :=
  { s with parents := #[s.curView] ++ s.parents }

-- | Max rows to fetch (prevent OOM on huge files)
def maxRows : Nat := 1000

-- | Fetch table for view. Returns (updated view, table, error).
--   1. Return cached table if available
--   2. Query backend, early return empty on failure
--   3. Compute total count, cache result
def View.fetch (v : View) : IO (View × SomeTable × String) := do
  if let some st := v.cache then return (v, st, "")
  let prql := v.query.render
  let some st ← Backend.query (Backend.mkLimited prql maxRows) | return (v, ← SomeTable.empty, "")
  let total ← v.total.map pure |>.getD ((← Backend.queryCount prql).getD st.nRows |> pure)
  return ({ v with cache := some st, total := some total }, st, "")

-- | Invalidate cache (after PRQL change)
def View.invalidate (v : View) : View := { v with cache := none }

-- | View.copy helper for updating query and resetting cache/total
def View.copy (v : View) (query : Prql.Query := v.query) : View :=
  { v with query := query, cache := none, total := none }

end App
