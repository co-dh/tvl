/-
  Backend: PRQL compilation + chunked query execution
  Views store PRQL strings, compile to SQL on demand
-/
import Tv.Adbc
import Tv.Types
import Tv.Prql

namespace Backend

-- | Init backend (ADBC + shellfs extension)
def init : IO Bool := do
  let ok ← Adbc.init
  if ok then let _ ← Adbc.query "LOAD shellfs"
  pure ok

-- | Shutdown backend
def shutdown : IO Unit := Adbc.shutdown

-- | Execute raw SQL, return SomeTable (zero-copy)
def execSql (sql : String) : IO SomeTable := do
  let qr ← Adbc.query sql
  SomeTable.ofQueryResult qr

-- | Get timestamp as HH:MM:SS.mmm
def timestamp : IO String := do
  let ms ← IO.monoMsNow
  let s := ms / 1000 % 86400  -- seconds in day
  let h := s / 3600
  let m := (s % 3600) / 60
  let sec := s % 60
  let milli := ms % 1000
  let d2 := fun n : Nat => s!"{Char.ofNat (48 + n / 10)}{Char.ofNat (48 + n % 10)}"
  pure s!"{h}:{d2 m}:{d2 sec}.{milli}"

-- | Log to /tmp/tv.log
def logPrql (prql : String) : IO Unit := do
  let ts ← timestamp
  let h ← IO.FS.Handle.mk "/tmp/tv.log" .append
  h.putStrLn s!"[{ts}] [prql] {prql}"

-- | Log error to file (not stderr - silent is golden)
def logError (msg : String) : IO Unit := do
  let ts ← timestamp
  let h ← IO.FS.Handle.mk "/tmp/tv.log" .append
  h.putStrLn s!"[{ts}] [error] {msg}"

-- | Check if string contains "take "
def hasLimit (s : String) : Bool := (s.splitOn "take ").length > 1

-- | PRQL query with proof it has a limit
structure LimitedQuery where
  prql : String
  proof : hasLimit prql = true

-- | Execute PRQL query (requires proof of limit)
def query (q : LimitedQuery) : IO (Except String SomeTable) := do
  logPrql q.prql
  match ← Prql.compile q.prql with
  | .error e => return .error e
  | .ok sql =>
    try
      let st ← execSql sql
      return .ok st
    catch e =>
      return .error s!"SQL error: {e}"

-- | Create LimitedQuery by appending take
def mkLimited (prql : String) (n : Nat) : LimitedQuery :=
  let q := s!"{prql} | take {n}"
  if h : hasLimit q = true then ⟨q, h⟩
  else ⟨"select 1 | take 1", by native_decide⟩  -- fallback, never reached

-- | Theorem: example shows mkLimited has limit
theorem mkLimited_example : hasLimit "from df | take 1000" = true := by native_decide

-- | Get total row count for PRQL query
def queryCount (prql : String) : IO (Except String Nat) := do
  let countPrql := prql ++ " | aggregate {n = std.count this}"
  match ← query (mkLimited countPrql 1) with
  | .error e => return .error e
  | .ok st =>
    if st.nRows > 0 then
      match st.getIdx 0 0 with
      | .int cnt => return .ok cnt.toNat
      | _ => return .ok 0
    else
      return .ok 0

-- | Get cell values for a specific row (for freq Enter filter)
def queryRow (prql : String) (row : Nat) (ncols : Nat) : IO (Except String (Array Cell)) := do
  let rowPrql := prql ++ s!" | take {row + 1}"
  match ← query (mkLimited rowPrql (row + 1)) with
  | .error e => return .error e
  | .ok st =>
    if st.nRows > row then
      return .ok (Array.range ncols |>.map fun c => st.getIdx row c)
    else
      return .ok #[]

-- | Query all distinct values for a column (for fzf picker)
def queryDistinct (prql : String) (col : String) : IO (Except String (Array String)) := do
  let distinctPrql := prql ++ " | select {" ++ col ++ "} | group {" ++ col ++ "} (take 1)"
  logPrql distinctPrql
  match ← Prql.compile distinctPrql with
  | .error e => return .error e
  | .ok sql =>
    try
      let st ← execSql sql
      return .ok ((Array.range st.nRows).map fun r => toString (st.getIdx r 0))
    catch e =>
      return .error s!"SQL error: {e}"

end Backend
