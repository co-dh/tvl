-- Test ADBC + Backend integration
import Tv.Adbc
import Tv.Backend

def main : IO Unit := do
  IO.println "Testing ADBC..."

  -- Init backend
  let ok ← Backend.init
  if !ok then
    IO.println "Failed to init Backend"
    return
  IO.println "Backend initialized"

  -- Test PRQL compilation
  IO.println "\nTesting PRQL compilation..."
  match ← Backend.compilePrql "from df | select {a, b}" with
  | .error e => IO.println s!"PRQL error: {e}"
  | .ok sql => IO.println s!"SQL: {sql}"

  -- Test query with PRQL
  IO.println "\nTesting PRQL query on CSV..."
  match ← Backend.query "from df | take 3" "data/basic.csv" with
  | .error e => IO.println s!"Query error: {e}"
  | .ok tbl =>
    IO.println s!"Table: {tbl.nRows} rows, {tbl.nCols} cols"
    -- Print header
    for c in tbl.cols do IO.print s!"{c.name}\t"
    IO.println ""
    -- Print rows
    for r in [:tbl.nRows] do
      for c in [:tbl.nCols] do
        IO.print s!"{tbl.get r c}\t"
      IO.println ""

  -- Test count
  IO.println "\nTesting row count..."
  match ← Backend.queryCount "from df" "data/basic.csv" with
  | .error e => IO.println s!"Count error: {e}"
  | .ok n => IO.println s!"Total rows: {n}"

  -- Shutdown
  Backend.shutdown
  IO.println "\nDone"
