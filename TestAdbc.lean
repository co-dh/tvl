-- Test ADBC + Backend integration
import Tv.Adbc
import Tv.Backend
import Tv.Source
import Tv.Prql

def main : IO Unit := do
  IO.println "Testing ADBC..."
  let path := "data/basic.csv"
  let base := Source.fromExpr path

  -- Init backend
  let ok ← Backend.init
  if !ok then
    IO.println "Failed to init Backend"
    return
  IO.println "Backend initialized"

  -- Test PRQL compilation
  IO.println "\nTesting PRQL compilation..."
  match ← Prql.compile s!"{base} | select {{a, b}}" with
  | .error e => IO.println s!"PRQL error: {e}"
  | .ok sql => IO.println s!"SQL: {sql}"

  -- Test query with PRQL
  IO.println "\nTesting PRQL query on CSV..."
  match ← Backend.query (Backend.mkLimited s!"{base}" 3) with
  | .error e => IO.println s!"Query error: {e}"
  | .ok tbl =>
    IO.println s!"Table: {tbl.nRows} rows, {tbl.nCols} cols"
    -- Print header
    IO.println (tbl.colNames.toList |> String.intercalate "\t")
    -- Print rows
    for r in [:tbl.nRows] do
      for c in [:tbl.nCols] do
        IO.print s!"{tbl.getIdx r c}\t"
      IO.println ""

  -- Test count
  IO.println "\nTesting row count..."
  match ← Backend.queryCount base with
  | .error e => IO.println s!"Count error: {e}"
  | .ok n => IO.println s!"Total rows: {n}"

  -- Test freq
  IO.println "\nTesting freq..."
  match ← Backend.query (Backend.mkLimited s!"{base} | freq b" 100) with
  | .error e => IO.println s!"Freq error: {e}"
  | .ok tbl =>
    IO.println s!"Freq: {tbl.nRows} rows, {tbl.nCols} cols"
    IO.println (tbl.colNames.toList |> String.intercalate "\t")
    for r in [:tbl.nRows] do
      for c in [:tbl.nCols] do
        IO.print s!"{tbl.getIdx r c}\t"
      IO.println ""

  -- Shutdown
  Backend.shutdown
  IO.println "\nDone"
