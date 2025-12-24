-- Test ADBC integration
import Tv.Adbc

def main : IO Unit := do
  IO.println "Testing ADBC..."

  -- Init
  let ok ← Adbc.init
  if !ok then
    IO.println "Failed to init ADBC"
    return
  IO.println "ADBC initialized"

  -- Query
  let qr ← Adbc.query "SELECT 1 as a, 'hello' as b, 3.14 as c"
  let nc ← Adbc.ncols qr
  let nr ← Adbc.nrows qr
  IO.println s!"Result: {nr} rows, {nc} cols"

  -- Print column names
  for i in [:nc.toNat] do
    let name ← Adbc.colName qr i.toUInt64
    let fmt ← Adbc.colFmt qr i.toUInt64
    IO.println s!"  col {i}: {name} ({fmt})"

  -- Print data
  for r in [:nr.toNat] do
    for c in [:nc.toNat] do
      let v ← Adbc.cellStr qr r.toUInt64 c.toUInt64
      IO.print s!"{v}\t"
    IO.println ""

  -- Test CSV file
  IO.println "\nTesting CSV read..."
  let qr2 ← Adbc.query "SELECT * FROM read_csv('data/basic.csv') LIMIT 5"
  let nc2 ← Adbc.ncols qr2
  let nr2 ← Adbc.nrows qr2
  IO.println s!"CSV: {nr2} rows, {nc2} cols"

  for c in [:nc2.toNat] do
    let name ← Adbc.colName qr2 c.toUInt64
    IO.print s!"{name}\t"
  IO.println ""

  for r in [:nr2.toNat] do
    for c in [:nc2.toNat] do
      let v ← Adbc.cellStr qr2 r.toUInt64 c.toUInt64
      IO.print s!"{v}\t"
    IO.println ""

  -- Shutdown
  Adbc.shutdown
  IO.println "\nDone"
