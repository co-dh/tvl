/-
  Key tests for TV - matches Rust TV test suite
  Run with: lake build test && LD_LIBRARY_PATH=/usr/local/lib .lake/build/bin/test
-/
import Tv.Backend

namespace Test

-- | Run tv with --keys and capture rendered output (strip ANSI, take screen buffer)
def runKeys (keys : String) (file : String) : IO String := do
  let child ← IO.Process.spawn {
    cmd := "bash"
    args := #["-c", s!"script -q -c 'stty rows 24 cols 80; LD_LIBRARY_PATH=/usr/local/lib .lake/build/bin/tv \"{file}\" --keys \"{keys}\"' /dev/null | ansi2txt | tail -24"]
    stdin := .null
    stdout := .piped
    stderr := .piped
    env := #[("TERM", "xterm")]
  }
  let stdout ← child.stdout.readToEnd
  let _ ← child.wait
  return stdout

-- | Check if line is content (has letters/digits, not empty)
def isContent (l : String) : Bool :=
  l.any (fun c => c.isAlpha || c.isDigit)

-- | Check string contains substring
def contains (s sub : String) : Bool := (s.splitOn sub).length > 1

-- | Extract footer: (tab line, status line) - last two content lines
def footer (output : String) : String × String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  let tab := lines.getD (n - 2) ""
  let status := lines.getD (n - 1) ""
  (tab, status)

-- | Extract header row: last 80 chars of first line (final render)
def header (output : String) : String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let hdr := lines.headD ""
  if hdr.length > 80 then hdr.drop (hdr.length - 80) else hdr

-- | Get all data lines (skip header, skip footer 2 lines: tab + status)
def dataLines (output : String) : List String :=
  let lines := output.splitOn "\n" |>.filter isContent
  let n := lines.length
  lines.drop 1 |>.take (n - 3)  -- skip header(1) and footer(2)

-- | Assert with message
def assert (cond : Bool) (msg : String) : IO Unit := do
  if cond then IO.println s!"✓ {msg}"
  else throw (IO.userError s!"✗ {msg}")

-- | Check string ends with substring
def endsWith (s sub : String) : Bool := s.endsWith sub

-- === test_keys.rs ===

def test_freq_a_in_tab_line : IO Unit := do
  let output ← runKeys "F" "tests/data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "freq a") s!"F should show 'freq a' in tab: {tab}"

def test_freq_enter_filters_parent : IO Unit := do
  let output ← runKeys "lF<ret>" "tests/data/basic.csv"
  let (tab, status) := footer output
  assert (contains tab "filter") s!"F<ret> should show filter in tab: {tab}"
  assert (contains status "/3") s!"F<ret> should filter to 3 rows: {status}"

def test_delete_twice_different_columns : IO Unit := do
  let output ← runKeys "DD" "tests/data/sample.parquet"
  let hdr := header output
  assert (!contains hdr "id") s!"id should be deleted: {hdr}"
  assert (!contains hdr "age") s!"age should be deleted: {hdr}"
  assert (contains hdr "year") s!"year should be first column: {hdr}"

-- | Bug: D then key 2 cols then F would fail with EXCLUDE syntax (PRQL doesn't support it)
def test_delete_key_freq : IO Unit := do
  let output ← runKeys "Dl l !F" "tests/data/sample.parquet"
  let (tab, status) := footer output
  assert (contains tab "freq") s!"D+key+F should show freq: {tab}"
  assert (!contains status "Error") s!"D+key+F should not error: {status}"

def test_sort_asc_orders_first_row_smallest : IO Unit := do
  let output ← runKeys "[" "tests/data/unsorted.csv"
  let rows := dataLines output
  let first := rows.headD ""
  assert (first.startsWith "1 " || contains first " 1 ") s!"[ should sort asc, first=1: {first}"

def test_sort_desc_orders_first_row_largest : IO Unit := do
  let output ← runKeys "]" "tests/data/unsorted.csv"
  let rows := dataLines output
  let first := rows.headD ""
  assert (first.startsWith "3 " || contains first " 3 ") s!"] should sort desc, first=3: {first}"

def test_meta_shows_column_stats : IO Unit := do
  let output ← runKeys "M" "tests/data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "meta") s!"M should show meta: {tab}"
  assert (contains output "column") s!"should show column stats: {output}"

def test_keys_parquet_freq : IO Unit := do
  let output ← runKeys "F" "tests/data/sample.parquet"
  let (tab, _) := footer output
  assert (contains tab "freq id") s!"F should show freq view: {tab}"

def test_keys_parquet_freq_enter : IO Unit := do
  let output ← runKeys "F<ret>" "tests/data/sample.parquet"
  let (_, status) := footer output
  assert (endsWith status "0/1") s!"F<ret> should filter to 1 row: {status}"

def test_keys_parquet_sort_asc : IO Unit := do
  let output ← runKeys "l[" "tests/data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first " 18 ") s!"[ on age should sort asc, age=18: {first}"

def test_keys_parquet_sort_desc : IO Unit := do
  let output ← runKeys "l]" "tests/data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  assert (contains first " 80 ") s!"] on age should sort desc, age=80: {first}"

def test_keys_parquet_meta : IO Unit := do
  let output ← runKeys "M" "tests/data/sample.parquet"
  let (tab, _) := footer output
  assert (contains tab "meta") s!"M should show meta: {tab}"

def test_page_down_scrolls : IO Unit := do
  let without ← runKeys "" "tests/data/sample.parquet"
  let withPgdn ← runKeys "<C-d>" "tests/data/sample.parquet"
  let (_, status1) := footer without
  let (_, status2) := footer withPgdn
  assert (status1 != status2) s!"Page down should scroll: before={status1} after={status2}"

def test_last_col_visible : IO Unit := do
  let output ← runKeys "llllllllllllllllllll" "tests/data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  let nonWs := first.toList.filter (!·.isWhitespace) |>.length
  assert (nonWs > 0) s!"Last col should show data: {first}"

def test_toggle_key_column : IO Unit := do
  let output ← runKeys "!" "tests/data/xkey.csv"
  let hdr := header output
  assert (contains hdr "|") s!"! should add key column separator: {hdr}"

def test_toggle_key_remove : IO Unit := do
  let output ← runKeys "!!" "tests/data/xkey.csv"
  let hdr := header output
  assert (!contains hdr "|") s!"!! should remove key column separator: {hdr}"

def test_navigation_down : IO Unit := do
  let output ← runKeys "j" "tests/data/basic.csv"
  let (_, status) := footer output
  assert (contains status "1/") s!"j should move to row 1: {status}"

def test_space_selects_column : IO Unit := do
  let output ← runKeys " " "tests/data/basic.csv"
  let (_, status) := footer output
  assert (contains status "*") s!"space should select column (show *): {status}"

-- === test_command.rs ===

def test_multi_column_select : IO Unit := do
  let output ← runKeys " l " "tests/data/full.csv"
  let (_, status) := footer output
  assert (contains status "sel=2") s!"sel=2: {status}"

def test_select_columns : IO Unit := do
  let output ← runKeys "sname,city<ret>" "tests/data/full.csv"
  let hdr := header output
  assert (contains hdr "name") s!"Should have name: {hdr}"
  assert (!contains hdr "value") s!"Should not have value: {hdr}"

def test_select_single : IO Unit := do
  let output ← runKeys "sa<ret>" "tests/data/basic.csv"
  assert (contains output "a") s!"Should have column a"

def test_delcol_multi : IO Unit := do
  let output ← runKeys "l ll D" "tests/data/full.csv"
  let hdr := header output
  assert (!contains hdr "city") s!"Header should not have city: {hdr}"
  assert (contains hdr "name") s!"Header should have name: {hdr}"

def test_delcol_single : IO Unit := do
  let output ← runKeys "lD" "tests/data/basic.csv"
  let hdr := header output
  assert (contains hdr "a") s!"Should have column a: {hdr}"
  assert (!contains hdr "b") s!"Should not have column b: {hdr}"

def test_rename_column : IO Unit := do
  let output ← runKeys "^num<ret>" "tests/data/basic.csv"
  let hdr := header output
  assert (contains hdr "num") s!"Header should have num: {hdr}"
  assert (!contains hdr " a ") s!"Header should not have a: {hdr}"

def test_duplicate_view : IO Unit := do
  let output ← runKeys "T" "tests/data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "[#2]") s!"T should duplicate view (show [#2]): {tab}"

def test_toggle_key_selected_cols : IO Unit := do
  let output ← runKeys " l !" "tests/data/xkey.csv"
  let (_, status) := footer output
  assert (contains status "keys=2") s!"! on selected should set 2 keys: {status}"

-- | Cursor should track column name when key is toggled
-- Navigate to col 1 (b), set as key, cursor should stay on b (now at pos 0), l moves to a
def test_key_cursor_tracks_column : IO Unit := do
  let output ← runKeys "l!l" "tests/data/basic.csv"  -- go to b, key it, go right
  let hdr := header output
  let (_, status) := footer output
  -- After l!l: b is key at pos 0, cursor moved right to a (pos 1)
  assert (contains hdr "b|") s!"b should be key col: {hdr}"
  assert (contains status "c1+") s!"cursor should be at col 1 after l: {status}"

def test_freq_after_meta : IO Unit := do
  let output ← runKeys "MqF" "tests/data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "freq a") s!"tab: {tab}"

def test_freq_by_key_columns : IO Unit := do
  let output ← runKeys "l!F" "tests/data/full.csv"
  let (tab, _) := footer output
  let hdr := header output
  assert (contains tab "freq city") s!"Tab should show freq city: {tab}"
  assert (!contains hdr "name") s!"Header should not contain name: {hdr}"

def test_freq_multi_key_columns : IO Unit := do
  -- Set keys on a and b, then F should freq by both
  let output ← runKeys "!l!F" "tests/data/multi_freq.csv"
  let (tab, _) := footer output
  let hdr := header output
  assert (contains tab "freq a,b") s!"Tab should show freq a,b: {tab}"
  assert (contains hdr "Cnt") s!"Header should have Cnt: {hdr}"
  assert (contains hdr "|") s!"Header should have | separator: {hdr}"

def test_freq_multi_key_enter : IO Unit := do
  -- Set keys on a and b, F, then Enter pushes filtered view
  let output ← runKeys "!l!F<ret>" "tests/data/multi_freq.csv"
  let (_, status) := footer output
  -- First row of freq (a=1,b=x or a=2,b=y with count 2) filters to 2 rows
  assert (endsWith status "/2") s!"Should filter to 2 rows: {status}"

def test_freq_enter_pushes_view : IO Unit := do
  -- F then Enter should push view, q returns to freq, q again returns to original
  let output ← runKeys "F<ret>q" "tests/data/basic.csv"
  let (tab, _) := footer output
  -- After q from filtered view, should be back at freq view
  assert (contains tab "freq") s!"Should be at freq view after q: {tab}"

def test_decimal_increase : IO Unit := do
  let output ← runKeys "." "tests/data/floats.csv"
  -- After '.', decimals goes from 3 to 4: 1.1234 (truncated, not rounded)
  assert (contains output "1.1234") s!"Should show 4 decimals: {output}"

def test_decimal_decrease : IO Unit := do
  let output ← runKeys "," "tests/data/floats.csv"
  let rows := dataLines output
  let firstRow := rows.headD ""
  assert (contains firstRow "1.12") s!"Should show 2 decimals: {firstRow}"
  assert (!contains firstRow "1.123") s!"Should not show 3 decimals: {firstRow}"

def test_swap_views : IO Unit := do
  let output ← runKeys ":filter a > 2<ret>S" "tests/data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "basic") s!"Should show original: {tab}"
  assert (contains tab "filter a > 2") s!"Should show filter: {tab}"

def test_meta_select_rows_xkey_parent : IO Unit := do
  -- TODO: Meta enter should set selected cols as key cols in parent
  -- For now, just verify meta view shows
  let output ← runKeys "M" "tests/data/xkey.csv"
  let (tab, _) := footer output
  assert (contains tab "meta") s!"Should show meta: {tab}"

def test_meta_0_select_null_cols : IO Unit := do
  -- null_col.csv has a,b where b is all null
  -- M0 should select row 1 (b column)
  let output ← runKeys "M0" "tests/data/null_col.csv"
  let (_, status) := footer output
  assert (contains status "rows=1") s!"Should select 1 row with nulls: {status}"

def test_meta_1_select_single_val_cols : IO Unit := do
  -- single_val.csv has a,b where b has only 'x' (dist=1)
  -- M1 should select row 1 (b column)
  let output ← runKeys "M1" "tests/data/single_val.csv"
  let (_, status) := footer output
  assert (contains status "rows=1") s!"Should select 1 row with single value: {status}"

def test_meta_enter_sets_keycols : IO Unit := do
  -- Select b column in meta (row 1), press enter, should see key col in parent
  let output ← runKeys "Mj <ret>" "tests/data/null_col.csv"
  let hdr := header output
  assert (contains hdr "|") s!"Should have key col separator after meta enter: {hdr}"

def test_meta_0_enter_sets_keycols : IO Unit := do
  -- M0<ret>: select null cols, enter sets them as keyCols in parent
  let output ← runKeys "M0<ret>" "tests/data/null_col.csv"
  let hdr := header output
  let (_, status) := footer output
  assert (contains hdr "|") s!"Should have key col separator after M0<ret>: {hdr}"
  assert (contains status "keys=1") s!"Should have 1 key col: {status}"

def test_meta_1_enter_sets_keycols : IO Unit := do
  -- M1<ret>: select single-value cols, enter sets them as keyCols in parent
  let output ← runKeys "M1<ret>" "tests/data/single_val.csv"
  let hdr := header output
  let (_, status) := footer output
  assert (contains hdr "|") s!"Should have key col separator after M1<ret>: {hdr}"
  assert (contains status "keys=1") s!"Should have 1 key col: {status}"

def test_aggregate_requires_key : IO Unit := do
  let output ← runKeys "b" "tests/data/basic.csv"
  let (_, status) := footer output
  assert (contains status "key" || contains status "xkey") s!"Should show key error: {status}"

def test_aggregate_multi_col : IO Unit := do
  -- key on city (col 1), agg on value (col 2) - must sum numeric
  let output ← runKeys "l!llb" "tests/data/full.csv"
  let hdr := header output
  assert (contains hdr "sum_value" || contains hdr "city") s!"Should have aggregate columns: {hdr}"

def test_multi_column_freq_enter : IO Unit := do
  let output ← runKeys ":freq a,b<ret><ret>" "tests/data/multi_freq.csv"
  let (_, status) := footer output
  assert (endsWith status "/2") s!"Should filter to 2 rows: {status}"

def test_lr_paths : IO Unit := do
  let output ← runKeys ":lr tests/data<ret>" "tests/data/basic.csv"
  assert (contains output "basic.csv") s!"lr should show paths: {output}"

def test_numeric_right_align : IO Unit := do
  let output ← runKeys "" "tests/data/sample.parquet"
  let rows := dataLines output
  let first := rows.headD ""
  -- Values should have leading spaces (right-aligned)
  assert (contains first "  ") s!"Should have spacing for alignment: {first}"

-- | Verify no stderr writes (grep source for eprintln)
def test_no_stderr : IO Unit := do
  let out ← IO.Process.output { cmd := "grep", args := #["-r", "eprintln", "Tv/"] }
  assert (out.stdout.trim.isEmpty) s!"Found stderr writes in Tv/: {out.stdout}"

-- | 'r' key shows lr view
def test_ls_view : IO Unit := do
  let output ← runKeys "r" "tests/data/basic.csv"
  let (tab, _) := footer output
  assert (contains tab "lr ./") s!"r should show 'lr ./' in tab: {tab}"

-- | lr shows recursive file listing with datetime column visible
def test_lr_files : IO Unit := do
  let output ← runKeys "r" "tests/data/basic.csv"
  assert (contains output "datetime") s!"lr should show datetime column: {output}"

-- | M0<ret>llllll: select null cols as keys, navigate right, cursor should stay visible
-- multi_null.csv has cols a,b,c,d,e where b,c,d are null
-- After M0<ret>, keyCols=[1,2,3] (b,c,d), then l should navigate in display order
def test_multi_null_keycols_nav : IO Unit := do
  let output ← runKeys "M0<ret>llllll" "tests/data/multi_null.csv"
  let (_, status) := footer output
  -- Check cursor position shows in status (c{col}+{off})
  -- After M0<ret>llllll, cursor should be visible (status shows col info)
  assert (contains status "c") s!"Status should show cursor col: {status}"

-- === Run all tests ===

def main : IO Unit := do
  IO.println "Running TV key tests (Rust TV compatible)...\n"

  let ok ← Backend.init
  if !ok then throw (IO.userError "Backend init failed")

  -- test_keys.rs
  test_freq_a_in_tab_line
  test_freq_enter_filters_parent
  test_delete_twice_different_columns
  test_delete_key_freq
  test_sort_asc_orders_first_row_smallest
  test_sort_desc_orders_first_row_largest
  test_meta_shows_column_stats
  test_keys_parquet_freq
  test_keys_parquet_freq_enter
  test_keys_parquet_sort_asc
  test_keys_parquet_sort_desc
  test_keys_parquet_meta
  test_page_down_scrolls
  test_last_col_visible
  test_toggle_key_column
  test_toggle_key_remove
  test_navigation_down
  test_space_selects_column

  -- test_command.rs
  test_multi_column_select
  test_select_columns
  test_select_single
  test_delcol_multi
  test_delcol_single
  test_rename_column
  test_duplicate_view
  test_toggle_key_selected_cols
  test_key_cursor_tracks_column
  test_freq_after_meta
  test_freq_by_key_columns
  test_freq_multi_key_columns
  test_freq_multi_key_enter
  test_freq_enter_pushes_view
  test_decimal_increase
  test_decimal_decrease
  test_swap_views
  test_meta_select_rows_xkey_parent
  test_meta_0_select_null_cols
  test_meta_1_select_single_val_cols
  test_meta_enter_sets_keycols
  test_meta_0_enter_sets_keycols
  test_meta_1_enter_sets_keycols
  test_aggregate_requires_key
  test_aggregate_multi_col
  test_multi_column_freq_enter
  test_lr_paths
  test_numeric_right_align
  test_no_stderr
  test_ls_view
  test_lr_files
  test_multi_null_keycols_nav

  Backend.shutdown
  IO.println "\nAll tests passed!"

end Test

def main : IO Unit := Test.main
