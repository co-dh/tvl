#!/bin/bash
# Key tests for Lean TV
set -e
cd "$(dirname "$0")/.."

run() {
  TERM=xterm script -q -c "stty rows 24 cols 80; LD_LIBRARY_PATH=/usr/local/lib .lake/build/bin/tv '$2' --keys '$1'" /dev/null 2>&1
}

# Extract last line (status bar)
status() { echo "$1" | tail -1; }
# Extract first line (header)
header() { echo "$1" | head -1; }

PASS=0
FAIL=0

test_assert() {
  local name="$1" cond="$2"
  if eval "$cond"; then
    echo "✓ $name"
    ((PASS++))
  else
    echo "✗ $name"
    ((FAIL++))
  fi
}

# Test: F creates freq view with header
out=$(run "F" "tests/data/basic.csv")
test_assert "F creates freq view" '[[ "$out" == *"Cnt"* && "$out" == *"Pct"* ]]'

# Test: sort ascending
out=$(run "[" "tests/data/unsorted.csv")
first=$(echo "$out" | grep -E "^[0-9]" | head -1)
test_assert "[ sorts ascending (first=1)" '[[ "$first" == *" 1 "* ]]'

# Test: sort descending
out=$(run "]" "tests/data/unsorted.csv")
first=$(echo "$out" | grep -E "^[0-9]" | head -1)
test_assert "] sorts descending (first=3)" '[[ "$first" == *" 3 "* ]]'

# Test: M shows meta view
out=$(run "M" "tests/data/basic.csv")
test_assert "M shows meta view" '[[ "$out" == *"column"* || "$out" == *"distinct"* ]]'

# Test: D deletes column
out=$(run "lD" "tests/data/basic.csv")
hdr=$(header "$out")
test_assert "D deletes column b" '[[ "$hdr" != *" b "* ]]'

# Test: ! toggles key column
out=$(run "!" "tests/data/xkey.csv")
hdr=$(header "$out")
test_assert "! adds key column (shows |)" '[[ "$hdr" == *"|"* ]]'

# Test: !! removes key
out=$(run "!!" "tests/data/xkey.csv")
hdr=$(header "$out")
test_assert "!! removes key (no |)" '[[ "$hdr" != *"|"* ]]'

# Test: navigation (j moves down)
out=$(run "j" "tests/data/basic.csv")
stat=$(status "$out")
test_assert "j moves to row 2" '[[ "$stat" == *"2/5"* ]]'

# Test: space selects column
out=$(run " " "tests/data/basic.csv")
stat=$(status "$out")
test_assert "space selects column" '[[ "$stat" == *"*"* ]]'

echo ""
echo "Results: $PASS passed, $FAIL failed"
exit $FAIL
