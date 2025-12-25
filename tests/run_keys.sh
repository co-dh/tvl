#!/bin/bash
# Run tv with --keys and output clean buffer
# Usage: ./run_keys.sh "keys" "file"
# Note: Use this script to avoid Claude Code Bash tool escaping '!'
cd "$(dirname "$0")/.."
KEYS="${1:-}"
FILE="${2:-tests/data/basic.csv}"
TERM=xterm script -q -c "stty rows 24 cols 80; .lake/build/bin/tv --keys '$KEYS' '$FILE'" /dev/null 2>&1 | ansi2txt
