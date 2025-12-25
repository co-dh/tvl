#!/bin/bash
# Run tv with --keys and output clean buffer
# Usage: ./run_keys.sh "keys" "file"
KEYS="${1:-}"
FILE="${2:-tests/data/basic.csv}"
TERM=xterm script -q -c "stty rows 24 cols 80; LD_LIBRARY_PATH=/usr/local/lib .lake/build/bin/tv '$FILE' --keys '$KEYS'" /dev/null 2>&1
