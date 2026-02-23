#!/usr/bin/env sh
set -eu

SCRIPT_DIR="$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)"
SRC="$SCRIPT_DIR/src/bern_http.c"
OUT_DIR="$SCRIPT_DIR/linux"
OUT_SO="$OUT_DIR/libbern_http.so"

mkdir -p "$OUT_DIR"
cc -shared -fPIC -O2 "$SRC" -o "$OUT_SO"

echo "Built $OUT_SO"
