#!/bin/sh
# Rebuild the four Fellowship variants of this report from a Fellowship
# source directory (default: wrap/fellowship of the current checkout) into
# OUT (default: bench/perf-2026-11/builds-local, not tracked).
#   bench/perf-2026-11/scripts/build.sh [SRC] [OUT]
set -e
SRC=${1:-wrap/fellowship}
OUT=${2:-bench/perf-2026-11/builds-local}
PATCH=$(cd "$(dirname "$0")/.." && pwd)/core-pathcopy.patch
WORK=$(mktemp -d)
mkdir -p "$OUT"
cp -R "$SRC"/. "$WORK"/
( cd "$WORK" && make clean >/dev/null 2>&1 || true
  make >/dev/null && cp fsp fsp-bytecode
  make clean >/dev/null && make nc >/dev/null && cp fsp fsp-native
  patch -p3 < "$PATCH" >/dev/null
  make clean >/dev/null && make >/dev/null && cp fsp fsp-bytecode-pathcopy
  make clean >/dev/null && make nc >/dev/null && cp fsp fsp-native-pathcopy )
cp "$WORK"/fsp-bytecode "$WORK"/fsp-native "$WORK"/fsp-bytecode-pathcopy "$WORK"/fsp-native-pathcopy "$OUT"/
rm -rf "$WORK"
ls -la "$OUT"
