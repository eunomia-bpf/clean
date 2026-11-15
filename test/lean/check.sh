#!/bin/sh
# End-to-end check for the `clightgen -lean` backend.
#
# Regenerates demo.lean from demo.c using the locally built clightgen, then
# typechecks it against the Lean support library (export/Clightdefs.lean).
#
# Requirements:
#   - ../../clightgen  (build with: ./configure -clightgen <target>; make clightgen)
#   - lean 4 on PATH   (see export/lean-toolchain for the pinned version)

set -e
here=$(cd "$(dirname "$0")" && pwd)
root=$(cd "$here/../.." && pwd)
export="$root/export"

install -d "$export/.lake/build"

echo "[1/5] Generating demo.lean with clightgen -lean ..."
"$root/clightgen" -lean -normalize -o "$export/.lake/build/Demo_generated.lean" "$here/demo.c"

echo "[2/5] Building the Lean support libraries (CCLib + Clightdefs) ..."
( cd "$export" && lake build CCLib Clightdefs )

echo "[3/5] Typechecking the generated file ..."
( cd "$export" && LEAN_PATH=.lake/build/lib/lean lean .lake/build/Demo_generated.lean )

echo "[4/5] Exporting and typechecking a long statement sequence ..."
"$root/clightgen" -lean -normalize -o "$export/.lake/build/Deepseq_generated.lean" "$here/deepseq.c"
( cd "$export" && LEAN_PATH=.lake/build/lib/lean lean .lake/build/Deepseq_generated.lean )

echo "[5/5] Checking long local lists at the default recursion limits ..."
"$root/clightgen" -lean -normalize -o "$export/.lake/build/Longlocals_generated.lean" "$here/longlocals.c"
( cd "$export" && LEAN_PATH=.lake/build/lib/lean lean .lake/build/Longlocals_generated.lean -o .lake/build/Longlocals_generated.olean )
( cd "$export" && LEAN_PATH=.lake/build:.lake/build/lib/lean lean "$here/longlocals-check.lean" )

echo "OK: demo, deepseq and longlocals exports pass."
