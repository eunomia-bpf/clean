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

echo "[1/4] Generating demo.lean with clightgen -lean ..."
"$root/clightgen" -lean -normalize -o "$export/.lake/build/Demo_generated.lean" "$here/demo.c"

echo "[2/4] Building the Lean support libraries (CCLib + Clightdefs) ..."
( cd "$export" && lake build CCLib Clightdefs )

echo "[3/4] Typechecking the generated file ..."
( cd "$export" && LEAN_PATH=.lake/build/lib/lean lean .lake/build/Demo_generated.lean )

echo "[4/4] Exporting and typechecking a long statement sequence ..."
"$root/clightgen" -lean -normalize -o "$export/.lake/build/Deepseq_generated.lean" "$here/deepseq.c"
( cd "$export" && LEAN_PATH=.lake/build/lib/lean lean .lake/build/Deepseq_generated.lean )

echo "OK: demo and deepseq exports typecheck."
