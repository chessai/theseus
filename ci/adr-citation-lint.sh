#!/usr/bin/env bash
# ADR-0021: every ADR must be cited at least once outside docs/src/adr/.
# Uncited decisions are either undocumented in the design doc, or dead.
set -euo pipefail
fail=0
for adr in docs/src/adr/[0-9]*.md; do
  n=$(basename "$adr" | cut -d- -f1)
  if ! grep -rql "ADR-$n" --exclude-dir=adr --exclude-dir=book --exclude-dir=target .; then
    echo "uncited: ADR-$n ($adr)"; fail=1
  fi
done
exit $fail
