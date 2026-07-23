#!/bin/sh
# Full gate for Nonconformity: engine, contrast, sweeps, DOM, and a live boot.
set -e
cd "$(dirname "$0")/.."
echo "== engine =="
Rscript tests/test_engine.R
echo "== contrast =="
Rscript tests/test_contrast.R
echo "== sweeps =="
Rscript tests/test_sweeps.R
echo "== dom =="
NODE_PATH=/home/claude/node_modules node tests/test_dom.js
echo "== live boot =="
Rscript -e 'shiny::runApp(".", port = 8123, launch.browser = FALSE)' > /tmp/nc_boot.log 2>&1 &
PID=$!
sleep 6
if curl -s http://127.0.0.1:8123 | grep -q "Nonconformity"; then
  echo "pass  app boots and serves the page"
else
  echo "FAIL  app did not serve"; kill $PID 2>/dev/null; exit 1
fi
kill $PID 2>/dev/null
echo "all suites passed"
