#!/bin/sh
# Full gate for Nonconformity: engine, server, contrast, sweeps, DOM, and a
# live boot. Run from anywhere; the script moves to the repository root.
set -e
cd "$(dirname "$0")/.."

# The DOM suite needs jsdom. A NODE_PATH set by the caller wins, then a
# node_modules folder in the repository root, then the global npm folder.
if [ -z "$NODE_PATH" ]; then
  if [ -d node_modules/jsdom ]; then
    NODE_PATH="$(pwd)/node_modules"
  else
    NODE_PATH="$(npm root -g 2>/dev/null || true)"
  fi
fi
export NODE_PATH
if ! node -e "require('jsdom')" >/dev/null 2>&1; then
  echo "jsdom is not installed. From the repository root run:"
  echo "  npm install --no-save jsdom"
  exit 1
fi

echo "== engine =="
Rscript tests/test_engine.R
echo "== server =="
Rscript tests/test_server.R
echo "== contrast =="
Rscript tests/test_contrast.R
echo "== sweeps =="
Rscript tests/test_sweeps.R
echo "== dom =="
node tests/test_dom.js

echo "== live boot =="
LOG="$(mktemp)"
Rscript -e 'shiny::runApp(".", port = 8123, launch.browser = FALSE)' > "$LOG" 2>&1 &
PID=$!
# Poll rather than sleep a fixed time, so a slow machine is not reported
# as a broken app and a fast one does not wait.
tries=0
until curl -s http://127.0.0.1:8123 | grep -q "Nonconformity"; do
  tries=$((tries + 1))
  if [ "$tries" -ge 30 ]; then
    echo "FAIL  app did not serve within 30 seconds. Log follows:"
    cat "$LOG"; kill "$PID" 2>/dev/null; rm -f "$LOG"; exit 1
  fi
  sleep 1
done
echo "pass  app boots and serves the page"
kill "$PID" 2>/dev/null; rm -f "$LOG"
echo "all suites passed"
