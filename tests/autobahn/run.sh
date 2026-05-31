#!/usr/bin/env bash
# Run the Autobahn TestSuite fuzzingclient against a locally-running
# Schematra echo server.
#
# Prerequisites:
#   1. Docker running.
#   2. examples/websocket-echo.scm running on localhost:9001 in another
#      terminal:  csi -s examples/websocket-echo.scm
#
# Output:
#   tests/autobahn/reports/clients/index.html  (open in a browser)
#
# Excludes §12.* and §13.* (permessage-deflate is not implemented).
# §6.4.x cases (UTF-8 fail-fast across fragment boundaries) will report
# NON-STRICT — see README.md for why.

set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
mkdir -p "$here/reports"

# host.docker.internal resolves to the host on Docker Desktop (macOS,
# Windows) and on Linux with --add-host below.
docker run --rm \
  --add-host=host.docker.internal:host-gateway \
  -v "$here:/config" \
  -v "$here/reports:/reports" \
  crossbario/autobahn-testsuite \
  wstest -m fuzzingclient -s /config/fuzzingclient.json

echo
echo "Report: $here/reports/clients/index.html"
