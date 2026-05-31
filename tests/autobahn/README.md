# Autobahn TestSuite — Schematra WebSocket conformance

[Autobahn TestSuite](https://github.com/crossbario/autobahn-testsuite) is the
de-facto RFC 6455 conformance suite. We run it in `fuzzingclient` mode:
Autobahn drives our server through ~500 framing, fragmentation, UTF-8,
close-handling, and performance cases and produces an HTML report.

## Running

1. **Start the echo server** in one terminal — either compiled or via csi:

   ```sh
   csc -O2 -d0 examples/websocket-echo.scm && examples/websocket-echo
   # or:
   csi -s examples/websocket-echo.scm
   ```

   Both modes block the main thread; only an interactive csi REPL spawns
   the server in a background thread (see `running-interactively?` in
   `schematra.scm`). The example listens on `ws://localhost:9001/` and
   echoes text/binary frames with the same opcode. It also raises
   `websocket-max-frame-size`, `websocket-max-message-size`, and
   `websocket-max-fragment-count` well above our production defaults so
   §9 (limit) cases test the protocol, not our DoS knobs.

2. **Run Autobahn** in another terminal (requires Docker):

   ```sh
   tests/autobahn/run.sh
   ```

3. **Open the report**:

   ```sh
   open tests/autobahn/reports/clients/index.html
   ```

   Each case lists OK / NON-STRICT / INFORMATIONAL / FAILED.

## Excluded cases

- **§12.* / §13.\*** — permessage-deflate (RFC 7692). Not implemented. The
  config excludes them so the report isn't drowned in expected fails.

## Conformance summary

Strict pass: **296/301 = 98.3%** across all implemented sections
(§1–§10). Sections §12–§13 are excluded (permessage-deflate, not
implemented). The remaining gap:

- **§6.4.3, §6.4.4** — NON-STRICT. Two cases that chop an invalid
  UTF-8 sequence across TCP writes *inside a single WebSocket frame*
  and expect rejection before the rest of the frame arrives. We read
  the full frame payload before validating, so the close 1007 lands
  late by Autobahn's measure (`behaviorClose` is still OK — the close
  handshake itself is correct). Bounded by `websocket-max-frame-size`,
  so the buffering cost is the same as any legitimate frame at the
  same size. Fixing requires chunked payload reads with an
  incremental DFA inside `read-frame-from-port`.
- **§7.1.6** — INFORMATIONAL. Sends a 256 KiB text message, then a
  close, then a ping back-to-back. RFC 6455 doesn't pick a winner
  between "finish the write, then process the close" and "process the
  close, drop the in-flight write"; Autobahn marks the case
  INFORMATIONAL either way. Schematra picks up the close first.
- **§7.13.1, §7.13.2** — INFORMATIONAL. Sends a close frame with an
  out-of-range code (5000, 65535). The RFC doesn't define server
  behavior for invalid close codes — Autobahn's expectation field
  literally reads *"Actual events are undefined by the spec."*

§6.4.1 and §6.4.2 (UTF-8 fail-fast *across fragment boundaries*) now
pass strictly — the fragmentation state machine threads a Höhrmann
UTF-8 DFA across frames and rejects with 1007 at the offending byte.

## Iterating

To narrow the run while debugging a specific area, edit
`fuzzingclient.json` and change `"cases"`:

```json
"cases": ["5.*"]              // just fragmentation
"cases": ["6.4.*"]            // just UTF-8 fail-fast
"cases": ["1.*", "2.*", "3.*"] // framing + ping/pong + reserved bits
```

The `reports/` directory is gitignored; reports are regenerated on every
run.
