# What's New in Schematra 0.7

Schematra 0.7 brings native WebSocket support to the framework. You can now write bidirectional, real-time endpoints using the same Sinatra-style syntax you already use for HTTP routes — no extra eggs, no separate process, no protocol gymnastics.

This release also reorganizes the framework into smaller modules. WebSocket support lives in a new `schematra.ws` module, and shared helpers (hex/byte conversion, HTTP header parsing, binary I/O) now live in `schematra.utils`. The core `schematra` module stays focused on routing and the request lifecycle.

## WebSockets

The new `websocket` macro registers a WebSocket route. Each clause names the event you want to handle and binds the relevant value:

<pre><code class="language-scheme">(import schematra schematra.ws)

(websocket "/echo"
  (on-open
   (send-text "welcome"))
  (on-text message
   (send-text (string-append "you said: " message)))
  (on-close code reason
   (void)))
</code></pre>

Under the hood, this performs the WebSocket upgrade handshake (validating headers, computing the `Sec-WebSocket-Accept` key), wraps the connection in a `websocket-connection` record, and runs the frame loop. Your handler bodies see only the decoded messages.

The macro is intentionally compositional with the rest of Schematra. You still have access to `current-request`, `current-params`, middleware (sessions, CSRF, oauth), and any other framework primitive — the upgrade just happens before your handler bodies run.

### Sending Messages

`send-text` and `send-binary` write a single frame to the current connection:

<pre><code class="language-scheme">(send-text "hello")
(send-binary (read-some-bytes))
</code></pre>

Both implicitly target `(current-websocket)`, the connection bound by the current handler. Sends are mutex-guarded per connection, so it is safe to call them from multiple threads as long as you parameterize `current-websocket` to the right connection.

### Broadcasting and Force-Close

Real applications usually need to push messages from outside a single handler — a chat room broadcasting to every client, a job system pushing progress updates, or an admin command kicking a misbehaving connection. Schematra makes this straightforward by exposing the connection as a first-class value.

Keep a registry of connected clients, then iterate over a snapshot when you want to broadcast:

<pre><code class="language-scheme">(define clients-mutex (make-mutex))
(define clients '())

(define (broadcast! message)
  (let ((snapshot (with-clients (lambda () clients))))
    (for-each
     (lambda (ws)
       (parameterize ((current-websocket ws))
         (condition-case (send-text message)
           (exn () (remove-client! ws)))))
     snapshot)))
</code></pre>

To force-close a connection from outside its handler thread, pass the connection explicitly to `close-websocket!`:

<pre><code class="language-scheme">;; Kick everyone except yourself.
(let ((me (current-websocket)))
  (for-each
   (lambda (ws)
     (unless (eq? ws me)
       (close-websocket! 1000 "kicked" ws)))
   (with-clients (lambda () clients))))
</code></pre>

`close-websocket!` sends the close frame, tears down the underlying input port so the read loop unblocks immediately, and the connection's `on-close` clause still runs for cleanup — even when the close came from another thread.

### Handler-Level Error Handling

Each WebSocket route can declare an `on-error` clause that receives the exception before the connection is closed:

<pre><code class="language-scheme">(websocket "/chat"
  (on-open ...)
  (on-text message ...)
  (on-close code reason ...)
  (on-error exn
   (log-error "websocket handler failed" exn)))
</code></pre>

If a handler raises an exception, `on-error` runs first (so your app can record the failure or notify other clients), then the framework sends a `1011` close frame, runs `on-close`, and lets the router log the original exception. This mirrors the way Schematra already handles route-level exceptions — `on-error` is the WebSocket analog of an error page.

### A Working Example

A complete multi-client chat demo lives in `examples/websocket-demo.scm`. It shows broadcasting to all connected clients, naming connections, handling disconnects, force-closing from outside the handler context, and the `on-error` clause. Run it the same way you'd run any Schematra app:

<pre><code class="language-bash">csi -s examples/websocket-demo.scm
</code></pre>

Open `http://localhost:8080` in multiple browser tabs and watch messages relay between them.

### RFC 6455 Conformance

The implementation is tested against the [Autobahn TestSuite](https://github.com/crossbario/autobahn-testsuite) — the same conformance suite the major WebSocket libraries publish results against. Schematra 0.7 passes **296 of 301 implemented cases strictly (98.3%)** across framing, fragmentation, control frames, UTF-8 handling, close handshakes, and the limit/performance sections.

The five non-strict results aren't correctness gaps. Two (§6.4.3, §6.4.4) test UTF-8 fail-fast when an invalid byte sequence is split across TCP writes inside a single frame: we read the full frame payload before running the UTF-8 DFA, so the close 1007 lands one buffer late by Autobahn's strict measure. The close handshake itself is RFC-correct in both cases (Autobahn reports `behaviorClose` as OK), and the worst-case buffering is capped by `websocket-max-frame-size` — the same cap that already applies to every well-formed frame. The remaining three are cases the RFC leaves undefined and Autobahn always marks INFORMATIONAL: §7.1.6 sends a large message followed immediately by a close (the spec doesn't pick a winner between finishing the write and processing the close first); §7.13.1 and §7.13.2 send close frames with out-of-range codes (5000, 65535) for which RFC 6455 doesn't define server behavior at all. permessage-deflate (§12, §13) is not implemented and is excluded from the run.

Reproducing the results locally takes one Docker container — see `tests/autobahn/README.md`.

## Module Reorganization

Schematra 0.7 splits the framework into three modules:

- `schematra` — routing, request/response, middleware, sessions, SSE.
- `schematra.ws` — WebSocket upgrade, frame parser/writer, `send-text`, `close-websocket!`, the `websocket` macro.
- `schematra.utils` — internal helpers: hex conversion, header value parsing, binary I/O. Mostly framework-internal but exported so extension modules can use them.

If you don't use WebSockets, nothing changes. If you do, add the import:

<pre><code class="language-scheme">(import schematra schematra.ws)
</code></pre>

The split keeps the core `schematra` module focused — you're not pulling in `base64`, `simple-sha1`, and `chicken.bitwise` for a plain HTTP app.

### Long-Connection Primitives Are Now Public

To make `schematra.ws` work as a separate module, the long-connection primitives that SSE has used internally are now exported from `schematra`:

- `current-long-connection` — parameter holding the current long-lived connection.
- `start-long-connection!` — converts the current request into a long-lived connection.
- `long-connection-input-port`, `long-connection-output-port`, `long-connection-response-set!` — accessors.

Most applications won't touch these directly, but they're available if you want to build your own long-lived protocol on top of Schematra's router (a custom binary protocol, a streaming RPC, MQTT-over-WebSocket, etc.).

## Smaller Improvements

A handful of smaller changes shipped alongside the WebSocket work:

- **SSE responses now propagate cookies.** Setting a cookie during an SSE handler used to silently drop the `Set-Cookie` header because the response was constructed from a fresh header list. 0.7 includes `(cookies->alist (response-cookies))` and the existing response headers when starting the SSE stream, so `set-cookie!` works on SSE endpoints. If you were writing cookie headers manually as a workaround, you can delete that code.
- **`csi -s app.scm` now blocks on `schematra-start`.** A new `running-interactively?` check inspects `(argv)` for script-mode flags (`-s`, `-ss`, `-script`, `-b`, `-batch`). In script mode `schematra-start` blocks; in a true `csi` REPL it still spawns a background server thread so the prompt stays responsive. The net effect: `csi -s app.scm` no longer falls off the end of the script before the server binds — manual `thread-join!` keep-alive loops can go away.

## Upgrade Notes: 0.6.8 → 0.7

No code changes are required for existing HTTP apps — routing, middleware, sessions, body parsing, SSE, and chiccup are source-compatible. The mechanical steps to upgrade:

- **Refresh dependencies.** The new WebSocket accept-key code pulls in `simple-sha1`. Re-run `chicken-install $(cat deps.txt)`, or add `simple-sha1` to your pinned list.
- **Reinstall the egg.** The egg now ships four extensions (`schematra`, `schematra.utils`, `schematra.ws`, `schematra.ws.internal`) instead of just `schematra`. A `cd eggs/schematra && chicken-install` rebuilds the whole set.
- **Import `schematra.ws` if you want WebSockets.** Plain HTTP apps don't need it.

No public API was removed or renamed. If something stopped working after the upgrade, it's a regression — please file an issue.

---

*Feedback, bug reports, and pull requests welcome on [GitHub](https://github.com/schematra/schematra).*
