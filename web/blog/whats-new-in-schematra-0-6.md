# What's New in Schematra 0.6

Schematra 0.5 and 0.6 landed quietly but brought several features that have meaningfully changed how real apps are built with the framework. This post covers everything that's new since the 0.4 release: webhook signature verification, Server-Sent Events, better exception handling, testing improvements, and a new HTTP client backend in oauthtoothy.

## Webhook Signature Verification (0.6.2–0.6.3)

If you're receiving webhooks from GitHub, Stripe, or any other provider that signs payloads with HMAC-SHA256, you need to verify the signature against the *exact bytes* that were sent — not a parsed or re-serialized version of the body. This is why body parsers that eagerly decode the payload before your handler runs are a problem.

Schematra's `body-parser-middleware` now captures the request body and exposes it via `current-request-body`, so your signature check sees precisely what arrived on the wire:

<pre><code class="language-scheme">(import schematra schematra.body-parser hmac sha2 message-digest)

(use-middleware! (body-parser-middleware))

(define (hmac-sha256-hex key data)
  (message-digest->string
   (hmac-message-digest (open-input-string key)
                        sha256-primitive
                        (open-input-string data) byte)))

(post "/webhook"
  (let* ((raw (request-body-string (current-request-body)))
         (sig (alist-ref "x-hub-signature-256"
                         (request-headers (current-request)) equal?))
         (expected (string-append "sha256="
                                  (hmac-sha256-hex
                                   (get-environment-variable "WEBHOOK_SECRET")
                                   raw))))
    (if (and sig (string=? sig expected))
        (begin
          (process-event! (read-json raw))
          '(ok "Received"))
        '(forbidden "Invalid signature"))))
</code></pre>

`current-request-body` returns a replayable request body object regardless of content-type. Use `request-body-string` when you need the exact body bytes as a string. The parsed parameters (via `current-params`) remain available as usual.

### Testing Webhook Routes Without a Server

`test-route` passes the request body through middleware when you pass a `body:` argument, so your webhook tests don't need an actual HTTP server:

<pre><code class="language-scheme">(import test schematra schematra.test)

(define secret "test-secret")
(define payload "{\"action\":\"push\"}")
(define valid-sig
  (string-append "sha256=" (hmac-sha256-hex secret payload)))

(test "rejects missing signature"
  'forbidden
  (test-route-status webhook-app 'POST "/webhook"
                     body: payload))

(test "accepts valid signature"
  'ok
  (test-route-status webhook-app 'POST "/webhook"
                     body: payload
                     headers: `((x-hub-signature-256 . ,valid-sig))))
</code></pre>

Tests run in milliseconds with no network overhead and no mock servers to maintain.

## Server-Sent Events

Schematra now exports `sse` and `write-sse-data` for building streaming endpoints. SSE is a great fit for real-time dashboards, live logs, and progress indicators — anything where the server pushes updates to the browser without a WebSocket.

<pre><code class="language-scheme">(import schematra)

(get "/events"
  (sse
   (lambda (send!)
     (let loop ((n 0))
       (send! `((event . "tick")
                (data  . ,(number->string n))))
       (thread-sleep! 1)
       (when (&lt; n 10)
         (loop (+ n 1)))))))
</code></pre>

`sse` sets the correct `Content-Type: text/event-stream` header and keeps the connection open while your callback runs. `write-sse-data` is the lower-level primitive if you want to manage the response port yourself.

On the browser side, it's the standard `EventSource` API:

<pre><code class="language-javascript">const es = new EventSource("/events");
es.addEventListener("tick", e =&gt; console.log(e.data));
</code></pre>

A working SSE demo lives in `examples/sse-demo/`.

## Better Exception Reporting

Before 0.5, uncaught exceptions in route handlers were sometimes swallowed or reported with a truncated call chain — Spiffy's internal error handler would unwind the stack before Schematra could capture the full trace.

The framework now captures the exception and its call chain *inside* the handler, before control returns to Spiffy. The result is cleaner error pages in development mode and more complete log output in production.

No API changes are required. If you were catching exceptions manually with `condition-case`, your code continues to work as before.

## oauthtoothy 0.3.x: http-curl Backend and Retry Logic

The oauthtoothy egg gained a significant under-the-hood overhaul in 0.3.

### http-curl Backend

oauthtoothy previously depended on `http-client` for all outbound OAuth requests. It now supports `http-curl` as an alternative backend, which avoids the OpenSSL dependency on systems where curl is already available:

<pre><code class="language-scheme">;; Use the default http-client backend (unchanged)
(use-middleware!
 (oauthtoothy-middleware providers http-client: #t))

;; Use http-curl instead
(use-middleware!
 (oauthtoothy-middleware providers http-curl: #t))
</code></pre>

The Docker image now ships with `curl-dev` so the http-curl egg installs cleanly in containers.

### Configurable HTTP Client Parameters

Timeouts, SSL verification, and custom headers can now be passed through to the underlying HTTP client. This is useful when your OAuth provider is behind a corporate proxy or has unusual certificate configurations.

### Retry Logic on Token Exchange

Token exchange requests (the step where the authorization code is swapped for an access token) now retry automatically on transient network errors. The default is two attempts with a short backoff. This eliminates the rare-but-annoying "OAuth failed because of a momentary network hiccup" errors in production.

## Upgrade Notes

- **0.4 → 0.5**: No breaking changes. Add `(import schematra.body-parser)` if you want request body parsing.
- **0.5 → 0.6**: No breaking changes. Request bodies are available through `current-request-body` when `body-parser-middleware` is installed.
- **oauthtoothy 0.2 → 0.3**: The middleware API is unchanged. The http-client backend is selected by default so existing code continues to work.

---

*Feedback, bug reports, and pull requests welcome on [GitHub](https://github.com/schematra/schematra).*
