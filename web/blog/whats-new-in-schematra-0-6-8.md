# What's New in Schematra 0.6.8

Schematra 0.6.8 is a small release, but it makes production logs much more useful. Access logs can now be emitted as structured JSON entries, which means status codes, methods, paths, and user agents are queryable as individual fields instead of being buried inside one Apache-style string.

This post also catches up on the smaller 0.6.x fixes that landed after the previous 0.6 write-up: body-parser edge cases, webhook testing improvements, multipart documentation, replayable bodies, and a few deployment fixes.

## Structured JSON Access Logs

Schematra has used the `logger` egg for framework logging since 0.6.0. Until now, access logs still looked like Spiffy's original access log line, even when `logger/format` was set to `'json`. That gave you JSON output, but the interesting request details were all inside the `message` string.

In 0.6.8, JSON access logs are structured:

<pre><code class="language-json">{
  "ts": 1777830331,
  "level": "info",
  "module": "schematra",
  "message": "request",
  "remote_addr": "127.0.0.1",
  "method": "GET",
  "uri": "http://localhost:8080/",
  "http_major": 1,
  "http_minor": 1,
  "response_code": 200,
  "referer": "-",
  "user_agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/147.0.0.0 Safari/537.36"
}
</code></pre>

That makes it much easier to answer operational questions like:

- Which routes are returning 500s?
- How many 404s did we serve in the last hour?
- Which methods are hitting a route?
- Which user agents are causing unusual traffic?

Text logs are unchanged. If `log-format` is `'text`, Schematra keeps the existing Spiffy-style access log line.

## Enabling JSON Logs

Pass `log-format: 'json` when starting the server:

<pre><code class="language-scheme">(schematra-start log-format: 'json)
</code></pre>

`SCHEMATRA_ENV` and `log-format` are separate knobs. `SCHEMATRA_ENV=production` controls production server behavior, but it does not automatically switch logs to JSON unless your app chooses to do that.

For the Schematra website, production now selects JSON logs explicitly:

<pre><code class="language-scheme">(schematra-start
 log-format: (if (equal? (get-environment-variable "SCHEMATRA_ENV") "production")
                 'json
                 'text))
</code></pre>

That keeps local development logs readable while making production logs queryable.

## User-Agent Handling

One detail was surprisingly easy to get wrong: `intarweb` parses the `User-Agent` header into a list of products, not a plain string. Passing that parsed structure straight to the JSON logger makes the JSON encoder try to treat it as structured data.

Schematra now formats that parsed value back into the familiar User-Agent string before logging it:

<pre><code class="language-scheme">(("Mozilla" "5.0" "Macintosh; Intel Mac OS X 10_15_7")
 ("AppleWebKit" "537.36" "KHTML, like Gecko")
 ("Chrome" "147.0.0.0" #f)
 ("Safari" "537.36" #f))
</code></pre>

becomes:

<pre><code>Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/147.0.0.0 Safari/537.36
</code></pre>

## Other 0.6.x Updates

The previous 0.6 post covered the big picture: logger integration, better exception reporting, webhook body capture, Server-Sent Events, and the oauthtoothy HTTP backend work. Since then, the 0.6 series has also picked up several smaller fixes and polish items:

- Route exceptions now preserve more context before Spiffy unwinds the stack, which makes development errors and production error logs easier to understand.
- A crash in the exception reporting path caused by `##sys#resize-trace-buffer` was removed.
- Raw request body buffering is available for webhook handlers that need to verify signatures against the exact incoming payload.
- `current-raw-body` is documented and available through `test-route`, so webhook signature tests can run without a live HTTP server.
- Requests without a `Content-Length` header no longer make the body parser wait for a body that is not there.
- Form body parsing now decodes `+` as a space, matching standard `application/x-www-form-urlencoded` behavior.
- Replayable request bodies let middleware and route handlers safely read the body more than once.
- Multipart body handling is documented more clearly, including when to use Schematra's body capture helpers instead of reading directly from the original request port.

## Upgrade Notes

- **0.6.0 → 0.6.8**: No breaking changes are expected for normal route handlers.
- **JSON logs**: Pass `log-format: 'json` to `schematra-start` if you want structured access logs.
- **Production apps**: `SCHEMATRA_ENV=production` does not imply JSON logging by itself. Set `log-format` explicitly.
- **User-Agent field**: JSON access logs now emit `user_agent` as a string, not the parsed intarweb structure.

---

*Feedback, bug reports, and pull requests welcome on [GitHub](https://github.com/schematra/schematra).*
