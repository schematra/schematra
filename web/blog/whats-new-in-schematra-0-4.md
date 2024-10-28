<img class="mx-auto" data-ai-model="imagefx" data-ai-prompt="software testing laboratory with computer screens showing Scheme code and test results, mechanical testing equipment transformed into code testing tools, clean modern workspace, Lisp parentheses floating in air like molecular structures, test tubes containing glowing green checkmarks and red X marks, whiteboard with flow diagrams and lambda symbols, performance graphs on monitors showing upward trends, version 0.4 subtly visible on computer screen, warm professional lighting, technical illustration style, programming meets science aesthetic, organized and methodical workspace. Subject in the wall somewhere with the text Schematra" style="width: 80%" src="https://s3-us-west-1.amazonaws.com/assets.schematra.com/public/images/testing-lab-04.jpg" alt="Testing laboratory meets code" title="Testing laboratory - AI Generated, look at the source for details">

<p class="text-xs text-center">(A testing laboratory where Scheme code gets validated - generated with ImageFX, manually edited after. Oct 25 2025, look at source for details)</p>

# What's New in Schematra 0.4

Schematra 0.4 is here, and it brings some significant improvements to testing ergonomics, API consistency, and performance insights. This release is all about making your development experience smoother while keeping the framework lightweight and fast. The version jumped to 0.4 because there's a small breaking change in the API, more on that below.

**Quick reminder**: Schematra is still evolving. The API is in flux as we refine it based on real-world usage. If something breaks between versions, that's intentional: the prioritization is to get the design right over backwards compatibility at this stage.

## Making Testing Better

Testing web apps should be easy. In previous versions, testing Schematra routes required a lot of boilerplate - creating mock requests, mock responses, and manually wiring everything together. Even if you were using chiccup to create your DOM, when running tests you would need to clumsily test using regex, try to parse the HTML or do something similar. Version 0.4 introduces the new `schematra.test` module that makes testing a one-liner, as well as the ability for routes to return **ccup tuples**.

### Before (0.3)

<pre><code class="language-scheme">(import test schematra spiffy uri-common intarweb)

(test "GET /hello returns greeting"
  '(ok (ccup [h1 "Hello"]) ())
  (with-schematra-app test-app
    ((current-request (make-request method: 'GET
                                   uri: (uri-reference "/hello")
                                   port: (open-input-string "")
                                   headers: (headers '())))
     (current-response (make-response port: (open-output-string)
                                      headers: (headers '()))))
    (lambda ()
      (schematra-route-request (current-request)))))
</code></pre>

### After (0.4)

<pre><code class="language-scheme">(import test schematra schematra.test)

(test "GET /hello returns greeting"
  '(ok (ccup [h1 "Hello"]) ())
  (test-route test-app 'GET "/hello"))
</code></pre>

The `schematra.test` module provides several convenience helpers:

- `test-route` - Returns the full response tuple `(status body headers)`
- `test-route-status` - Extracts just the status symbol
- `test-route-body` - Extracts just the body content
- `test-route-headers` - Extracts just the headers

All helpers support keyword arguments for headers and body:

<pre><code class="language-scheme">;; POST with JSON body
(test-route test-app 'POST "/api/users"
           body: "{\"name\":\"Alice\"}"
           headers: '((content-type . "application/json")))

;; Just check the status
(test-route-status test-app 'GET "/hello")  ;; => 'ok
</code></pre>

## "Native" Chiccup rendering, for better testing and composition

Routes can now return chiccup directly without calling `ccup->html`. This might seem like a small change, but it has huge implications for testing and composition.

<pre><code class="language-scheme">;; Before: had to convert to HTML yourself
(get "/hello"
     (ccup->html `[h1 "Hello, World!"]))

;; After: just return the chiccup
(get "/hello"
     '(ccup [h1 "Hello, World!"]))
</code></pre>

### Why This Matters: Structural Testing

Your tests can now assert against the *structure* of your responses instead of parsing HTML strings:

<pre><code class="language-scheme">;; Test the chiccup structure directly
(test "returns correct structure"
  '(ccup [h1 "Hello, World!"])
  (test-route-body test-app 'GET "/hello"))

;; No more string comparisons or HTML parsing!
</code></pre>

### Middleware Composition: The Real Power

The framework automatically detects chiccup responses (marked with the `ccup` sentinel symbol) and renders them to HTML when sending the HTTP response. But here's the interesting part: **middleware runs before that conversion happens**. This means middleware can inspect and transform the chiccup structure before it becomes HTML.

Want to automatically inject CSRF tokens into every form? Easy with `sxml-transforms`:

<pre><code class="language-scheme">(import sxml-transforms)

(define (csrf-injection-middleware next)
  (let ((result (next)))
    (if (and (list? result) (eq? (car result) 'ccup))
        (let ((csrf-token (session-get "csrf-token")))
          `(ccup ,(inject-csrf-tokens (cadr result) csrf-token)))
        result)))

(define (inject-csrf-tokens sxml-tree token)
  (pre-post-order* sxml-tree
    `((form . ,(lambda (tag attrs . children)
                 ;; Inject CSRF token as first child of form
                 `(form ,attrs
                        (input (@ (type "hidden")
                                  (name "csrf_token")
                                  (value ,token)))
                        ,@children)))
      ;; Default: recurse into all other elements unchanged
      (*default* . ,(lambda (tag . children) (cons tag children)))
      (*text* . ,(lambda (_ txt) txt)))))
</code></pre>

Now every form in your app automatically gets CSRF protection without touching your route handlers:

<pre><code class="language-scheme">;; Your route just returns plain chiccup
(post "/contact"
      '(ccup [form (@ (method "POST") (action "/submit"))
                   [input (@ (type "text") (name "email"))]
                   [button "Submit"]]))

;; Middleware automatically transforms it to:
'(ccup [form (@ (method "POST") (action "/submit"))
             [input (@ (type "hidden") (name "csrf_token") (value "abc123"))]
             [input (@ (type "text") (name "email"))]
             [button "Submit"]])
</code></pre>

Other useful transformations:

* Wrap all pages in a consistent layout
* Add analytics tracking to links
* Inject user-specific data into templates
* Apply XSS sanitization to user content
* Add accessibility attributes

Your routes stay clean and focused on business logic. Cross-cutting concerns live where they belong: in middleware.

## API Improvements

The `with-schematra-app` macro now follows standard Scheme conventions by taking a thunk (a function with no arguments) instead of accepting arbitrary body expressions. This aligns it with other `with-*` forms in Scheme like `with-input-from-file`.

### Migration Guide

<pre><code class="language-scheme">;; Before
(with-schematra-app app
  (get "/hello" "Hello")
  (post "/echo" ...))

;; After - wrap in a lambda
(with-schematra-app app
  (lambda ()
    (get "/hello" "Hello")
    (post "/echo" ...)))
</code></pre>

As a bonus, the macro now supports injecting parameters for testing:

<pre><code class="language-scheme">;; Inject mock request/response for testing
(with-schematra-app test-app
  ((current-request (make-mock-request 'GET "/hello"))
   (current-response (make-mock-response)))
  (lambda ()
    (schematra-route-request (current-request))))
</code></pre>

Though honestly, you should just use `test-route` instead - it's much cleaner!

## Performance Deep Dive: Why Chiccup Doesn't Need Caching

I was curious whether chiccup's HTML generation was fast enough for production use, or if we needed to implement template caching. So I built a quick benchmark suite to find out.

The results? **Chiccup is plenty fast without caching.**

### The Numbers

Running compiled code (`csc -O2`) on various template complexities, on my M1 macbook:

- **Simple elements**: 339,000 ops/sec (~3μs each)
- **Complex nested structures**: 14,000 ops/sec (~71μs each)
- **Table with 50 rows**: 1,990 ops/sec (~502μs each)
- **Average across all tests**: ~145,000 ops/sec

Even the worst case (a 50-row table) takes half a millisecond to render. For comparison, a typical database query takes 5-50ms, and network latency is measured in tens or hundreds of milliseconds.

### What This Means

The bottleneck in your web app is **not** HTML generation. It's:

- Database queries
- External API calls
- Business logic
- Network I/O

Chiccup rendering is fast enough that caching would add complexity for no real benefit. Focus your optimization efforts where they matter.

The full benchmark suite is in the repo at `benchmarks/chiccup_benchmark.scm` if you want to run it yourself. Please send me a PR if you think we're missing a critical test that might reflect a more real-world scenario.

## What's Coming Next

I'm working on some exciting features that are already being tested in pre-production apps. Here's a sneak peek:

### Async Job System

A simple, Redis-backed job queue that's been running in production for a few weeks now. It still needs work on resiliency and priority handling, but the core is solid:

<pre><code class="language-scheme">;; Register a job handler
(ds/register-job 'send-email
  (lambda (session-id to subject body)
    (send-mail to subject body)))

;; Schedule a job
(ds/add-job 'send-email '("user@example.com" "Welcome!" "..."))

;; Or schedule for later (60 seconds from now)
(ds/add-job 'send-email '("user@example.com" "Reminder" "...") 60)
</code></pre>

You can spawn multiple workers, and jobs can spawn other jobs (manual sequencing). I'm exploring more explicit task dependencies where outputs can feed into other tasks, but I'm biasing toward simplicity.

### ORM (Very Early Preview)

I'm building a lightweight ORM on top of `ssql` and `rqlite`. The API is still being refined, but here's where it's at:

<pre><code class="language-scheme">
;; Define models from your database schema
(define-model users)
(define-model posts)

;; Find records
(users/find '(= email "foo@bar.com"))  ;; => user alist or #f

;; Query with conditions. Find all users that were created more than hour ago.
;; uses an interval helper to create the timestamp
(users/where '(&lt; created-at ?)
            `(,(ago (hours 1)))
            order: '(desc created-at)
            limit: 20)  ;; => vector of user alists

;; Relationships
(model/has-many users posts)

;; Update records, borrowing thrush combinator (->>) from clojurian egg
(users/save
  (->> user-row
       (alist-update 'email "new@email.com")
       (alist-update 'name "Rolando")))

;; Models are just functions on alists, so they are easily extended:
(define (users/send-welcome-email user-row)
  (let ((email (alist-ref 'email user-row)))
    (send-email email "Welcome!" "...")))
</code></pre>

Oh, and it also supports migrations. The schema modification format though, is something I'm not super happy about yet. Migrations are just a name/id and two lambdas (up & down). The name/id is irrelevant, they are sequenced as defined. That's how we can migrate up and down.

<pre><code class="language-scheme">
(model/migration
 "0001_add_initial_tables"
 ;; up
 (lambda ()
   (model/schema/create-table 'users
			      '(id integer (primary-key) (autoincrement))
			      '(provider_id string (not-null #t))
			      '(provider_name string (not-null #t))
			      '(name string (not-null #t))
			      '(email string (unique #t))
			      '(profile_image_url string)
			      '(auth_token string)
			      '(created_at datetime (default CURRENT_TIMESTAMP))
			      '(updated_at datetime (default CURRENT_TIMESTAMP)))
   (rqlite/execute "CREATE UNIQUE INDEX users_provider_id_provider ON users (provider_id, provider_name)"))
 ;; down
 (lambda ()
   (rqlite/execute "DROP INDEX users_provider_id_provider")
   (model/schema/drop-table 'users)))
   
;; to migrate to the latest version:
(model/migrate)
;; or to roll-back/forward to a specific version (will run the "down" block for each migration, if needed)
(model/migrate "0001_add_initial_tables")
</code></pre>

Why `rqlite`? I'm a PostgreSQL fan, but I've been genuinely surprised by rqlite's simplicity and performance for smaller apps. It's SQLite with raft consensus - dead simple to deploy, easy to backup, and good enough for most use cases.

### Debugging Guide

I'm writing a post on debugging Schematra apps - profiling, logging, error handling, the works. Should be ready soon.

## Try It Out!

Ready to give Schematra 0.4 a spin?

**Get started**: Check out the [documentation](https://schematra.com) or dive into the [source code on GitHub](https://github.com/schematra/schematra).

**Found a bug? Have an idea?** Open an issue or send a PR - we'd love your feedback!

The testing improvements alone make this version worth upgrading to. And if you've been wondering whether chiccup performance is good enough - wonder no more, the benchmarks speak for themselves.

---

*Note: Schematra is still in active development. The API may change as we refine the framework based on real-world use. Follow along on GitHub for the latest updates.*
