# Schematra Documentation

A modern web framework for Scheme that combines simplicity with power.

## Table of Contents

- [What is Schematra?](#what-is-schematra)
- [Quick Reference](#quick-reference)
- [Getting Started](#getting-started)
- [Core Concepts](#core-concepts)
- [Chiccup Templating](#chiccup-templating)
- [Middleware System](#middleware-system)
- [API Reference](#api-reference)
- [Advanced Topics](#advanced-topics)
- [Examples & Recipes](#examples--recipes)
- [Contributing](#contributing)

## What is Schematra?

Schematra is a lightweight, expressive web framework for CHICKEN Scheme inspired by Sinatra. It brings the elegance of Lisp to web development with a focus on simplicity and developer productivity.

### Key Features

- **Minimal boilerplate** - Get a web server running with just a few lines of code
- **Chiccup templating** - Write HTML using Lisp syntax with built-in CSS class support
- **Flexible routing** - Sinatra-style route definitions with URL parameters
- **Built-in sessions** - Cookie-based session management out of the box
- **Middleware support** - Extensible request/response processing pipeline
- **Static file serving** - Serve CSS, JavaScript, and assets effortlessly

### Who is it for?

Schematra is perfect for:
- **Scheme enthusiasts** who want to build web applications in their favorite language
- **Lisp developers** looking for a modern web framework with functional programming principles
- **Rapid prototyping** when you need to quickly spin up web services or small applications
- **Educational projects** teaching web development concepts in a functional language

## Quick Reference

### Basic App Structure
```scheme
(import schematra chiccup)

;; Define routes
(get ("/path") (ccup/html `[h1 "Hello"]))
(post ("/submit") "Form submitted!")

;; Start server
(schematra-install)
(schematra-start)
```

### Common Route Patterns
```scheme
;; Simple routes
(get ("/") "Home page")
(get ("/about") (ccup/html `[h1 "About Us"]))

;; URL parameters
(get ("/user/:id") 
     (let ((id (alist-ref "id" (current-params) equal?)))
       (ccup/html `[h1 ,(string-append "User " id)])))

;; Query parameters
(get ("/search")
     (let ((q (alist-ref 'q (current-params))))
       (if q (format "Searching for: ~A" q) "No query")))
```

### Response Types
```scheme
;; String response (200 OK)
"Simple text response"

;; Status + body
'(created "Resource created")

;; Status + body + headers
'(ok "{\"status\":\"success\"}" ((content-type application/json)))

;; Redirects
(redirect "/login")
(redirect "/new-url" 'moved-permanently)

;; Early exit
(halt 'not-found "Page not found")
```

### Chiccup HTML Basics
```scheme
;; Basic elements
`[h1 "Title"]
`[p "Paragraph text"]

;; CSS classes and IDs
`[.container.mx-auto#main "Content"]
`[button.btn.btn-primary "Click me"]

;; Attributes
`[a ((href . "/page")) "Link"]
`[input ((type . "text") (name . "username"))]

;; Dynamic content
`[ul ,@(map (lambda (item) `[li ,item]) items)]

;; Raw HTML (unescaped)
`[div (raw "<em>emphasized</em>")]
```

### Sessions
```scheme
(import sessions)
(use-middleware! (session-middleware "secret-key"))

;; Get/set session data
(session-get "user-id")
(session-set! "user-id" "12345")
(session-delete! "key")
(session-destroy!)
```

### Static Files
```scheme
;; Serve files from ./public at /assets
(static "/assets" "./public")
```

## Getting Started

### System Requirements

- CHICKEN Scheme 5.0 or later
- Required eggs: json, spiffy, format, openssl, message-digest, hmac,
  sha2, base64, http-client, medea, nrepl, srfi-1, srfi-13, srfi-18,
  srfi-69

### Installation

1. Clone the Schematra repository:
   ```bash
   git clone https://github.com/schematra/schematra.git
   cd schematra
   ```

2. Install Schematra and its dependencies:
   ```bash
   chicken-install
   ```

3. Verify installation by running the demo:
   ```bash
   csi -s demo.scm
   ```

### Your First App

Create a file called `hello.scm`:

```scheme
(import schematra chiccup)

(get ("/")
     (ccup/html
      `[html
        [head [title "My First App"]]
        [body [h1 "Hello, Schematra!"]]]))

(schematra-install)
(schematra-start)
```

Run it with:
```bash
csi -s hello.scm
```

Visit `http://localhost:8080` to see your app!

## Core Concepts

### Routing

Schematra uses a simple routing system inspired by Sinatra:

```scheme
(get ("/path") 
     ;; handle GET request
     )

(post ("/submit")
      ;; handle POST request
      )

(put ("/update/:id")
     ;; handle PUT request with parameter
     )

(delete ("/remove/:id")
        ;; handle DELETE request
        )
```

### URL Parameters

Extract parameters from URLs using the `:param` syntax. URL parameters
can be accessed using `(current-params)` with string keys:

```scheme
(get ("/user/:name")
     (let ((name (alist-ref "name" (current-params) equal?)))
       (ccup/html `[h1 ,(string-append "Hello, " name "!")])))
```

### Query Parameters

Access query parameters through `(current-params)`, using
symbol keys:

```scheme
(get ("/search")
     (let ((query (alist-ref 'q (current-params))))
       (if query
           (ccup/html `[p ,(string-append "Searching for: " query)])
           (ccup/html `[p "No search query provided"]))))
```

## Chiccup HTML rendering

Chiccup allows you to write HTML using Lisp syntax with CSS class integration.

### Basic Syntax

```scheme
;; Element with content
`[h1 "Hello World"]

;; Element with attributes
`[a (("href" . "https://example.com")) "Link"])
;; => <a href="https://example.com">Link</a>

;; Element with CSS classes - default element is "div"
`[.container.mx-auto#the-id "Content"]
;; => <div class="container mx-auto" id="the-id">Content</div>

;; Nested elements
`[.card
  [h2.title "Card Title"]
  [p.content "Card content goes here"]]
;; => <div class="card"><h2 class="title">Card Title</h2><p class="content">Card content goes here</p></div>
```

By default all content is html-escaped:

```scheme
(ccup/html `[p "my <em>emphasized</em> text"])
;; => <p>my &lt;em&gt;emphasized&lt;/em&gt; text</p>
```

If you want to add raw text, pass it as a list with 'raw as the first
element:

```scheme
(ccup/html `[p (raw "my <em>emphasized</em> text")])
;; => <p>my <em>emphasized</em> text</p>
```

### HTML Attributes

You can pass an optional association list as a second element in the
html-tuple to covert those to html attributes.

```scheme
(ccup/html `[.bg-blue-500.text-white.p-4.rounded
  [a.text-2xl.font-bold ((hx-post . "/target")) "Styled link"]
  [p.mt-2 "Styled paragraph"]])
;; => <div class="bg-blue-500 text-white p-4 rounded"><a class="text-2xl font-bold" hx-post="/target">Styled link</a><p class="mt-2">Styled paragraph</p></div>
```

Attribute keys can be either symbol or strings. Values will be html-escaped. If you add css-classes as an attribute, they will be aggregated with the element classes:

```scheme
(ccup/html `[.container ((class . ,(if selected "foo" "bar")))])
;; if selected is #t => <div class="container foo"></div>
;; if selected is #f => <div class="container bar"></div>
```

If you need to dynamically build an id, just pass it as an attribute.

### Dynamic Content

```scheme
(let ((items '("Apple" "Banana" "Cherry")))
  `[ul.list-disc.ml-4
    ,@(map (lambda (item) `[li ,item]) items)])
```

## Middleware System

Schematra supports middleware functions that can process requests before they reach your route handlers. Middleware is useful for cross-cutting concerns like authentication, logging, request parsing, and session management.

### Using Middleware

Install middleware using the `use-middleware!` function:

```scheme
(use-middleware! my-middleware-function)
```

Middleware functions have the following signature:

```scheme
(define (my-middleware next)
  ;; Process request before handler
  (display (current-params))
  (let ((response (next)))  ; Call next middleware or handler
    ;; Process response-tuple after handler
    response))
```

### Middleware Parameters

- `next`: A thunk (zero-argument function) that calls the next middleware in the chain or the final route handler

Middleware can access request parameters using `(current-params)` and modify them using `(current-params new-params)` to affect the parameters seen by subsequent middleware and route handlers.

### Middleware Examples

**Simple Logging Middleware**

```scheme
(define (logging-middleware next)
  (let* ((request (current-request))
         (method (request-method request))
         (uri (request-uri request))
         (path (uri-path uri)))
    (log-dbg "~A ~A" method (uri->string uri))
    (next)))

(use-middleware! logging-middleware)
```

**Authentication Middleware**

```scheme
(define (valid-token? header)
  (and (list? header)
       (= 1 (length header))
       (vector? (car header))
       (string=? (symbol->string (get-value (car header))) "bearer")
       (string=? (symbol->string (caar (get-params (car header)))) "secret")))

(define (auth-middleware next)
  (let ((auth-header (header-contents 'authorization (request-headers (current-request)))))
    (if (and auth-header (valid-token? auth-header))
        (next)  ; Continue to next middleware or route
        '(unauthorized "You don't belong here"))))  ; Return error response

(use-middleware! auth-middleware)
```

### Body Parser Middleware

The body parser middleware automatically parses form-encoded request bodies and makes form data available through `(current-params)`. This is essential for handling HTML forms and POST requests with form data.

#### Installation and Usage

```scheme
(import schematra-body-parser)

;; Install body parser middleware
(use-middleware! (body-parser-middleware))
```

#### How It Works

The middleware automatically:
- Detects requests with `Content-Type: application/x-www-form-urlencoded`
- Parses the request body into key-value pairs
- Adds the parsed form data to the current parameters alist
- Form field keys become **symbol keys** in the parameters

#### Example Usage

```scheme
(import schematra schematra-body-parser chiccup)

;; Install the middleware
(use-middleware! (body-parser-middleware))

;; HTML form
(get ("/login")
     (ccup/html
      `[html
        [head [title "Login"]]
        [body
         [form (("method" . "post") ("action" . "/login"))
          [input (("type" . "text") ("name" . "username") ("placeholder" . "Username"))]
          [input (("type" . "password") ("name" . "password") ("placeholder" . "Password"))]
          [button (("type" . "submit")) "Login"]]]]))

;; Form handler - access form data via current-params
(post ("/login")
      (let ((username (alist-ref 'username (current-params)))  ; Symbol key
            (password (alist-ref 'password (current-params))))  ; Symbol key
        (if (valid-credentials? username password)
            (ccup/html `[p "Login successful!"])
            (ccup/html `[p "Invalid credentials"]))))

;; Mixed parameters example - URL params + form data
(post ("/users/:id/update")
      (let ((user-id (alist-ref "id" (current-params) equal?))      ; String key (URL param)
            (email   (alist-ref 'email (current-params)))          ; Symbol key (form data)
            (name    (alist-ref 'name (current-params))))          ; Symbol key (form data)
        (update-user! user-id name email)
        (ccup/html `[p "User updated successfully"])))
```

#### Parameter Key Types

Remember the different key types when accessing parameters:
- **URL parameters**: String keys (e.g., `"id"`, `"user-id"`)
- **Query parameters**: Symbol keys (e.g., `'search`, `'limit`) 
- **Form data**: Symbol keys (e.g., `'username`, `'email`)

```scheme
;; For request: POST /users/123/edit?debug=true with form data: name=John&email=john@example.com
(let ((user-id (alist-ref "id" (current-params) equal?))     ; "123" (URL param)
      (debug   (alist-ref 'debug (current-params)))          ; "true" (query param)
      (name    (alist-ref 'name (current-params)))           ; "John" (form data)
      (email   (alist-ref 'email (current-params))))         ; "john@example.com" (form data)
  ;; Handle the request...
  )
```

### Middleware Execution Order

Middleware is executed in the order it's installed with `use-middleware!`. The first middleware installed runs first on the request, and last on the response:

```scheme
(use-middleware! middleware-a)  ; Runs first
(use-middleware! middleware-b)  ; Runs second
(use-middleware! middleware-c)  ; Runs third

;; Execution flow:
;; Request: middleware-a -> middleware-b -> middleware-c -> route-handler
;; Response: route-handler -> middleware-c -> middleware-b -> middleware-a
```

## API Reference

### Core Functions

#### Route Return Values

Route handlers can return responses in two formats:

**1. Simple String Response**
Return a string directly for a 200 OK response with that string as the body:

```scheme
(get ("/hello") "Hello, World!")
```

**2. Response Tuple**
Return a list in the format `(status body [headers])` for full control:

```scheme
;; Status and body only
(get ("/api/user") 
     '(created "User created successfully"))

;; With custom headers
(get ("/api/data")
     '(ok "{\"message\": \"success\"}" 
          ((content-type application/json))
           (cache-control no-cache))))
```

**Status Codes**
Common status symbols include:
- Success: `ok` (200), `created` (201), `accepted` (202)
- Redirects: `moved-permanently` (301), `found` (302), `see-other` (303)
- Client Errors: `bad-request` (400), `unauthorized` (401), `forbidden` (403), `not-found` (404)
- Server Errors: `internal-server-error` (500), `bad-gateway` (502), `service-unavailable` (503)

For the full list, see [Intarweb](https://wiki.call-cc.org/eggref/5/intarweb#responses)

**Headers Format**
Headers should be provided in the way they're expected by the [headers function](https://wiki.call-cc.org/eggref/5/intarweb#headers) in intarweb. Refer to the [header type](https://wiki.call-cc.org/eggref/5/intarweb#header-types) section for the full list. The general gist is that they're a list of lists, and each header is a list of two values: the header symbol and its value/options.

```scheme
'((content-type text/html)
  (cache-control (max-age . 3600)))
```

#### `(get (route) body ...)`
Define a GET route handler.

#### `(post (route) body ...)`
Define a POST route handler.

#### `(put (route) body ...)`
Define a PUT route handler.

#### `(delete (route) body ...)`
Define a DELETE route handler.

#### `(current-params)`
Returns an association list containing the current request parameters. This includes:
- **Path parameters** (string keys): URL segments starting with ':' become parameters
- **Query parameters** (symbol keys): URL query string parameters

Example:
```scheme
;; For route /users/:id and request /users/123?format=json
(current-params) ; => '(("id" . "123") (format . "json"))

;; Access path parameter
(alist-ref "id" (current-params) equal?) ; => "123"

;; Access query parameter  
(alist-ref 'format (current-params))    ; => "json"
```

#### `(halt status [body] [headers])`
Immediately stop request processing and send an HTTP response.

This function halts the current handler execution and sends a response with the specified status, body, and headers. It's useful for early returns, error responses, or authentication checks.

```scheme
;; Simple error response
(halt 'not-found "Page not found")

;; JSON API error with headers
(halt 'bad-request 
      "{\"error\": \"Invalid input\"}"
      '((content-type application/json)))

;; Authentication check
(get ("/admin")
     (unless (authenticated? (current-params))
       (halt 'unauthorized "Access denied"))
     "Admin dashboard")
```

#### `(redirect location [status])`
Redirect the client to a different URL.

Sends an HTTP redirect response with the Location header set. The default status is `found` (302).

```scheme
;; Simple redirect
(redirect "/login")

;; Permanent redirect
(redirect "/new-location" 'moved-permanently)

;; After form submission (prevents resubmission)
(post ("/submit")
      (process-form (current-params))
      (redirect "/success" 'see-other))
```

#### `(send-json-response datum [status])`
Send a JSON response with proper content-type headers.

Automatically serializes Scheme data structures to JSON and sets the appropriate `content-type` header. The default status is `ok` (200).

```scheme
;; Simple JSON response
(get ("/api/status")
     (send-json-response '((status . "healthy") (version . "1.0"))))

;; Custom status code
(post ("/api/users")
      (let ((user (create-user! (current-params))))
        (send-json-response
          `((id . ,(user-id user)) (created . #t))
          'created)))

;; Error response
(get ("/api/user/:id")
     (let ((user (find-user (alist-ref "id" (current-params) equal?))))
       (if user
           (send-json-response (user->alist user))
           (send-json-response
             '((error . "User not found"))
             'not-found))))
```

#### `(static path directory)`
Serve static files from a directory. Mount them at `path` prefix.

#### `(schematra-install)`
Install the schematra route handlers.

#### `(schematra-start #!key development? nrepl?)`
Start the web server.
- `development?`: Enable development mode (default: #f) - starts the
  Spiffy server on a separate thread.
- `nrepl?`: If on dev mode, start NREPL server (default: #t)

### Chiccup Functions

#### `(ccup/html s-html)`
Convert Chiccup list to HTML string.

## Advanced Topics

### Static File Serving

Serve static assets like CSS, JavaScript, and images:

```scheme
(static "/assets" "./public")
(static "/css" "./styles")
```

### Session Management

Schematra includes built-in session management through middleware. Sessions are stored in HTTP cookies and automatically serialized/deserialized on each request.

#### Installing Session Middleware

```scheme
(import sessions)

;; Install session middleware with a secret key
(use-middleware! (session-middleware "your-secret-key-here"))
```

#### Configuration Parameters

Control session behavior with these parameters:

```scheme
;; Set session cookie lifetime (default: 24 hours)
(session-max-age (* 2 60 60))  ; 2 hours

;; Set custom cookie name (default: "schematra.session_id")
(session-key "myapp_session")
```

#### Session Functions

**`(session-get key [default])`**
Retrieve a value from the current session:

```scheme
(let ((user-id (session-get "user-id")))
  (if user-id
      (format "Welcome user ~A" user-id)
      "Please log in"))

;; With default value
(session-get "theme" "light")
```

**`(session-set! key value)`**
Store a value in the current session:

```scheme
(session-set! "user-id" "12345")
(session-set! "username" "alice")
(session-set! "preferences" '((theme . "dark") (lang . "en")))
```

**`(session-delete! key)`**
Remove a key from the current session:

```scheme
(session-delete! "user-id")
(session-delete! "username")
```

**`(session-destroy!)`**
Clear all session data (useful for logout):

```scheme
(post ("/logout" params)
      (session-destroy!)
      (redirect "/"))
```

#### Complete Example

```scheme
(import schematra chiccup sessions)

;; Install session middleware
(use-middleware! (session-middleware "my-secret-key-12345"))

;; Login route
(post ("/login")
      (let ((username (alist-ref "username" (current-params) equal?))
            (password (alist-ref "password" (current-params) equal?)))
        (if (valid-credentials? username password)
            (begin
              (session-set! "user-id" (get-user-id username))
              (session-set! "username" username)
              (redirect "/dashboard"))
            (ccup/html `[p "Invalid credentials"]))))

;; Protected route
(get ("/dashboard")
     (let ((username (session-get "username")))
       (if username
           (ccup/html `[h1 ,(string-append "Welcome, " username "!")])
           (redirect "/login"))))

;; Logout route
(post ("/logout")
      (session-destroy!)
      (redirect "/"))
```

### Database Integration

Schematra doesn't include built-in database integration, but you can use any [CHICKEN Scheme database egg](https://eggs.call-cc.org/5/#db) and create middleware to provide a persistence layer.

*Detailed database integration examples coming soon.*

### Error Handling

*Documentation for error handling patterns coming soon.*

### Deployment

*Deployment guides for various platforms coming soon.*

## Examples & Recipes

### Simple Blog

*Coming soon - complete blog example.*

### REST API

*Coming soon - REST API patterns.*

### Form Handling

*Coming soon - form processing examples.*

### File Uploads

*Coming soon - file upload handling.*

## Contributing

We welcome contributions to Schematra! Here's how you can help:

### Getting Started

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

### Code Style

- Follow standard Scheme formatting conventions
- Use descriptive variable names
- Add comments for complex logic
- Keep functions focused and small

### Reporting Issues

Please report bugs and feature requests on our [GitHub Issues](https://github.com/schematra/schematra/issues) page.

### Roadmap

- [X] Session management
- [X] Middleware system
- [X] Authentication helpers
- [ ] WebSocket support

---

*This documentation is a work in progress. Help us improve it by contributing examples and clarifications!*
