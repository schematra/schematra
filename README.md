# Schematra

![Schematra Logo](logo.png)

A minimal web framework for [CHICKEN Scheme](https://call-cc.org/), inspired by [Sinatra](https://sinatrarb.com/). Schematra is currently an early exploration project created for learning purposes, but hopefully it will grow into something more useful over time.

## Why Schematra?

I created Schematra because I wanted to:

- **Improve my knowledge of scheme**: Building a web framework is a great way to explore a language's capabilities and idioms
- **Create something simple**: Most web frameworks are complex beasts. Schematra aims to be minimal and understandable
- **Enable modern web development**: The framework is designed to work well with modern tools like [Tailwind CSS](https://tailwindcss.com/) and [htmx](https://htmx.org/), making it easy to build interactive web applications without heavy JavaScript frameworks. Although tbh this is completely agnostic to how the framework works, it's what most of my examples will use.

## Features

- Simple route definition with `get` and `post` functions
- URL parameter extraction (e.g., `/users/:id`) & body parsing
- Middleware support
- Included naive session middleware (cookie storage)
- Development mode with REPL integration (leveraging emacs `run-scheme`)
- Very simple [hiccup](https://github.com/weavejester/hiccup) inspired template system
- Built on top of the solid [Spiffy](http://wiki.call-cc.org/eggref/5/spiffy) web server

## Installation

First, make sure you have [CHICKEN Scheme](https://call-cc.org/) installed. Then install the required dependencies:

```bash
chicken-install spiffy intarweb uri-common \
  srfi-1 srfi-13 srfi-18 srfi-69 \
  format random-mtzig message-digest \
  hmac sha2 base64 medea openssl http-client
```

For development mode, you'll also need:

```bash
chicken-install nrepl
```

Next step would be to build schematra & the core modules:

```bash
make
```

## Quick Start

Here's a simple "Hello World" application:

```scheme
(import schematra)

;; Define routes
(get "/" (lambda (req params) "Hello, World!"))

(get "/users/:id" 
     (lambda (req params)
       (let ((user-id (alist-ref "id" params equal?)))
         (format "User ID: ~A" user-id))))

(post "/submit"
      (lambda (req params)
        (let ((body (request-body-string req)))
          (format "Received: ~A" body))))

;; Start the server
(schematra-install)
(schematra-start port: 8080)
```

Save this as `app.scm` and run:

```bash
csi app.scm
```

Visit `http://localhost:8080` to see your application running.

## Development Mode

For interactive development, start the server in development mode:

```scheme
(schematra-start development?: #t port: 8080 repl-port: 1234)
```

This starts the web server in a background thread and opens an NREPL on port 1234. You can connect with your favorite Scheme editor or use `nc localhost 1234` for a simple REPL session.

For a more elegant environment, you can use emacs `run-scheme` by running `C-u M-x run-scheme RET nc localhost 1234`.

## Route Parameters

Schematra supports URL parameters using the `:parameter` syntax, as well as query params:

```scheme
(define (lookup key alist)
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
        #f)))

(get "/users/:user-id/posts/:post-id"
     (lambda (req params)
       (let ((user-id (lookup "user-id" params))
             (post-id (lookup "post-id" params))
			 (q       (lookup 'q params))) ;; query params use symbol keys
         (format "User: ~A, Post: ~A" user-id post-id))))
```

The `params` argument contains both URL parameters (with string keys) and query parameters (with symbol keys).

## Route Handlers

Route handlers are functions that process HTTP requests and generate responses. Understanding how they work and what they should return is crucial for building Schematra applications.

### Handler Function Signature

Every route handler must accept exactly two arguments:

```scheme
(define (my-handler request params)
  ;; Handler implementation
  )
```

- **`request`**: The [intarweb](https://wiki.call-cc.org/eggref/5/intarweb#requests) request object containing headers, method, URI, and request port
- **`params`**: An association list containing both URL path parameters and query parameters

### The Request Object

The `request` parameter provides access to all aspects of the HTTP request:

```scheme
(get "/debug"
     (lambda (req params)
       (let ((method (request-method req))          ; 'GET, 'POST, etc.
             (uri (request-uri req))                ; Full URI object
             (headers (request-headers req))        ; Request headers
             (port (request-port req)))             ; Input port for body
         (format "Method: ~A, Path: ~A" 
                 method 
                 (uri-path uri)))))
```

Common request operations:
- `(request-method request)` - Get HTTP method (GET, POST, etc.)
- `(request-uri request)` - Get URI object with path, query, etc. (see [uri-common](https://wiki.call-cc.org/eggref/5/uri-common))
- `(request-headers request)` - Get request headers
- `(request-body-string request)` - Read request body as string (useful for POST data)

### The Params Argument

The `params` argument is an association list containing two types of parameters:

1. **Path Parameters** (string keys): Extracted from URL segments starting with `:`
2. **Query Parameters** (symbol keys): Extracted from the URL query string

```scheme
;; Route: /users/:id?format=json&limit=10
;; URL: /users/123?format=json&limit=10
;; params = (("id" . "123") (format . "json") (limit . "10"))

(get "/users/:id"
     (lambda (req params)
       (let ((user-id (alist-ref "id" params equal?))      ; Path param (string key)
             (format (alist-ref 'format params))           ; Query param (symbol key)
             (limit (alist-ref 'limit params)))            ; Query param (symbol key)
         (format "User ~A, format: ~A, limit: ~A" user-id format limit))))
```

### Handler Return Values

Route handlers can return different types of values, which Schematra automatically converts to the corresponding [intarweb response](https://wiki.call-cc.org/eggref/5/intarweb#responses):

#### 1. String Response (Most Common)

Return a string to send a 200 OK response with that string as the body:

```scheme
(get "/hello"
     (lambda (req params)
       "Hello, World!"))  ; Returns 200 OK with "Hello, World!" body
```

#### 2. Response List

Return a list in the format `(status body [headers])` for full control over the response:

```scheme
(get "/custom"
     (lambda (req params)
       '(created "Resource created successfully")))  ; Returns 201 Created

(get "/with-headers"
     (lambda (req params)
       '(ok "Success" ((content-type . "text/plain")))))  ; With custom headers
```

Some common valid status symbols include:
- `ok` (200)
- `created` (201)
- `found` (302) - for redirects
- `bad-request` (400)
- `unauthorized` (401)
- `forbidden` (403)
- `not-found` (404)
- `internal-server-error` (500)
- And [many others](https://wiki.call-cc.org/eggref/5/intarweb#responses) following HTTP status code conventions

#### 3. Halting the routing

Schematra provides helper functions for common response patterns:

```scheme
;; Redirect to another URL
(get "/old-page"
     (lambda (req params)
       (redirect "/new-page")))  ; 302 redirect

;; Halt with specific status and message
(get "/admin"
     (lambda (req params)
       (if (not (authenticated? req))
           (halt 'unauthorized "Access denied")
           "Welcome to admin panel")))
```

Redirect and halt both generate a specific signal that's captured by the main router and short-circuit any other processing: no other middleware or part of the route handler will be executed.

### HTML Responses with Chiccup

For HTML responses, use the included Chiccup template system:

```scheme
(import chiccup)

(get "/page"
     (lambda (req params)
       (ccup/html
        `[html
          [head [title "My Page"]]
          [body
           [h1 "Welcome"]
           [p "This is generated HTML"]]])))
```

### Working with Request Bodies

For POST requests, you can easily access the request body using `request-body-string`:

```scheme
(post "/submit"
      (lambda (req params)
        (let ((body (request-body-string req)))
          (if (string=? body "")
              '(bad-request "Empty request body")
              (format "Received: ~A" body)))))
```

Since you have access to the intarweb request object, you can also access the object and its port directly if you want.

## Middleware

Schematra supports middleware functions that can process requests before they reach your route handlers. Middleware is useful for cross-cutting concerns like authentication, logging, request parsing, and session management.

### Using Middleware

Install middleware using the `use-middleware!` function:

```scheme
(use-middleware! my-middleware-function)
```

Middleware functions have the following signature:

```scheme
(define (my-middleware request params next)
  ;; Process request/params before handler
  (let ((response (next)))  ; Call next middleware or handler
    ;; Process response after handler
    response))
```

### Middleware Parameters

- `request`: The HTTP request object
- `params`: The route and query parameters alist
- `next`: A thunk (zero-argument function) that calls the next middleware in the chain or the final route handler

### Middleware Examples

#### Simple Logging Middleware

```scheme
(define (logging-middleware request params next)
  (let* ((method (request-method request))
         (uri (request-uri request))
         (path (uri-path uri)))
    (log-dbg "~A ~A" method (uri->string uri))
    (next)))

(use-middleware! logging-middleware)
```

#### Authentication Middleware

```scheme
(define (valid-token? header)
  (and (list? header)
       (= 1 (length header))
       (vector? (car header))
       (string=? (symbol->string (get-value (car header))) "bearer")
       (string=? (symbol->string (caar (get-params (car header)))) "secret")))

;; detail of the headers content: https://wiki.call-cc.org/eggref/5/intarweb#headers
(define (auth-middleware request params next)
  (let ((auth-header (header-contents 'authorization (request-headers request))))
    (if (and auth-header (valid-token? auth-header))
        ;; Continue to next middleware or route
        (next)
        ;; Return error response
        '(unauthorized "You don't belong here"))))

(use-middleware! auth-middleware)
```

#### Request Processing Middleware

```scheme
(define (json-middleware request params next)
  (let* ((content-type (header-value 'content-type (request-headers request)))
         (is-json? (and content-type (string-contains content-type "application/json"))))
    (if is-json?
        (let* ((body (request-body-string request))
               (parsed-json (parse-json body))
               ;; Add parsed JSON to params
               (enhanced-params (cons `(json . ,parsed-json) params)))
          ;; Call next with enhanced params
          (next request enhanced-params))
        (next))))

(use-middleware! json-middleware)
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

### Built-in Middleware

Schematra includes session middleware for cookie-based session management. See the "Session Management" section for details on using the session middleware.

## Working with Modern Web Tools

Schematra plays nicely with modern web development tools:

### Tailwind CSS

Include Tailwind via CDN in your HTML responses:

```scheme
(get "/tw-demo"
     (lambda (req params)
       (ccup/html
	`[html
	  [head [script (("src" . "https://cdn.tailwindcss.com"))]]
	  [body.bg-gray-100.p-8 [h1.text-3xl.font-bold.text-blue-600 "Hello, Tailwind!"]]])))
```

### htmx

Add htmx for dynamic interactions:

```scheme
(get "/htmx-demo"
     (lambda (req params)
       (ccup/html
	`[html
	  [head [script (("src" . "https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js"))]]
	  [body
	   [button (("hx-get" . "/clicked") ("hx-target" . "#result")) "Click me!"]
	   [\#result]]])))

(get "/clicked"
     (lambda (req params)
       (ccup/html `[p "Button was clicked!"])))
```

## Session Management

Schematra includes a simple session middleware that stores session data in HTTP cookies. Sessions are automatically serialized and deserialized on each request.

### Basic Usage

First, install the session middleware with a secret key:

```scheme
(import sessions)

;; Install session middleware
(use-middleware! (session-middleware "your-secret-key-here"))
```

Then use session functions in your route handlers:

```scheme
(get "/login"
     (lambda (req params)
       (session-set! "user-id" "12345")
       (session-set! "username" "alice")
       "Logged in successfully"))

(get "/profile"
     (lambda (req params)
       (let ((user-id (session-get "user-id")))
         (if user-id
             (format "Welcome user ~A" user-id)
             "Please log in"))))

(get "/logout"
     (lambda (req params)
       (session-delete! "user-id")
       (session-delete! "username")
       "Logged out"))
```

### Session Functions

- `(session-get key [default])` - Retrieve a value from the session
- `(session-set! key value)` - Store a value in the session
- `(session-delete! key)` - Remove a key from the session

### Configuration

You can customize session behavior using parameters:

```scheme
;; Set session cookie name (default: "schematra.session_id")
(session-key "myapp_session")

;; Set session expiration time in seconds (default: 24 hours)
(session-max-age (* 7 24 60 60))  ; 1 week
```

### Security Notes

- Sessions are stored as serialized data in cookies (client-side storage)
- The secret key is used for session identification but not encryption
- Avoid storing sensitive data in sessions
- Consider implementing proper encryption/signing for production use
- Session cookies are HTTP-only by default to prevent JavaScript access

## Current Status

**This is a toy project!** Schematra is still in early development and should not be used for production applications. It's missing many features you'd expect from a mature web framework:

- Limited error handling
- No template engine, but you can use the included chiccup, a [hiccup](https://github.com/weavejester/hiccup)-inspired template system.
- No database integration (but you can use any [database egg](https://eggs.call-cc.org/5/#db))
- No background job system (working on one though)
- Limited security features

However, it's perfect for:
- Learning Scheme
- Prototyping simple web applications
- Experimenting with htmx and Tailwind CSS
- Understanding how web frameworks work under the hood

## Contributing

If you find Schematra interesting and want to help it grow beyond a toy project, contributions are welcome! Feel free to:

- Report bugs or suggest features via GitHub issues
- Submit pull requests with improvements
- Share your experience using Schematra
- Help improve the documentation

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

See the source code for the complete license text.
