# Schematra Documentation

A modern web framework for Scheme that lets you write web apps the way you think.

## Table of Contents

- [What is Schematra?](#what-is-schematra)
- [Quick Reference](#quick-reference)
- [Getting Started](#getting-started)
- [Core Concepts](#core-concepts)
- [Chiccup HTML Generation](#chiccup-html-generation)
- [Middleware System](#middleware-system)
- [OAuth2 Authentication (Oauthtoothy)](#oauth2-authentication-oauthtoothy)
- [API Reference](#api-reference)
- [Advanced Topics](#advanced-topics)
- [Examples & Recipes](#examples--recipes)
- [Contributing](#contributing)

## What is Schematra?

Schematra is a lightweight, expressive web framework for CHICKEN Scheme inspired by Sinatra. Express HTML as data with Chiccup, build components that compose naturally, and create powerful middleware with simple functions.

### Key Features

- **Minimal boilerplate** - Get a web app running with just a few lines of code
- **Chiccup HTML generation** - S-expressions for HTML with syntactic sugar - your HTML structure mirrors your data structure
- **Flexible routing** - Sinatra-style route definitions with URL parameters
- **Built-in sessions** - Cookie-based session management out of the box
- **Middleware support** - Extensible request/response processing pipeline
- **Static file serving** - Serve CSS, JavaScript, and assets effortlessly
- **Compile to a binary** - Because Schematra runs on CHICKEN, you can create a static or dynamically linked binary that makes distribution & release a breeze.

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
(get ("/path") (ccup->html `[h1 "Hello"]))
(post ("/submit") "Form submitted!")

;; Start server
(schematra-install)
(schematra-start)
```

### Common Route Patterns
```scheme
;; Simple routes
(get ("/") "Home page")
(get ("/about") (ccup->html `[h1 "About Us"]))

;; URL parameters
(get ("/user/:id") 
     (let ((id (alist-ref "id" (current-params) equal?)))
       (ccup->html `[h1 ,(string-append "User " id)])))

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

;; Attributes with @ syntax
`[a (@ (href "/page")) "Link"]
`[input (@ (type "text") (name "username"))]

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
     (ccup->html
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
       (ccup->html `[h1 ,(string-append "Hello, " name "!")])))
```

### Query Parameters

Access query parameters through `(current-params)`, using
symbol keys:

```scheme
(get ("/search")
     (let ((query (alist-ref 'q (current-params))))
       (if query
           (ccup->html `[p ,(string-append "Searching for: " query)])
           (ccup->html `[p "No search query provided"]))))
```

## Chiccup HTML Generation

Chiccup is S-expressions for HTML with syntactic sugar that makes writing HTML easier. Chiccup is built on top of sxml-transforms, converting your chiccup syntax to SXML and then to HTML. You can use `ccup->sxml` to get the intermediate SXML representation and leverage the full power of the sxml-transforms ecosystem for advanced transformations.

### Basic Syntax

```scheme
;; Element with content
`[h1 "Hello World"]

;; Element with CSS classes and ID - default element is "div"
`[.container.mx-auto#the-id "Content"]
;; => <div class="container mx-auto" id="the-id">Content</div>

;; Nested elements
`[.card
  [h2.title "Card Title"]
  [p.content "Card content goes here"]]
;; => <div class="card"><h2 class="title">Card Title</h2><p class="content">Card content goes here</p></div>
```

**Note:** CSS selector syntax follows standard conventions where ID (#id) must come after classes.

By default all content is html-escaped:

```scheme
(ccup->html `[p "my <em>emphasized</em> text"])
;; => <p>my &lt;em&gt;emphasized&lt;/em&gt; text</p>
```

If you want to add raw text, pass it as a list with 'raw as the first element:

```scheme
(ccup->html `[p (raw "my <em>emphasized</em> text")])
;; => <p>my <em>emphasized</em> text</p>
```

### HTML Attributes

You can add HTML attributes using the `@` syntax as the second element:

```scheme
;; Basic attributes with @
`[a (@ (href "https://example.com")) "Link"]
;; => <a href="https://example.com">Link</a>

;; Multiple attributes
`[button (@ (type "submit") (disabled)) "Submit"]
;; => <button type="submit" disabled>Submit</button>

;; Combining CSS classes with attributes
`[button.btn.primary (@ (type "submit") (disabled)) "Submit"]
;; => <button class="btn primary" type="submit" disabled>Submit</button>
```

#### Attribute Value Escaping

Following security best practices from frameworks like React and Vue, **attribute values are automatically HTML-escaped** to prevent XSS attacks:

```scheme
;; Quotes and other HTML characters are escaped in attribute values
`[div (@ (data-config "{\"theme\":\"dark\"}")) "Content"]
;; => <div data-config="{&quot;theme&quot;:&quot;dark&quot;}">Content</div>

;; This prevents XSS through crafted attribute values
`[input (@ (value "User input with \"quotes\" & <tags>")) ""]
;; => <input value="User input with &quot;quotes&quot; &amp; &lt;tags&gt;">
```

### Boolean Attributes

Boolean attributes (attributes without values) are rendered correctly:

```scheme
`[input (@ (type "checkbox") (checked) (disabled))]
;; => <input type="checkbox" checked disabled>
```

### Conditional Attributes

Use unquote-splicing (`,@`) to conditionally add attributes:

```scheme
;; Simple conditional attribute
(let ((is-disabled #t))
  `[button (@ (type "submit") ,@(if is-disabled '((disabled)) '())) "Submit"])
;; => <button type="submit" disabled>Submit</button>

;; Multiple conditional attributes
(let ((is-admin #t) (is-active #f))
  `[div (@ (class "user") 
           ,@(if is-admin '((data-role "admin")) '())
           ,@(if is-active '((data-status "active")) '())) "Content"])
;; => <div class="user" data-role="admin">Content</div>
```

### Attribute Merging

CSS classes from selectors and attributes are automatically merged:

```scheme
`[.container (@ (class "extra-class")) "Content"]
;; => <div class="container extra-class">Content</div>

;; Conditional class merging
(let ((highlighted #t))
  `[.card (@ ,@(if highlighted '((class "highlight")) '())) "Content"])
;; if highlighted is #t => <div class="card highlight">Content</div>
;; if highlighted is #f => <div class="card">Content</div>
```

### Dynamic Content

```scheme
(let ((items '("Apple" "Banana" "Cherry")))
  `[ul.list-disc.ml-4
    ,@(map (lambda (item) `[li ,item]) items)])
```

### Inspiration

Chiccup is highly inspired and influenced by the [Hiccup project](https://github.com/weavejester/hiccup).

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
     (ccup->html
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
            (ccup->html `[p "Login successful!"])
            (ccup->html `[p "Invalid credentials"]))))

;; Mixed parameters example - URL params + form data
(post ("/users/:id/update")
      (let ((user-id (alist-ref "id" (current-params) equal?))      ; String key (URL param)
            (email   (alist-ref 'email (current-params)))          ; Symbol key (form data)
            (name    (alist-ref 'name (current-params))))          ; Symbol key (form data)
        (update-user! user-id name email)
        (ccup->html `[p "User updated successfully"])))
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

## OAuth2 Authentication (Oauthtoothy)

Oauthtoothy is Schematra's OAuth2 authentication middleware that provides complete social login integration. It handles the entire OAuth2 flow automatically, from authorization redirects to user info retrieval, with built-in session management and user persistence.

### Key Features

- **Complete OAuth2 Flow**: Handles authorization, token exchange, and user info retrieval
- **Multiple Provider Support**: Configure multiple OAuth2 providers (Google, GitHub, etc.)
- **Automatic Route Registration**: Creates `/auth/{provider}` and `/auth/{provider}/callback` routes
- **Session Integration**: Seamlessly integrates with Schematra's session system
- **User Persistence**: Optional save/load procedures for user data storage
- **Configurable Redirects**: Customize post-authentication redirect behavior

### Installation and Setup

```scheme
(import schematra schematra-session oauthtoothy chiccup)

;; Session middleware is required for Oauthtoothy
(use-middleware! (session-middleware "your-secret-key"))

;; Install OAuth2 middleware
(use-middleware!
 (oauthtoothy-middleware
  (list (google-provider 
         client-id: (get-environment-variable "GOOGLE_CLIENT_ID")
         client-secret: (get-environment-variable "GOOGLE_CLIENT_SECRET")))
  success-redirect: "/dashboard"
  save-proc: save-user-to-db
  load-proc: load-user-from-db))
```

### Core Functions

#### `(oauthtoothy-middleware providers #!key success-redirect save-proc load-proc)`

Creates OAuth2 authentication middleware that handles the complete authentication flow.

**Parameters:**
- `providers`: list - List of OAuth2 provider configurations
- `success-redirect`: string - URL to redirect to after successful authentication (default: "/")
- `save-proc`: procedure or #f - Function to save user data (default: #f for no persistence)
- `load-proc`: procedure or #f - Function to load user data (default: #f for no persistence)

**Registered Routes:**
For each provider named "example", automatically creates:
- `GET /auth/example` - Initiates OAuth2 flow, redirects to provider
- `GET /auth/example/callback` - Handles OAuth2 callback from provider

#### `(auth-base-url [url])`

Parameter that defines the base URL for OAuth2 callback URLs.

```scheme
;; Get current base URL
(auth-base-url) ; => "http://localhost:8080"

;; Set base URL for production
(auth-base-url "https://myapp.example.com")
```

#### `(current-auth)`

Parameter containing the current user's authentication state.

**Unauthenticated state:**
```scheme
'((authenticated? . #f))
```

**Authenticated state:**
```scheme
'((authenticated? . #t)
  (user-id . "user123")
  (name . "John Doe")
  (email . "john@example.com")
  ;; Additional fields from user-info-parser
  )
```

### Provider Configuration

Each provider is configured as an association list with these required keys:

```scheme
(define (custom-provider #!key client-id client-secret)
  `((name . "custom")                    ; Unique provider identifier
    (client-id . ,client-id)             ; OAuth2 client ID
    (client-secret . ,client-secret)     ; OAuth2 client secret
    (auth-url . "https://provider.com/oauth2/auth")        ; Authorization endpoint
    (token-url . "https://provider.com/oauth2/token")      ; Token exchange endpoint
    (user-info-url . "https://api.provider.com/user")     ; User info endpoint
    (scopes . "profile email")           ; Space-separated scopes
    (user-info-parser . ,parse-custom-user)))             ; User data normalizer
```

#### Required Provider Fields

- **name**: Unique string identifier for the provider
- **client-id**: OAuth2 client ID from the provider's developer console
- **client-secret**: OAuth2 client secret from the provider's developer console
- **auth-url**: Provider's OAuth2 authorization endpoint URL
- **token-url**: Provider's OAuth2 token exchange endpoint URL
- **user-info-url**: Provider's user information API endpoint URL
- **scopes**: Space-separated list of OAuth2 scopes to request
- **user-info-parser**: Function to normalize provider-specific user data

#### User Info Parser

The user-info-parser function normalizes provider-specific JSON responses into a standard format:

```scheme
(define (parse-google-user json-response)
  `((id . ,(alist-ref 'id json-response))
    (name . ,(alist-ref 'name json-response))
    (email . ,(alist-ref 'email json-response))))

(define (parse-github-user json-response)
  `((id . ,(number->string (alist-ref 'id json-response)))
    (name . ,(alist-ref 'name json-response))
    (email . ,(alist-ref 'email json-response))
    (username . ,(alist-ref 'login json-response))))
```

### Sample Providers

#### Google Provider

```scheme
(define (google-provider #!key client-id client-secret)
  `((name . "google")
    (client-id . ,client-id)
    (client-secret . ,client-secret)
    (auth-url . "https://accounts.google.com/o/oauth2/auth")
    (token-url . "https://oauth2.googleapis.com/token")
    (user-info-url . "https://www.googleapis.com/oauth2/v2/userinfo")
    (scopes . "profile email")
    (user-info-parser . ,parse-google-user)))
```

**Setup Google OAuth2:**
1. Go to [Google Cloud Console](https://console.cloud.google.com/)
2. Create a new project or select existing project
3. Enable the Google+ API
4. Create OAuth2 credentials
5. Add your callback URL: `{your-domain}/auth/google/callback`

### User Persistence

Oauthtoothy supports optional user data persistence through save and load procedures:

```scheme
;; In-memory storage (for testing/development)
(define user-store (make-hash-table))

(define (save-user user-id user-data)
  (hash-table-set! user-store user-id user-data))

(define (load-user user-id)
  (hash-table-ref/default user-store user-id #f))
```

### Authentication Flow

1. **User visits protected route** → redirected to `/auth/{provider}`
2. **Authorization request** → user redirected to provider's login page
3. **User authorizes** → provider redirects to `/auth/{provider}/callback?code=...`
4. **Token exchange** → Oauthtoothy exchanges code for access token
5. **User info retrieval** → Fetches user profile from provider API
6. **Data normalization** → user-info-parser converts to standard format
7. **Session storage** → User ID stored in session
8. **Optional persistence** → save-proc called if provided
9. **Success redirect** → User redirected to success-redirect URL

### Complete Example

```scheme
(import schematra schematra-session oauthtoothy chiccup srfi-69)

;; User storage
(define user-store (make-hash-table))
(define (save-user user-id user-data)
  (hash-table-set! user-store user-id user-data))
(define (load-user user-id)
  (hash-table-ref/default user-store user-id #f))

;; Google user data parser
(define (parse-google-user json-response)
  `((id . ,(alist-ref 'id json-response))
    (name . ,(alist-ref 'name json-response))
    (email . ,(alist-ref 'email json-response))))

;; Provider configuration
(define (google-provider #!key client-id client-secret)
  `((name . "google")
    (client-id . ,client-id)
    (client-secret . ,client-secret)
    (auth-url . "https://accounts.google.com/o/oauth2/auth")
    (token-url . "https://oauth2.googleapis.com/token")
    (user-info-url . "https://www.googleapis.com/oauth2/v2/userinfo")
    (scopes . "profile email")
    (user-info-parser . ,parse-google-user)))

;; Install middleware
(use-middleware! (session-middleware "secret-key"))
(use-middleware!
 (oauthtoothy-middleware
  (list (google-provider
         client-id: (get-environment-variable "GOOGLE_CLIENT_ID")
         client-secret: (get-environment-variable "GOOGLE_CLIENT_SECRET")))
  success-redirect: "/profile"
  save-proc: save-user
  load-proc: load-user))

;; Protected route
(get ("/profile")
     (let ((auth (current-auth)))
       (if (alist-ref 'authenticated? auth)
           (ccup->html 
            `[html
              [head [title "Profile"]]
              [body
               [h1 ,(string-append "Welcome, " (alist-ref 'name auth))]
               [p "Email: " ,(alist-ref 'email auth)]
               [a ((href . "/logout")) "Logout"]]])
           (redirect "/auth/google"))))

;; Logout route
(get ("/logout")
     (session-destroy!)
     (redirect "/"))

;; Public landing page
(get ("/")
     (ccup->html
      `[html
        [head [title "My App"]]
        [body
         [h1 "Welcome to My App"]
         [a ((href . "/profile")) "Login with Google"]]]))

(schematra-install)
(schematra-start)
```

### Configuration Parameters

#### Setting Base URL

Configure the base URL for OAuth2 callbacks (required for production):

```scheme
;; Development (default)
(auth-base-url "http://localhost:8080")

;; Production
(auth-base-url "https://myapp.example.com")
```

### Route Protection Patterns

#### Simple Authentication Check

```scheme
(get ("/dashboard")
     (let ((auth (current-auth)))
       (if (alist-ref 'authenticated? auth)
           (ccup->html `[h1 "Dashboard Content"])
           (redirect "/auth/google"))))
```

#### Authentication Middleware

Create reusable authentication middleware:

```scheme
(define (require-auth next)
  (let ((auth (current-auth)))
    (if (alist-ref 'authenticated? auth)
        (next)
        (redirect "/auth/google"))))

(use-middleware! require-auth)

;; Now all routes require authentication
(get ("/admin") (ccup->html `[h1 "Admin Panel"]))
(get ("/settings") (ccup->html `[h1 "User Settings"]))
```

### Multiple Provider Setup

```scheme
;; Configure multiple OAuth2 providers
(use-middleware!
 (oauthtoothy-middleware
  (list (google-provider 
         client-id: (get-environment-variable "GOOGLE_CLIENT_ID")
         client-secret: (get-environment-variable "GOOGLE_CLIENT_SECRET"))
        (github-provider
         client-id: (get-environment-variable "GITHUB_CLIENT_ID")
         client-secret: (get-environment-variable "GITHUB_CLIENT_SECRET")))
  success-redirect: "/dashboard"))

;; Login page with multiple options
(get ("/login")
     (ccup->html
      `[html
        [head [title "Login"]]
        [body
         [h1 "Choose Login Method"]
         [a ((href . "/auth/google")) "Login with Google"]
         [a ((href . "/auth/github")) "Login with GitHub"]]]))
```

### Error Handling

Oauthtoothy automatically handles common OAuth2 errors:

- **Invalid authorization codes** → 502 Bad Gateway response
- **Provider API failures** → 502 Bad Gateway response  
- **JSON parsing errors** → 502 Bad Gateway response
- **Missing user info** → Graceful fallback to unauthenticated state

### Security Considerations

- **HTTPS Required**: Use HTTPS in production for secure token transmission
- **Secret Management**: Store client secrets in environment variables, never in code
- **Callback URL Validation**: Ensure callback URLs are registered with providers
- **Session Security**: Use strong secret keys for session middleware

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

#### `(ccup->html s-html)`
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
            (ccup->html `[p "Invalid credentials"]))))

;; Protected route
(get ("/dashboard")
     (let ((username (session-get "username")))
       (if username
           (ccup->html `[h1 ,(string-append "Welcome, " username "!")])
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
