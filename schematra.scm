;; Schematra - a very simple web framework for scheme inspired in
;; Sinatra
;; Copyright 2025 Rolando Abarca
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following
;; disclaimer in the documentation and/or other materials provided
;; with the distribution.
;;
;; 3. Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived
;; from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(module schematra
 ( ;; Parameters
  schematra-default-handler
  schematra-default-vhost
  ;; Procedures
  ;; verbs & friends
  get post put delete
  sse write-sse-data
  current-body
  current-params
  ;; other utilities
  static
  add-resource! ;; used by the verb-routing macros
  halt redirect
  log-err log-dbg
  cookie-set! cookie-delete! cookie-ref
  use-middleware!
  request-body-string
  send-json-response
  schematra-install
  schematra-start
  ) ; end export list

 (import scheme)
 (import
  chicken.base
  chicken.platform ;; chicken-version
  chicken.io
  chicken.condition
  chicken.string
  chicken.file
  chicken.pathname
  chicken.file.posix
  chicken.time.posix
  medea ;; for send-json
  sendfile
  spiffy
  format
  uri-common
  (rename intarweb (headers intarweb:headers))
  (rename srfi-1 (delete srfi1:delete))
  srfi-13
  srfi-18
  srfi-69
  chiccup)

 (define version-major "0")
 (define version-minor "2")
 (define version-patch "1")

 ;;; Default virtual host pattern for Schematra routing
 ;;;
 ;;; This parameter defines the regular expression pattern used to match virtual hosts
 ;;; when installing the Schematra router. The default pattern ".*" matches all hostnames,
 ;;; meaning the router will handle requests for any domain or IP address.
 ;;;
 ;;; ### Usage
 ;;; You can customize this to restrict routing to specific domains:
 ;;; - "example\\.com" - matches only example.com
 ;;; - "(api|www)\\.example\\.com" - matches api.example.com and www.example.com
 ;;; - "localhost" - matches only localhost
 ;;;
 ;;; This parameter is used by `schematra-install` when configuring the vhost-map.
 (define schematra-default-vhost (make-parameter ".*"))

 ;;; Default handler for unmatched routes
 ;;; 
 ;;; This handler is called when no specific route matches the incoming request.
 ;;; It serves as a fallback and returns a simple welcome message. This handler
 ;;; is automatically registered for the root path ("/") when the route trees
 ;;; are initialized.
 ;;; 
 ;;; ### Returns
 ;;;   A string containing the default welcome message
 (define (schematra-default-handler)
   "Welcome to Schematra, Sinatra's weird friend.")

 ;;; Log an error message to the error log
 ;;;
 ;;; Writes a formatted error message to the configured error log stream.
 ;;; By default, this writes to standard error (stderr). The error log destination
 ;;; can be configured using Spiffy's error-log parameter.
 ;;;
 ;;; ### Parameters
 ;;;   - `format`: string - Format string compatible with the format procedure
 ;;;   - `rest`: any - Additional arguments for the format string
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; (log-err "Database connection failed: ~A" error-message)
 ;;; (log-err "Invalid user ID: ~A (expected number)" user-id)
 ;;; ```
 (define (log-err format . rest)
   (apply log-to (error-log) format rest))

 ;;; Log a debug/access message to the access log
 ;;;
 ;;; Writes a formatted debug or access message to the configured access log stream.
 ;;; By default, this writes to standard output (stdout). The access log destination
 ;;; can be configured using Spiffy's access-log parameter.
 ;;;
 ;;; This is commonly used for request logging, debugging information, and general
 ;;; application status messages that don't indicate errors.
 ;;;
 ;;; ### Parameters
 ;;;   - `format`: string - Format string compatible with the format procedure
 ;;;   - `rest`: any - Additional arguments for the format string
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; (log-dbg "Processing request for path: ~A" path)
 ;;; (log-dbg "User ~A logged in successfully" username)
 ;;; (log-dbg "Cache hit for key: ~A" cache-key)
 ;;; ```
 (define (log-dbg format . rest)
   (apply log-to (access-log) format rest))

 (define (make-path-tree)
   `("/" ,schematra-default-handler))

 ;; resources tree based on the verb
 (define resources-tree-for-verb (make-hash-table))

 ;; find a resource based on a path on a tree. Returns the handler if
 ;; found, #f otherwise.
 (define (find-resource path tree #!optional (params '()))
   (cond
    ;; If tree is not a list or is empty, no match
    [(not (and (list? tree) (>= (length tree) 2)))
     #f]
    ;; Handle wildcard match - captures all remaining path segments
    [(string=? (car tree) "*")
     (if (procedure? (cadr tree))
         (list (cadr tree) (cons (cons "*" (string-join path "/")) params))
         #f)]
    ;; Try to match first path element with tree node
    [(or (string=? (car path) (car tree))
         (and (string-prefix? ":" (car tree)) (not (null? path))))
     ;; Collect parameter if this is a param segment
     (let ((new-params (if (string-prefix? ":" (car tree))
                           (cons (cons (substring (car tree) 1) (car path)) params)
                           params)))
       ;; If we've matched and this is the last path element
       (if (null? (cdr path))
           ;; Check if second element is a procedure (leaf node)
           (if (procedure? (cadr tree))
               (list (cadr tree) (reverse new-params)) ; Return handler and params
               #f)
           ;; Otherwise, continue searching in subtrees
           (let loop ((subtrees (cddr tree)))
             (cond
              [(null? subtrees) #f]
              [(list? (car subtrees))
               (let ((result (find-resource (cdr path) (car subtrees) new-params)))
                 (if result result (loop (cdr subtrees))))]
              [else (loop (cdr subtrees))]))))]
    ;; No match with current tree node
    [else #f]))

 ;; internal function that updates a tree. Returns a new tree
 (define (add-resource path tree handler)
   (if (null? path)
       tree                            ; Empty path, return tree as-is
       (let ((target-segment (car path))
             (remaining-path (cdr path)))
         (cond
          ;; If tree matches current segment
          [(and (list? tree) (>= (length tree) 2) (string=? target-segment (car tree)))
           (if (null? remaining-path)
               ;; Last segment, update handler but preserve existing subtrees
               (append (list (car tree) handler) (cddr tree))
               ;; More segments, recursively add to subtrees
               (let loop ((subtrees (cddr tree)) (result (list (car tree) (cadr tree))))
                 (cond
                  [(null? subtrees)
                   ;; No matching subtree, create new one
                   (append result (list (add-resource remaining-path (list (car remaining-path) #f) handler)))]
                  [(and (list? (car subtrees)) (string=? (car remaining-path) (caar subtrees)))
                   ;; Found matching subtree
                   (append result
                           (list (add-resource remaining-path (car subtrees) handler))
                           (cdr subtrees))]
                  [else
                   ;; Keep looking
                   (loop (cdr subtrees) (append result (list (car subtrees))))])))]
          ;; Tree doesn't match, create new tree for this path
          [else
           (if (null? remaining-path)
               (list target-segment handler)
               (list target-segment #f (add-resource remaining-path (list (car remaining-path) #f) handler)))]))))

 ;; external version that can be leveraged by macros
 (define (add-resource! path http-verb handler)
   (let* ((tree     (hash-table-ref/default resources-tree-for-verb http-verb (make-path-tree)))
          (new-tree (add-resource (normalize-path (uri-path (uri-reference path))) tree handler)))
     (hash-table-set! resources-tree-for-verb http-verb new-tree)))

 (define development-mode? #f)

 (define (normalize-path path-list)
   (let* ( ;; Ensure path-list is actually a list
          (path-as-list (if (list? path-list) path-list (list path-list)))
          ;; Ensure all elements are strings, converting symbols to strings
          (string-path (map (lambda (segment)
                              (if (symbol? segment)
                                  (symbol->string segment)
                                  segment))
                            path-as-list))
          ;; Filter out empty strings
          (normalized-path (filter (lambda (segment)
                                     (not (string=? segment "")))
                                   string-path)))
     normalized-path))

 (import-for-syntax srfi-13)
 (define-syntax define-verb
   (er-macro-transformer
    (lambda (exp rename compare)
      (let* ((verb-name (cadr exp))
             (verb-symbol (string->symbol (string-upcase (symbol->string verb-name)))))
        `(define-syntax ,verb-name
           (syntax-rules ()
             ((_ (path) body ...)
              (let ((handler (lambda () body ...)))
                (add-resource! path ',verb-symbol handler)))))))))

 ;;; Register a GET route handler
 ;;;
 ;;; Registers a handler function to respond to HTTP GET requests for a specific path.
 ;;; The path can include parameter segments prefixed with ':' to capture dynamic values.
 ;;;
 ;;; ### Parameters
 ;;;   - `path`: string - URL path pattern (e.g., "/users", "/api/posts/:id", "/users/:user-id/posts/:post-id")
 ;;;   - `body`: expressions - Route handler body that processes the request
 ;;;
 ;;; ### Handler Parameters
 ;;;
 ;;; You can get the params by referring the `current-params` parameter, which contains two types of parameters:
 ;;; 1. **Path Parameters** (string keys): URL segments starting with ':' become parameters
 ;;;    - Route "/users/:id" matches "/users/123" 
 ;;;    - Contributes `'(("id" . "123"))` to params
 ;;;    - Route "/users/:user-id/posts/:post-id" matches "/users/alice/posts/42"
 ;;;    - Contributes `'(("user-id" . "alice") ("post-id" . "42"))` to params
 ;;; 2. **Query Parameters** (symbol keys): URL query string parameters
 ;;;    - Request "/users/123?format=json&limit=10"
 ;;;    - Contributes `'((format . "json") (limit . "10"))` to params
 ;;;   
 ;;; **Note:** Path parameters use string keys, query parameters use symbol keys.
 ;;; This allows you to distinguish between the two types when processing params.
 ;;;
 ;;; ### Handler Return Value
 ;;; The route handler should return a string (which becomes the response body with 200 OK status)
 ;;; or a response list in the format `(status body [headers])`.
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;;; Simple static route
 ;;; (get ("/hello") "Hello, World!")
 ;;;
 ;;; ;;; Route with parameters
 ;;; (get ("/users/:id")
 ;;;      (let ((user-id (alist-ref "id" (current-params) equal?)))
 ;;;        (format "User ID: ~A" user-id)))
 ;;;
 ;;; ;;; Route with multiple parameters
 ;;; (get ("/users/:user-id/posts/:post-id")
 ;;;      (let* ((params (current-params)
 ;;;             (user-id (alist-ref "user-id" params equal?))
 ;;;             (post-id (alist-ref "post-id" params equal?)))
 ;;;        (format "User ~A, Post ~A" user-id post-id)))
 ;;; ```
 (define-verb get)

 ;;; Register a POST route handler
 ;;;
 ;;; Registers a handler function to respond to HTTP POST requests for a specific path.
 ;;; The path can include parameter segments prefixed with ':' to capture dynamic values.
 ;;;
 ;;; ### Parameters
 ;;;   - `path`: string - URL path pattern (e.g., "/users", "/api/posts", "/users/:id")
 ;;;   - `body`: expressions - Route handler body that processes the request
 ;;;
 ;;; ### Handler Parameters
 ;;; See the `get` function documentation for complete details on handler parameters,
 ;;; path parameters, query parameters, and return values. POST handlers work
 ;;; identically to GET handlers in terms of parameter handling and responses.
 (define-verb post)
 (define-verb put)
 (define-verb delete)

;;; Register a Server-Sent Events (SSE) endpoint
;;;
;;; Creates an SSE endpoint that can stream real-time data to web browsers.
;;; SSE provides a simple way to push data from server to client over a persistent
;;; HTTP connection, perfect for live updates, notifications, and real-time features.
;;;
;;; ### Parameters
;;;   - `path`: string - URL path for the SSE endpoint (e.g., "/events", "/chat/:room")
;;;   - `req`: symbol - Request parameter name (typically 'req')
;;;   - `body`: expressions - SSE handler body that manages the connection
;;;
;;; Unlike regular handlers, SSE handlers typically run in a loop to continuously
;;; send data. The handler should call `write-sse-data` to send events to the client.
;;;
;;; ### Automatic Headers
;;; The sse function automatically sets the required SSE headers:
;;;   - Content-Type: text/event-stream
;;;   - Cache-Control: no-cache
;;;   - Connection: keep-alive
;;;
;;; ### Client-Side Usage
;;; Connect to SSE endpoints using JavaScript EventSource or HTMX SSE extension:
;;;   
;;; **JavaScript:**
;;; ```javascript
;;; const eventSource = new EventSource('/events');
;;; eventSource.onmessage = function(event) {
;;;   console.log('Received:', event.data);
;;; };
;;; ```
;;;
;;; **HTMX:**
;;; ```html
;;; <div hx-ext="sse" sse-connect="/events" sse-swap="message"></div>
;;; ```
;;;
;;; ### Examples
;;; ```scheme
;;; ;;; Simple time server
;;; (sse ("/time" req)
;;;      (let loop ()
;;;        (write-sse-data (current-time-string) event: "time-update")
;;;        (thread-sleep! 1)
;;;        (loop)))
;;;
;;; ;;; Chat room with parameters
;;; (sse ("/chat/:room" req)
;;;      (let ((room (alist-ref "room" (current-params) equal?)))
;;;        (let loop ()
;;;          (let ((messages (get-room-messages room)))
;;;            (when (new-messages? messages)
;;;              (write-sse-data (format-message messages) event: "message"))
;;;            (thread-sleep! 1)
;;;            (loop)))))
;;; ```
 (define (sse path handler)
   (get (path)
        (current-response
         (update-response (current-response)
                          headers:
                          (intarweb:headers `((content-type text/event-stream)
                                              (cache-control no-cache)
                                              (connection keep-alive)
                                              (x-sse-handler #t)))))
        (write-logged-response)
        (handler)))

 ;;; Send data to an SSE client
 ;;;
 ;;; This function sends a Server-Sent Events message to the connected client.
 ;;; It formats the data according to the SSE protocol specification and writes
 ;;; it to the response stream. This function should only be called within an
 ;;; SSE handler registered with the `sse` function.
 ;;;
 ;;; ### Parameters
 ;;;   - `data`: string - The message data to send to the client
 ;;;
 ;;; ### Keyword Parameters
 ;;;   - `id`: string or #f - Optional event ID for client-side event tracking (default: #f)
 ;;;       When provided, the client can use this ID to resume connections and
 ;;;       avoid duplicate events. The browser will send this ID in the
 ;;;       Last-Event-ID header when reconnecting.
 ;;;
 ;;;   - `event`: string or #f - Optional event type name (default: #f)
 ;;;          When provided, the client can listen for specific event types.
 ;;;          Without this, the client receives generic "message" events.
 ;;;
 ;;; ### SSE Message Format
 ;;; The function automatically formats messages according to SSE protocol:
 ;;;   - "id: <id>\n" (if id provided)
 ;;;   - "event: <event>\n" (if event provided)  
 ;;;   - "data: <data>\n"
 ;;;   - "\n" (blank line to end message)
 ;;;
 ;;; ### Client-Side Handling
 ;;; **JavaScript EventSource API:**
 ;;; ```javascript
 ;;; eventSource.addEventListener('custom-event', function(e) {
 ;;;   console.log('ID:', e.lastEventId, 'Data:', e.data);
 ;;; });
 ;;; ```
 ;;;
 ;;; **HTMX SSE Extension:**
 ;;; ```html
 ;;; <div sse-swap="custom-event">Content updated by custom-event</div>
 ;;; ```
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Simple message
 ;;; (write-sse-data "Hello, client!")
 ;;;
 ;;; ;; Message with event type
 ;;; (write-sse-data "New notification" event: "notification")
 ;;;
 ;;; ;; Message with ID and event type
 ;;; (write-sse-data "Chat message" id: "msg-123" event: "chat")
 ;;;
 ;;; ;; HTML content for HTMX
 ;;; (write-sse-data (ccup->html `[div.alert "Server alert!"]) event: "update")
 ;;; ```
 ;;;
 ;;; ### Connection Management
 ;;;   - Each call sends one complete SSE message
 ;;;   - The connection remains open for subsequent calls
 ;;;   - Connection closes when the handler function exits
 ;;;   - Clients automatically reconnect on connection loss
 (define (write-sse-data data #!key id event)
   (let ((msg (conc (if id (conc "id: " id "\n") "")
                    (if event (conc "event: " event "\n") "")
                    "data: " data "\n\n")))
     ;; this code will throw an i/o exception if the client
     ;; disconnects
     (display msg (response-port (current-response)))
     (finish-response-body (current-response))))

 ;;; Immediately halt request processing and send an HTTP response
 ;;;
 ;;; This function stops the current request handler execution and sends an HTTP
 ;;; response with the specified status, body, and headers. It works by signaling
 ;;; a special halt-condition that is caught by the router and converted into an
 ;;; HTTP response.
 ;;;
 ;;; This is useful for early returns from handlers, error responses, or any
 ;;; situation where you need to bypass normal handler flow and send a response
 ;;; immediately.
 ;;;
 ;;; ### Parameters
 ;;;   - `status`: symbol - HTTP status code symbol (e.g., 'ok, 'not-found, 'internal-server-error)
 ;;;           Common status symbols include:
 ;;;           - 'ok (200), 'created (201), 'accepted (202)
 ;;;           - 'moved-permanently (301), 'found (302), 'see-other (303)
 ;;;           - 'bad-request (400), 'unauthorized (401), 'forbidden (403), 'not-found (404)
 ;;;           - 'internal-server-error (500), 'bad-gateway (502), 'service-unavailable (503)
 ;;;
 ;;;   - `body`: string or #f - Response body content (default: #f for empty body)
 ;;;         The content that will be sent to the client as the response body.
 ;;;
 ;;;   - `headers`: alist or #f - Additional HTTP headers to include (default: #f for no extra headers)
 ;;;            List of header name/value pairs in the format: `'((header-name . value) ...)`
 ;;;            Example: `'((content-type . "application/json") (cache-control . "no-cache"))`
 ;;;
 ;;; ### Behavior
 ;;;   - Immediately stops execution of the current handler
 ;;;   - Does not return to the calling code
 ;;;   - Bypasses any remaining middleware or handler logic
 ;;;   - Sends the specified response to the client
 ;;;   - Can be called from anywhere within a request handler or middleware
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Simple error response
 ;;; (halt 'not-found "Page not found")
 ;;;
 ;;; ;; JSON API error with custom headers
 ;;; (halt 'bad-request 
 ;;;       "{\"error\": \"Invalid input\"}"
 ;;;       '((content-type . "application/json")))
 ;;;
 ;;; ;; Authentication check with early return
 ;;; (get ("/admin" req params)
 ;;;      (unless (authenticated? req)
 ;;;        (halt 'unauthorized "Access denied"))
 ;;;      ;; Continue with admin logic...
 ;;;      "Admin dashboard")
 ;;;
 ;;; ;; Custom status with no body
 ;;; (halt 'no-content)
 ;;; ```
 (define (halt status #!optional body headers)
   (signal (condition `(halt-condition status ,status body ,body headers ,headers))))

 ;;; Redirect the client to a different URL
 ;;;
 ;;; This function sends an HTTP redirect response to the client, instructing their
 ;;; browser to navigate to a different URL. It's a convenience wrapper around the
 ;;; `halt` function that automatically sets the appropriate Location header and
 ;;; redirect status code.
 ;;;
 ;;; ### Parameters
 ;;;   - `location`: string or uri - The target URL to redirect to
 ;;;             Can be an absolute URL ("https://example.com/page") or relative path ("/login")
 ;;;             If a string is provided, it will be converted to a URI reference
 ;;;
 ;;;   - `status`: symbol - HTTP redirect status code (default: 'found for 302 Found)
 ;;;           Common redirect status codes:
 ;;;           - 'moved-permanently (301): Permanent redirect, search engines update their index
 ;;;           - 'found (302): Temporary redirect, most common for user actions
 ;;;           - 'see-other (303): Redirect after POST to prevent duplicate submissions
 ;;;           - 'temporary-redirect (307): Like 302 but preserves request method
 ;;;           - 'permanent-redirect (308): Like 301 but preserves request method
 ;;;
 ;;; ### Behavior
 ;;;   - Immediately halts current handler execution (does not return)
 ;;;   - Sets the Location header to the specified URL
 ;;;   - Sends empty response body (as per HTTP redirect specification)
 ;;;   - Client browser automatically navigates to the new location
 ;;;
 ;;; ### SEO and Caching Considerations
 ;;;   - Use 'moved-permanently (301) for URLs that have permanently changed
 ;;;   - Use 'found (302) for temporary redirects or user-initiated actions
 ;;;   - Use 'see-other (303) after POST requests to prevent form resubmission
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Simple redirect to login page
 ;;; (redirect "/login")
 ;;;
 ;;; ;; Permanent redirect for moved content
 ;;; (redirect "/new-location" 'moved-permanently)
 ;;;
 ;;; ;; Redirect after successful form submission
 ;;; (post ("/submit" req params)
 ;;;       (process-form-data params)
 ;;;       (redirect "/success" 'see-other))
 ;;;
 ;;; ;; External redirect
 ;;; (redirect "https://external-site.com/page")
 ;;;
 ;;; ;; Conditional redirect based on authentication
 ;;; (get ("/dashboard" req params)
 ;;;      (if (authenticated? req)
 ;;;          "Welcome to your dashboard"
 ;;;          (redirect "/login")))
 ;;; ```
 (define (redirect location #!optional (status 'found))
   (let ((location (if (string? location) (uri-reference location) location)))
     (halt status "" `((location . (,location))))))

 (define (headers-for-file path)
   (let* ((req (current-request))
          (h   (request-headers req))
          (size (file-size path))
          (last-modified (file-modification-time path))
          (ext (pathname-extension path)))
     `((last-modified #(,(seconds->utc-time last-modified) ()))
       (content-length ,size)
       (content-type ,(file-extension->mime-type ext)))))

 ;; Serve static files from a directory
 ;;
 ;; Registers a route to serve static files from the filesystem.
 ;; Uses a wildcard pattern to capture all remaining path segments.
 ;;
 ;; ### Parameters
 ;;   - `path-prefix`: string - URL path prefix (e.g., "/static", "/assets")
 ;;   - `directory`: string - Local filesystem directory to serve files from
 ;;
 ;; ### Examples
 ;; ```scheme
 ;; (static "/static" "./public")
 ;; ;; Now /static/style.css serves ./public/style.css
 ;; ;; And /static/js/app.js serves ./public/js/app.js
 ;; ```
 (define (static path-prefix directory)
   (let ((route-pattern (string-append path-prefix "/*")))
     (get (route-pattern)
          (let* ((file-path (alist-ref "*" (current-params) equal?))
                 (full-path (make-pathname directory file-path)))
            ;; Security: prevent directory traversal
            (if (string-contains file-path "..")
                (halt 'not-found "File not found"))
            (if (and (file-exists? full-path)
                     (not (directory-exists? full-path)))
                `(static-file ,full-path ,(headers-for-file full-path))
                (halt 'not-found "File not found"))))))

 (define (is-response? list)
   (and
    ;; is this a list?
    (list? list)
    ;; we need a pair of at least two items: status & body
    (>= (length list) 2)
    ;; first element should be a valid status symbol
    (symbol? (car list))
    ;; next item should be a valid body-type
    (string? (cadr list))
    ;; should check for headers next
    (if (= 3 (length list)) (list? (list-ref list 2)) #t)))

 ;; updates the response with the schematra tuple (mimicking a Rack
 ;; response). If there's a body in the tuple, it will set
 ;; (current-body) to that.
 (define (update-response-with-tuple! response tuple)
   (cond
    [(string? tuple)
     (current-body tuple)
     (update-response response status: 'ok)]
    [(is-response? tuple)
     (current-body (cadr tuple))
     (let* ((new-headers (if (= 3 (length tuple)) (caddr tuple) '()))
            (raw-status (car tuple))
            (status (if (eq? raw-status 'static-file) 'ok raw-status)))
       (update-response
        response
        status: status
        headers: (intarweb:headers new-headers (response-headers response))))]
    [else
     (current-body (format #f "Error: response type not supported (~A)" tuple))
     (update-response
      response
      status: 'error)]))

 ;;; Extract the request body as a string
 ;;;
 ;;; Reads the HTTP request body from the request port and returns it as a string.
 ;;; This function handles both requests with and without Content-Length headers.
 ;;; It's commonly used in POST request handlers to access form data, JSON payloads,
 ;;; or other request body content.
 ;;;
 ;;; ### Parameters
 ;;;   - `request`: HTTP request object containing headers, method, URI, and port
 ;;;
 ;;; ### Returns
 ;;; A string containing the complete request body content
 ;;;
 ;;; ### Behavior
 ;;;   - If Content-Length header is present, reads exactly that many bytes
 ;;;   - If Content-Length header is missing, reads until EOF
 ;;;   - Returns empty string if no body content is available
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; (post ("/submit" req params)
 ;;;       (let ((body (request-body-string req)))
 ;;;         (format "Received: ~A" body)))
 ;;; ```
 (define (request-body-string request)
   (let* ((in-port (request-port request))
          (headers (request-headers request))
          (content-length (header-value 'content-length headers #f))
          (body (if content-length
                    (read-string content-length in-port)
                    (read-string #f in-port))))
     body))

 (define request-cookies (make-parameter #f))
 (define response-cookies (make-parameter #f))

 ;;; Set a cookie to be sent in the HTTP response
 ;;;
 ;;; This function queues a cookie to be included in the Set-Cookie headers of the
 ;;; current HTTP response. Cookies set with this function will be sent to the client
 ;;; and stored in their browser according to the specified attributes.
 ;;;
 ;;; ### Parameters
 ;;;   - `key`: string - The cookie name/key
 ;;;   - `val`: string - The cookie value
 ;;;
 ;;; ### Keyword Parameters
 ;;;   - `path`: uri struct (as defined in uri-common) - URL path where the
 ;;;         cookie is valid (default: "/") The cookie will only be
 ;;;         sent for requests matching this path prefix.  Examples:
 ;;;         "/" (entire site), "/admin" (admin section only)
 ;;;
 ;;;   - `max-age`: string or #f - Cookie lifetime in seconds (default: #f for session cookie)
 ;;;            If specified, cookie expires after this many seconds.
 ;;;            If #f, cookie expires when browser session ends.
 ;;;            Examples: "3600" (1 hour), "86400" (1 day), "0" (delete immediately)
 ;;;
 ;;;   - `secure`: boolean - Whether cookie should only be sent over HTTPS (default: #f)
 ;;;           When #t, cookie will only be transmitted over secure connections.
 ;;;           Should be #t for sensitive data in production.
 ;;;
 ;;;   - `http-only`: boolean - Whether cookie should be inaccessible to JavaScript (default: #f)
 ;;;              When #t, prevents client-side scripts from accessing the cookie,
 ;;;              providing protection against XSS attacks.
 ;;;
 ;;; ### Behavior
 ;;;   - Must be called within a request handler context where cookies parameter is initialized
 ;;;   - Silently fails if called outside of request context (when cookies is #f)
 ;;;   - Multiple calls with same key will overwrite previous value
 ;;;   - Cookies are automatically included in response headers by the router
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Simple session cookie
 ;;; (cookie-set! "user_id" "12345")
 ;;;
 ;;; ;; Persistent cookie with 1 day expiration
 ;;; (cookie-set! "preferences" "theme=dark" max-age: "86400")
 ;;;
 ;;; ;; Secure authentication cookie
 ;;; (cookie-set! "auth_token" token-value 
 ;;;              secure: #t 
 ;;;              http-only: #t 
 ;;;              max-age: "3600")
 ;;; ```
 (define (cookie-set! key val #!key
                      (path (uri-reference "/"))
                      (max-age #f)
                      (secure #f)
                      (http-only #f)
                      (domain #f))
   (hash-table-set! (response-cookies) key
                    `#(,val
                       ((path . ,path)
                        ,@(if max-age `((max-age . ,max-age)) '())
                        ,@(if secure `((secure . #t)) '())
                        ,@(if http-only `((http-only . #t)) '())
                        ,@(if domain `((domain . #t)) '())))))

 (define (cookie-ref key #!optional default)
   (if (hash-table? (request-cookies))
       (hash-table-ref/default (request-cookies) key default)
       default))

 ;; delete a cookie. Make sure the path/scope is the same, otherwise
 ;; the old one will still be kept
 (define (cookie-delete! key #!key (path (uri-reference "/")))
   (cookie-set! key "" max-age: 0 path: path))

 ;; returns an association list that con be used to build headers
 (define (cookies->alist cookies)
   (hash-table-map cookies
                   (lambda (key val-vector)
                     (let ((val (vector-ref val-vector 0)))
                       `(set-cookie #((,key . ,val) ,(vector-ref val-vector 1))))
                     )))

 ;; helper to output json content
 (define (send-json-response datum #!optional (status 'ok))
   (let ((output (json->string datum)))
    `(,status ,output ((content-type application/json)))))

 ;; middleware
 (define middleware-stack '())

 (define (use-middleware! middleware)
   (set! middleware-stack (append middleware-stack (list middleware))))

 (define (apply-middleware-stack handler)
   (let loop ((middlewares middleware-stack))
     (if (null? middlewares)
         (handler)
         (let ((middleware (car middlewares))
               (remaining  (cdr middlewares)))
           (middleware (lambda () (loop remaining)))))))

 (define (build-error-page exn)
   (ccup->html
    `[p "Got an error: " ,(format "~a" exn)]))

 (handle-exception
  (lambda (exn chain)
    (let ((is-sse (and (current-response)
                       (header-value 'x-sse-handler (response-headers (current-response)))))
          (thread-id (thread-name (current-thread))))
      (if is-sse
          (log-err "[ERROR] SSE connection closed: ~A" exn)
          ;; only send status for other reqs
	  (begin
	    (log-err (build-error-message exn chain #t))
            (send-status 'internal-server-error (build-error-page exn)))))))

 (define current-body (make-parameter #f))

 (define current-params (make-parameter '()))

 ;; from spiffy.scm
 (define (call-with-input-file* file proc)
   (call-with-input-file file (lambda (p)
                                (handle-exceptions exn
                                                   (begin (close-input-port p) (raise exn))
                                                   (proc p)))
                         #:binary))

 ;; router
 (define (schematra-router continue)
   (let* ((request (current-request))
          (headers (request-headers request))
          (raw-cookies (header-values 'cookie headers))
          (method (request-method request))
          (uri (request-uri request))
          (normalized-path (normalize-path (uri-path uri)))
          (route-handlers (hash-table-ref/default resources-tree-for-verb method #f))
          (resource (and route-handlers (find-resource normalized-path route-handlers))))
     (if resource
         (parameterize ((request-cookies (alist->hash-table raw-cookies))
                        (response-cookies (make-hash-table))
                        (current-body #f)
                        (current-params '()))
           (let* ((handler (car resource))
                  (route-params (cadr resource)))
             (current-params (append route-params (uri-query uri)))
             ;; this condition-case is here to handle halts that might happen mid-routing
             (condition-case
                 (let* ((response-tuple (apply-middleware-stack handler))
                        (orig-status (if (list? response-tuple) (car response-tuple) #f))
                        (old-headers (response-headers (current-response)))
                        (new-headers (intarweb:headers (cookies->alist (response-cookies))
                                                       old-headers)))
                   (current-response (update-response (current-response)
                                                      headers: new-headers))
                   (current-response (update-response-with-tuple! (current-response) response-tuple))
                   ;; need to include the body here, either send the file or just the string
                   (if (eq? orig-status 'static-file)
                       (condition-case
                           (call-with-input-file* (current-body)
                                                  (lambda (f)
                                                    (write-logged-response)
                                                    (sendfile f (response-port (current-response)))
                                                    (finish-response-body (current-response))))
                         [(exn i/o file) (send-status 'forbidden)])
                       (send-response body: (current-body))))
               [exn (halt-condition)
                    (let* ((status       (get-condition-property exn 'halt-condition 'status))
                           (body         (get-condition-property exn 'halt-condition 'body))
                           (halt-headers (get-condition-property exn 'halt-condition 'headers))
                           (new-headers  (append (or halt-headers '()) (cookies->alist (response-cookies)))))
                      (current-response (update-response-with-tuple! (current-response) (list status body new-headers)))
                      (send-response status: status body: body))])))
         ;; resource not found, let if spiffy handle it
         (continue))))

 ;;; Install the Schematra router as a virtual host handler
 ;;; 
 ;;; This function configures the Spiffy web server to use Schematra's routing system.
 ;;; It maps the default virtual host pattern (.*) to the schematra-router function,
 ;;; which will handle all incoming HTTP requests by matching them against registered
 ;;; routes and calling the appropriate handlers.
 ;;;
 ;;; Call this function before starting the server to enable route handling.
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; (schematra-install)
 ;;; (start-server port: 8080)
 ;;; ```
 (define (schematra-install)
   (let ((vhost (schematra-default-vhost)))
     (vhost-map
      `((,vhost . ,(lambda (continue) (schematra-router continue)))))))


 (define schematra-logo
   "                                                .:::.
                                            .-+##%%%%=
                                          -*%%#*****#%=
                                      .:=#%#*********#%=
                                   :+#%%#*************#%-
                            .     +%#*************###%%%%:             .
                          :+-    .%#*********##%%%%%%%%%@#-==++++=.    -+:
                        .=*:     .%#*****#%%%%%%%%%%%%#####**##%@%.     :*=.
                       .+*.       ##**#%%%%%%%%##*********##%%@%*.       .*+.
                      .+*:        *%#%%%%%##******######%%%%@%+:          :*+.
                     .+*-         -@%%#*****####**+=-:. :*@*-.             -*+
                     =*+        :+##***###*+=-..          *#                +*=
                    :**:      -*##*##%%+:.       :##=     .%=               -**.
                    =*+.     +@###%%%%+          *@%%.     ##--:.           .**=
                   .+*=      =%%%%%%%#           =%%+   :+########+:         +*+
                   :**-       .:---+%-            .:  :*%*+++++++*#%+.       =**.
                   :**-            **                =%%#******####%%+       -**:
                   :**-           -%:                *%*#%%%%++=-::...       -**:
                   :**-           *#                 =%#+*#%%=:.             -**:
                   .**=          :%-                .###%#***%@%.            =**.
                    +*+          *#                 +%++*%%%%*=:             +*+
                    -**:        =%:                .%*+++#%+##.             :**-
                    .+*=      .+%-                 :%*+++*%*+%+             =*+.
                     -**:    +%*:                  .%#+++#%*+%*            :**:
                      =*+.   =%#===+*+              :#%##%%%#*:           .+*-
                       =*+    .-=++=%*               .-==:*%-             +*=
                        -*+.        ##    ..               *#.          .+*-
                         :++:       :%+  .#%:              .%+         :++.
                           :=.       -%#-##*%=.     +=.     =%:       .=:
                                      .=#%: -#%*-:. +@%*+=--=@*
                                         .    :=*####%=-=+**++-")

 (define (schematra-banner)
   (let ((port (server-port))
         (address (or (server-bind-address) "(all)")))
     (conc schematra-logo "\n"
           "Schematra version: " version-major "." version-minor "." version-patch "\n"
           "Listening on " address ":" port "\n")))

 ;;; Start the Schematra web server
 ;;;
 ;;; This function starts the Spiffy web server with Schematra routing enabled.
 ;;; It supports both production and development modes with different behaviors.
 ;;;
 ;;; ### Parameters
 ;;;   - `development?`: boolean - Enable development mode (default: #f)
 ;;;   - `port`: integer - HTTP server port (default: 8080)  
 ;;;   - `repl-port`: integer - REPL port for development mode (default: 1234)
 ;;;
 ;;; ### Development Mode
 ;;; When `development?` is #t, the server runs in a separate thread and starts
 ;;; an nREPL server on the specified repl-port for interactive development.
 ;;; This allows you to connect with a REPL client and modify routes/handlers
 ;;; while the server is running.
 ;;;
 ;;; **IMPORTANT:** Development mode requires the 'nrepl' egg to be installed:
 ;;; ```bash
 ;;; $ chicken-install nrepl
 ;;; ```
 ;;;
 ;;; Development mode also enables request logging to stdout.
 ;;;
 ;;; ### Production Mode
 ;;; When `development?` is #f (default), starts the server normally in the
 ;;; current thread without REPL access.
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Production mode
 ;;; (schematra-start port: 3000)
 ;;;
 ;;; ;; Development mode with custom ports
 ;;; (schematra-start development?: #t port: 8080 repl-port: 1234)
 ;;; ```
 (define (schematra-start #!key (development? #f) (port 8080) (repl-port 1234) (bind-address #f) (nrepl? #t))
   (access-log ##sys#standard-output)
   (error-log ##sys#standard-error)

   ;; NOTE: figure out if we want to keep these params as arguments to
   ;; the start function or let the user set them as they want.
   (server-software `(("Schematra"
                       ,(conc version-major "." version-minor)
                       ,(conc "Running on CHICKEN " (chicken-version)))))
   (display (schematra-banner))
   (server-port port)
   (server-bind-address bind-address)

   (if development?
       (begin
         (set! development-mode? #t)
         ;; start the server inside a thread, then start the nrepl in port `repl-port`
         (thread-start!
          (lambda ()
            (start-server)))
         (when nrepl?
           (import nrepl)
           (nrepl repl-port)))
       (start-server))))
