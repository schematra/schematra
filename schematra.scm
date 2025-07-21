;; Schematra - a very simple web framework for scheme inspired in
;; Sinatra
;; Copyright (c) 2025 Rolando Abarca <cpm.rolandoa@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

(module schematra
 ( ;; Parameters
  schematra-default-handler
  schematra-default-vhost
  ;; Procedures
  get post
  log-err log-dbg
  request-body-string
  schematra-install
  schematra-start
  ) ; end export list

 (import scheme)
 (import
  (chicken base)
  (chicken io)
  spiffy
  format
  intarweb
  uri-common
  srfi-1
  srfi-13
  srfi-18)

 ;; Default virtual host pattern for Schematra routing
 ;;
 ;; This parameter defines the regular expression pattern used to match virtual hosts
 ;; when installing the Schematra router. The default pattern ".*" matches all hostnames,
 ;; meaning the router will handle requests for any domain or IP address.
 ;;
 ;; You can customize this to restrict routing to specific domains:
 ;;   - "example\\.com" - matches only example.com
 ;;   - "(api|www)\\.example\\.com" - matches api.example.com and www.example.com
 ;;   - "localhost" - matches only localhost
 ;;
 ;; This parameter is used by schematra-install when configuring the vhost-map.
 (define schematra-default-vhost ".*")

 ;; Default handler for unmatched routes
 ;; 
 ;; This handler is called when no specific route matches the incoming request.
 ;; It serves as a fallback and returns a simple welcome message. This handler
 ;; is automatically registered for the root path ("/") when the route trees
 ;; are initialized.
 ;; 
 ;; Parameters:
 ;;   _request: the HTTP request object (ignored by default handler)
 ;;   params: optional parameter alist (ignored by default handler)
 ;; 
 ;; Returns:
 ;;   A string containing the default welcome message
 (define (schematra-default-handler _request #!optional params)
   "Welcome to Schematra, Sinatra's weird friend.")

 ;; Log an error message to the error log
 ;;
 ;; Writes a formatted error message to the configured error log stream.
 ;; By default, this writes to standard error (stderr). The error log destination
 ;; can be configured using Spiffy's error-log parameter.
 ;;
 ;; Parameters:
 ;;   format: string - Format string compatible with the format procedure
 ;;   rest: any - Additional arguments for the format string
 ;;
 ;; Example usage:
 ;;   (log-err "Database connection failed: ~A" error-message)
 ;;   (log-err "Invalid user ID: ~A (expected number)" user-id)
 (define (log-err format . rest)
   (apply log-to (error-log) format rest))

 ;; Log a debug/access message to the access log
 ;;
 ;; Writes a formatted debug or access message to the configured access log stream.
 ;; By default, this writes to standard output (stdout). The access log destination
 ;; can be configured using Spiffy's access-log parameter.
 ;;
 ;; This is commonly used for request logging, debugging information, and general
 ;; application status messages that don't indicate errors.
 ;;
 ;; Parameters:
 ;;   format: string - Format string compatible with the format procedure
 ;;   rest: any - Additional arguments for the format string
 ;;
 ;; Example usage:
 ;;   (log-dbg "Processing request for path: ~A" path)
 ;;   (log-dbg "User ~A logged in successfully" username)
 ;;   (log-dbg "Cache hit for key: ~A" cache-key)
 (define (log-dbg format . rest)
   (apply log-to (access-log) format rest))

 (define (make-path-tree)
   `("/" ,schematra-default-handler))

 ;; find a resource based on a path on a tree. Returns the handler if
 ;; found, #f otherwise.
 (define (find-resource path tree)
   (find-resource-with-params path tree '()))

 (define (find-resource-with-params path tree params)
   (cond
    ;; If tree is not a list or is empty, no match
    [(not (and (list? tree) (>= (length tree) 2)))
     #f]
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
               (let ((result (find-resource-with-params (cdr path) (car subtrees) new-params)))
                 (if result result (loop (cdr subtrees))))]
              [else (loop (cdr subtrees))]))))]
    ;; No match with current tree node
    [else #f]))

 (define (add-resource! path tree handler)
   (if (null? path)
       tree                    ; Empty path, return tree as-is
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
                   (append result (list (add-resource! remaining-path (list (car remaining-path) #f) handler)))]
                  [(and (list? (car subtrees)) (string=? (car remaining-path) (caar subtrees)))
                   ;; Found matching subtree
                   (append result
                           (list (add-resource! remaining-path (car subtrees) handler))
                           (cdr subtrees))]
                  [else
                   ;; Keep looking
                   (loop (cdr subtrees) (append result (list (car subtrees))))])))]
          ;; Tree doesn't match, create new tree for this path
          [else
           (if (null? remaining-path)
               (list target-segment handler)
               (list target-segment #f (add-resource! remaining-path (list (car remaining-path) #f) handler)))]))))

 ;; empty routes. each verb has a list of routes
 (define get-routes (make-path-tree))
 (define post-routes (make-path-tree))
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

 ;; Register a GET route handler
 ;;
 ;; Registers a handler function to respond to HTTP GET requests for a specific path.
 ;; The path can include parameter segments prefixed with ':' to capture dynamic values.
 ;;
 ;; Parameters:
 ;;   path: string - URL path pattern (e.g., "/users", "/api/posts/:id", "/users/:user-id/posts/:post-id")
 ;;   body: procedure - Handler function that processes the request
 ;;
 ;; Handler Function Signature:
 ;;   The handler function must accept two arguments:
 ;;     request: HTTP request object containing headers, method, URI, etc.
 ;;     params: association list containing both path parameters and query parameters
 ;;
 ;; Parameters:
 ;;   The params argument contains two types of parameters:
 ;;   1. Path Parameters (string keys): URL segments starting with ':' become parameters
 ;;      - Route "/users/:id" matches "/users/123" 
 ;;      - Contributes '(("id" . "123")) to params
 ;;      - Route "/users/:user-id/posts/:post-id" matches "/users/alice/posts/42"
 ;;      - Contributes '(("user-id" . "alice") ("post-id" . "42")) to params
 ;;   2. Query Parameters (symbol keys): URL query string parameters
 ;;      - Request "/users/123?format=json&limit=10"
 ;;      - Contributes '((format . "json") (limit . "10")) to params
 ;;   
 ;;   Note: Path parameters use string keys, query parameters use symbol keys.
 ;;   This allows you to distinguish between the two types when processing params.
 ;;
 ;; Handler Return Value:
 ;;   The handler should return a string (which becomes the response body with 200 OK status)
 ;;   or a response list in the format (status body [headers]).
 ;;
 ;; Example usage:
 ;;   ;; Simple static route
 ;;   (get "/hello" (lambda (req params) "Hello, World!"))
 ;;
 ;;   ;; Route with parameters
 ;;   (get "/users/:id" 
 ;;        (lambda (req params)
 ;;          (let ((user-id (alist-ref "id" params equal?)))
 ;;            (format "User ID: ~A" user-id))))
 ;;
 ;;   ;; Route with multiple parameters
 ;;   (get "/users/:user-id/posts/:post-id"
 ;;        (lambda (req params)
 ;;          (let ((user-id (alist-ref "user-id" params equal?))
 ;;                (post-id (alist-ref "post-id" params equal?)))
 ;;            (format "User ~A, Post ~A" user-id post-id))))
 (define (get path body)
   (let ((raw-uri-path (uri-path (uri-reference path))))
     (set! get-routes
           (add-resource! (normalize-path raw-uri-path) get-routes body))))

 ;; Register a POST route handler
 ;;
 ;; Registers a handler function to respond to HTTP POST requests for a specific path.
 ;; The path can include parameter segments prefixed with ':' to capture dynamic values.
 ;;
 ;; Parameters:
 ;;   path: string - URL path pattern (e.g., "/users", "/api/posts", "/users/:id")
 ;;   body: procedure - Handler function that processes the request
 ;;
 ;; Handler Function:
 ;;   See the 'get' function documentation for complete details on handler function
 ;;   signature, path parameters, query parameters, and return values. POST handlers
 ;;   work identically to GET handlers in terms of parameter handling and responses.
 (define (post path body)
   (let ((raw-uri-path (uri-path (uri-reference path))))
     (set! post-routes
           (add-resource! (normalize-path raw-uri-path) post-routes body))))

 (define (alist? x)
   (and (list? x)
        (every pair? x)))

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
    (if (= 3 (length list)) (alist? (list-ref list 2)) #t)))

 (define (parse-response resp)
   (cond
    [(string? resp) (send-response status: 'ok body: resp)]
    [else
     (send-response
      status: 'error
      body: (format #f "Error: response type not supported (~A)" resp))]))

 ;; Extract the request body as a string
 ;;
 ;; Reads the HTTP request body from the request port and returns it as a string.
 ;; This function handles both requests with and without Content-Length headers.
 ;; It's commonly used in POST request handlers to access form data, JSON payloads,
 ;; or other request body content.
 ;;
 ;; Parameters:
 ;;   request: HTTP request object containing headers, method, URI, and port
 ;;
 ;; Returns:
 ;;   A string containing the complete request body content
 ;;
 ;; Behavior:
 ;;   - If Content-Length header is present, reads exactly that many bytes
 ;;   - If Content-Length header is missing, reads until EOF
 ;;   - Returns empty string if no body content is available
 ;;
 ;; Example usage:
 ;;   (post "/submit" 
 ;;         (lambda (req params)
 ;;           (let ((body (request-body-string req)))
 ;;             (format "Received: ~A" body))))
 (define (request-body-string request)
   (let* ((in-port (request-port request))
          (headers (request-headers request))
          (content-length (header-value 'content-length headers #f))
          (body (if content-length
                    (read-string content-length in-port)
                    (read-string #f in-port))))
     body))

 (define (schematra-router continue)
   (let* ((request (current-request))
          (method (request-method request))
          (uri (request-uri request))
          (normalized-path (normalize-path (uri-path uri)))
          (route-handlers
           (cond
            [(eq? method 'GET) get-routes]
            [(eq? method 'POST) post-routes]
            [else (error "no handlers for this method")]))
          (result (find-resource normalized-path route-handlers)))
     (if development-mode?
         (log-to (access-log) "Req: ~A. Path: ~A. Method: ~A" request normalized-path method))
     (if result 
         (let* ((handler (car result))
                (route-params (cadr result))
                (params (append route-params (uri-query uri))))
           (parse-response (handler request params)))
         (continue))))

 ;; Install the Schematra router as a virtual host handler
 ;; 
 ;; This function configures the Spiffy web server to use Schematra's routing system.
 ;; It maps the default virtual host pattern (.*) to the schematra-router function,
 ;; which will handle all incoming HTTP requests by matching them against registered
 ;; routes and calling the appropriate handlers.
 ;;
 ;; Call this function before starting the server to enable route handling.
 ;; Example usage:
 ;;   (schematra-install)
 ;;   (start-server port: 8080)
 (define (schematra-install)
   (vhost-map
    `((,schematra-default-vhost . ,(lambda (continue) (schematra-router continue))))))

 ;; Start the Schematra web server
 ;;
 ;; This function starts the Spiffy web server with Schematra routing enabled.
 ;; It supports both production and development modes with different behaviors.
 ;;
 ;; Parameters:
 ;;   development?: boolean - Enable development mode (default: #f)
 ;;   port: integer - HTTP server port (default: 8080)  
 ;;   repl-port: integer - REPL port for development mode (default: 1234)
 ;;
 ;; Development Mode:
 ;;   When development? is #t, the server runs in a separate thread and starts
 ;;   an nREPL server on the specified repl-port for interactive development.
 ;;   This allows you to connect with a REPL client and modify routes/handlers
 ;;   while the server is running.
 ;;
 ;;   IMPORTANT: Development mode requires the 'nrepl' egg to be installed:
 ;;     $ chicken-install nrepl
 ;;
 ;;   Development mode also enables request logging to stdout.
 ;;
 ;; Production Mode:
 ;;   When development? is #f (default), starts the server normally in the
 ;;   current thread without REPL access.
 ;;
 ;; Example usage:
 ;;   ;; Production mode
 ;;   (schematra-start port: 3000)
 ;;
 ;;   ;; Development mode with custom ports
 ;;   (schematra-start development?: #t port: 8080 repl-port: 1234)
 (define (schematra-start #!key (development? #f) (port 8080) (repl-port 1234))
   (access-log ##sys#standard-output)
   (error-log ##sys#standard-error)
   (if development?
       (begin
         (import nrepl)
         (set! development-mode? #t)
         ;; start the server inside a thread, then start the nrepl in port `repl-port`
         (thread-start!
          (lambda ()
            (start-server port: port)))
         (nrepl repl-port))
       (start-server port: port))))
