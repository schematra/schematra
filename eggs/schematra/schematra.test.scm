;; Schematra Testing Helpers
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
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
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

(module schematra.test
  (make-mock-request
   make-mock-response
   test-route
   test-route-status
   test-route-body
   test-route-headers
   test-route-full
   test-route-cookies
   response-cookie-value
   test-route-redirect-location
   with-test-app)

  (import scheme)
  (import
   chicken.base
   chicken.string
   spiffy
   (rename intarweb (headers intarweb:headers))
   srfi-13
   uri-common
   schematra)

;; Create a mock HTTP request for testing
;;
;; Creates a mock request object that can be used with schematra-route-request
;; for testing routes without an actual HTTP server.
;;
;; ### Parameters
;;   - `method`: symbol - HTTP method ('GET, 'POST, 'PUT, 'DELETE, etc.)
;;   - `path`: string - URL path (can include query string, e.g., "/users/123?format=json")
;;
;; ### Keyword Parameters
;;   - `headers`: alist - HTTP headers to include (default: '())
;;         Format: '((header-name value) ...)
;;         Example: '((content-type application/json))
;;   - `body`: string or #f - Request body content (default: #f)
;;   - `cookies`: alist - Cookies to include (default: '())
;;         Format: '((name . value) ...)
;;         Example: '(("flavor" . "chocolate") ("size" . "large"))
;;         These are parsed as real HTTP Cookie headers so cookie-ref works correctly.
;;
;; ### Examples
;; ```scheme
;; ;; Simple GET request
;; (make-mock-request 'GET "/hello")
;;
;; ;; POST with JSON body and headers
;; (make-mock-request 'POST "/api/users"
;;                    body: "{\"name\":\"Alice\"}"
;;                    headers: '((content-type application/json)))
;;
;; ;; GET with cookies
;; (make-mock-request 'GET "/profile"
;;                    cookies: '(("session_id" . "abc123")))
;; ```
(define (make-mock-request method path #!key headers body cookies)
  (let* ((cookie-base
          (if (and cookies (pair? cookies))
              ;; Parse raw Cookie header to get correct internal representation
              (let* ((cookie-str (string-join
                                  (map (lambda (c)
                                         (string-append (car c) "=" (cdr c)))
                                       cookies)
                                  "; "))
                     (raw (string-append "Cookie: " cookie-str "\r\n\r\n")))
                (read-headers (open-input-string raw)))
              (intarweb:headers '())))
         (final-headers (intarweb:headers (or headers '()) cookie-base)))
    (make-request method: method
                  uri: (uri-reference path)
                  port: (if body
                            (open-input-string body)
                            (open-input-string ""))
                  headers: final-headers)))

;; Create a mock HTTP response for testing
;;
;; Creates a mock response object that can be used with schematra-route-request
;; for testing routes without an actual HTTP server.
;;
;; ### Keyword Parameters
;;   - `headers`: alist - HTTP headers to include in response (default: '())
;;
;; ### Examples
;; ```scheme
;; (make-mock-response)
;; (make-mock-response headers: '((cache-control . "no-cache")))
;; ```
(define (make-mock-response #!key headers)
  (make-response port: (open-output-string)
                 headers: (intarweb:headers (or headers '()))))

;; Test a route and return the response tuple
;;
;; This is the main testing helper that simulates an HTTP request to a route
;; and returns the complete response tuple.
;;
;; ### Parameters
;;   - `app`: schematra-app - The app to test
;;   - `method`: symbol - HTTP method ('GET, 'POST, 'PUT, 'DELETE, etc.)
;;   - `path`: string - URL path (can include query string)
;;
;; ### Keyword Parameters
;;   - `headers`: alist - Request headers (default: '())
;;   - `body`: string or #f - Request body content (default: #f)
;;
;; ### Returns
;; - Response tuple: `'(status body headers)` when route matches
;; - `#f` when route not found (404)
;;
;; ### Examples
;; ```scheme
;; ;; Test GET request
;; (test-route app 'GET "/hello")
;; ;; => '(ok (ccup [h1 "Hello"]) ())
;;
;; ;; Test POST with query params
;; (test-route app 'POST "/api/echo?name=Alice")
;; ;; => '(ok "{\"message\":\"Hello Alice\"}" ((content-type application/json)))
;;
;; ;; Test 404
;; (test-route app 'GET "/unknown")
;; ;; => #f
;; ```
(define (test-route app method path #!key headers body cookies)
  (with-schematra-app app
    ((current-request (make-mock-request method path headers: headers body: body cookies: cookies))
     (current-response (make-mock-response)))
    (lambda ()
      (schematra-route-request (current-request)))))

;; Test a route and return only the status
;;
;; Convenience helper that extracts just the status symbol from the response tuple.
;;
;; ### Parameters
;; Same as `test-route`
;;
;; ### Returns
;; - Status symbol: `'ok`, `'created`, `'not-found`, etc. when route matches
;; - `#f` when route not found
;;
;; ### Examples
;; ```scheme
;; (test-route-status app 'GET "/hello")
;; ;; => 'ok
;;
;; (test-route-status app 'GET "/unknown")
;; ;; => #f
;; ```
(define (test-route-status app method path #!key headers body cookies)
  (let ((tuple (test-route app method path headers: headers body: body cookies: cookies)))
    (and tuple (car tuple))))

;; Test a route and return only the body
;;
;; Convenience helper that extracts just the body content from the response tuple.
;; The body can be a chiccup form, JSON string, or any other response body.
;;
;; ### Parameters
;; Same as `test-route`
;;
;; ### Returns
;; - Body content when route matches
;; - `#f` when route not found
;;
;; ### Examples
;; ```scheme
;; (test-route-body app 'GET "/hello")
;; ;; => '(ccup [h1 "Hello"])
;;
;; (test-route-body app 'POST "/api/echo?name=Alice")
;; ;; => "{\"message\":\"Hello Alice\"}"
;;
;; (test-route-body app 'GET "/unknown")
;; ;; => #f
;; ```
(define (test-route-body app method path #!key headers body cookies)
  (let ((tuple (test-route app method path headers: headers body: body cookies: cookies)))
    (and tuple (cadr tuple))))

;; Test a route and return only the headers
;;
;; Convenience helper that extracts just the response headers from the tuple.
;;
;; ### Parameters
;; Same as `test-route`
;;
;; ### Returns
;; - Headers alist when route matches
;; - `#f` when route not found
;;
;; ### Examples
;; ```scheme
;; (test-route-headers app 'POST "/api/users")
;; ;; => '((content-type application/json))
;;
;; (test-route-headers app 'GET "/unknown")
;; ;; => #f
;; ```
(define (test-route-headers app method path #!key headers body cookies)
  (let ((tuple (test-route app method path headers: headers body: body cookies: cookies)))
    (and tuple (caddr tuple))))

;; Test a route and return full response including cookies
;;
;; Returns a 4-element list: (status body headers cookies)
;; where cookies is an alist of (name . value) pairs extracted from
;; Set-Cookie response headers.
;;
;; ### Parameters
;; Same as `test-route`
;;
;; ### Returns
;; - `(status body headers cookies)` when route matches
;; - `#f` when route not found
;;
;; ### Examples
;; ```scheme
;; (test-route-full app 'GET "/set-cookie")
;; ;; => (ok "done" () ((my-cookie . "value")))
;; ```
(define (test-route-full app method path #!key headers body cookies)
  (with-schematra-app app
    ((current-request (make-mock-request method path headers: headers body: body cookies: cookies))
     (current-response (make-mock-response)))
    (lambda ()
      (let ((tuple (schematra-route-request (current-request))))
        (if tuple
            (let* ((resp-headers (response-headers (current-response)))
                   (cookies (header-values 'set-cookie resp-headers)))
              (list (car tuple) (cadr tuple)
                    (if (>= (length tuple) 3) (caddr tuple) '())
                    cookies))
            #f)))))

;; Test a route and return only the cookies
;;
;; Returns an alist of (name . value) pairs from Set-Cookie response headers.
;;
;; ### Parameters
;; Same as `test-route`
;;
;; ### Returns
;; - Cookies alist when route matches
;; - `#f` when route not found
(define (test-route-cookies app method path #!key headers body cookies)
  (let ((result (test-route-full app method path headers: headers body: body cookies: cookies)))
    (and result (cadddr result))))

;; Extract a specific cookie value from a test-route-full result
;;
;; ### Parameters
;;   - `full-result`: list - Result from test-route-full
;;   - `cookie-name`: string - Name of the cookie to find
;;
;; ### Returns
;; - Cookie value string, or #f if not found
(define (response-cookie-value full-result cookie-name)
  (and full-result
       (let ((cookies (cadddr full-result)))
         (let loop ((cs cookies))
           (cond
            ((null? cs) #f)
            ((and (pair? (car cs))
                  (equal? (caar cs) cookie-name))
             (cdar cs))
            (else (loop (cdr cs))))))))

;; Test a route and return the redirect Location header
;;
;; ### Parameters
;; Same as `test-route`
;;
;; ### Returns
;; - Location URI string when route redirects
;; - `#f` when route not found or no Location header
(define (test-route-redirect-location app method path #!key headers body cookies)
  (with-schematra-app app
    ((current-request (make-mock-request method path headers: headers body: body cookies: cookies))
     (current-response (make-mock-response)))
    (lambda ()
      (let ((tuple (schematra-route-request (current-request))))
        (and tuple
             (let* ((resp-headers (response-headers (current-response)))
                    (location (header-value 'location resp-headers #f)))
               (and location (uri->string location))))))))

;; Convenience macro for creating a test app with routes
;;
;; ### Parameters
;;   - `app-var`: symbol - Variable name to bind the app to
;;   - `body`: expressions - Route definitions and other setup
;;
;; ### Returns
;; The created app
;;
;; ### Examples
;; ```scheme
;; (with-test-app my-app
;;   (get "/hello" "Hello World"))
;; ;; my-app is now a schematra app with the /hello route
;; ```
(define-syntax with-test-app
  (syntax-rules ()
    ((_ app-var body ...)
     (let ((app-var (schematra/make-app)))
       (with-schematra-app app-var
         (lambda ()
           body ...))
       app-var))))

) ;; end module
