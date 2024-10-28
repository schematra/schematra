(define ex1 '(#<<EXAMPLE
;; Complete web app in just a few lines
(import schematra chiccup sessions)

(define app (schematra/make-app))
(with-schematra-app app
 (use-middleware! (session-middleware "secret-key"))

 (get "/"
      (let ((user (session-get "username")))
        (if user
            (ccup->html `[h1 ,(format "Welcome back, ~a!" user)])
            (redirect "/login"))))

 (get "/login"
      (ccup->html
       `[form (@ (method "POST") (action "/login"))
              [input (@ (type "text") (name "username")
                        (placeholder "Username"))]
              [button "Login"]]))

 (post "/login"
       (let ((username (alist-ref "username" (current-params) equal?)))
         (session-set! "username" username)
         (redirect "/")))

 (schematra-install)
 (schematra-start))
EXAMPLE
))

(define ex2 '(#<<EXAMPLE
;; Powerful middleware for cross-cutting concerns
(define (auth-middleware next)
  (let ((token (cdr (assoc 'token (current-params)))))
    (if (and token (valid-token? token))
        (next)  ; Continue to route handler
        '(unauthorized "Invalid token"))))

(define (logging-middleware next)
  (let* ((request (current-request))
         (method (request-method request))
         (path (uri-path (request-uri request))))
    (log-dbg "~A ~A" method path)
    (next)))

(use-middleware! logging-middleware)
(use-middleware! auth-middleware)

;; Now all routes are logged and require auth
(get "/api/users"
     '(ok "{\"users\": [...]}" 
          ((content-type application/json))))
EXAMPLE
))

(define ex3 '(#<<EXAMPLE
;; Chiccup: HTML that looks like your data
(define (render-todo todo)
  `[.todo-item.p-4.border.rounded
    [h3.font-bold ,(todo-title todo)]
    [p.text-gray-600 ,(todo-description todo)]
    [.flex.gap-2.mt-2
     [button.bg-green-500.text-white.px-3.py-1.rounded
      (@ (onclick ,(format "completeTodo(~a)" (todo-id todo))))
      "Complete"]
     [button.bg-red-500.text-white.px-3.py-1.rounded
      (@ (onclick ,(format "deleteTodo(~a)" (todo-id todo))))
      "Delete"]]])

(get "/todos"
     (let ((todos (get-user-todos (session-get "user-id"))))
       (ccup->html
        `[.container.mx-auto.p-6
          [h1.text-2xl.mb-4 "My Todos"]
          ,@(map render-todo todos)])))
EXAMPLE
))

(define ex4 '(#<<EXAMPLE
;; JSON APIs made effortless
(post "/api/users"
      (let* ((params (current-params))
             (name (alist-ref "name" params equal?))
             (email (alist-ref "email" params equal?)))
        (if (and name email (valid-email? email))
            (let ((user-id (create-user! name email)))
              (send-json-response
                'created
                `((id . ,user-id)
                  (message . "User created")
                  (email . ,email))))
            (send-json-response
              'bad-request
              '((error . "Invalid name or email")
                (required . ("name" "email")))))))

(get "/api/users"
     (let ((users (get-all-users)))
       (send-json-response 
         'ok 
         `((users . ,(map user->alist users))
           (count . ,(length users))))))
EXAMPLE
))

(define ex5 '(#<<EXAMPLE
;; Provider configuration
(define (google-provider #!key client-id client-secret)
  `((name . "google")
    (client-id . ,client-id)
    (client-secret . ,client-secret)
    (auth-url . "https://accounts.google.com/o/oauth2/auth")
    (token-url . "https://oauth2.googleapis.com/token")
    (user-info-url . "https://www.googleapis.com/oauth2/v2/userinfo")
    (scopes . "profile email")
    (user-info-parser . parse-google-user)))

;; Install middleware
(use-middleware! (session-middleware "secret-key"))
(use-middleware!
 (oauthtoothy-middleware
  (list (google-provider
         client-id: (get-environment-variable "GOOGLE_CLIENT_ID")
         client-secret: (get-environment-variable "GOOGLE_CLIENT_SECRET")))))

;; Protected route
(get "/profile"
     (let ((auth (current-auth)))
       (if (alist-ref 'authenticated? auth)
           (ccup->html `[h1 ,(string-append "Welcome, " 
                                            (alist-ref 'name auth))])
           (redirect "/auth/google"))))

;; Logout
(get "/logout"
     (session-destroy!)
     (redirect "/"))
EXAMPLE
))

(define ex6 '(#<<EXAMPLE
;; Testing routes without a server - fast and isolated!
(import test schematra schematra.test srfi-13 chicken.format medea)

;; Create isolated test app
(define test-app (schematra/make-app))

;; Define routes in test app - using chiccup responses
(with-schematra-app test-app
  (lambda ()
    ;; You can (import routes) or (include-relative "path/to/routes.scm") here
    ;; Adding some explicitly for educational purposes
    (get "/hello" '(ccup [h1 "Hello, World!"]))

    (get "/users/:id"
         (let ((id (alist-ref "id" (current-params) equal?)))
           `(ccup [div.user
                   [h2 ,(format "User ~a" id)]
                   [p "Profile page"]])))

    (post "/api/echo"
          (let ((name (alist-ref 'name (current-params))))
            (send-json-response `((message . ,(format "Hello ~a" name))))))

    ;; Add middleware that transforms responses (for demonstration)
    (use-middleware!
      (lambda (next)
        (let ((result (next)))
          ;; Middleware can inspect and transform responses
          (if (and (list? result) (eq? (car result) 'ccup))
              ;; Wrap chiccup responses with additional markup
              `(ccup [div.wrapped ,(cadr result)])
              result))))))

;; Run tests with the test egg
(test-group "Schematra Routes"

  (test "GET /hello returns response tuple with chiccup"
    '(ok (ccup [div.wrapped [h1 "Hello, World!"]]) ())
    (test-route test-app 'GET "/hello"))

  (test "GET /users/:id extracts params and wraps with middleware"
    '(ok (ccup [div.wrapped [div.user [h2 "User 123"] [p "Profile page"]]]) ())
    (test-route test-app 'GET "/users/123"))

  (test "Can extract just the chiccup body from response tuple"
    '(ccup [div.wrapped [h1 "Hello, World!"]])
    (test-route-body test-app 'GET "/hello"))

  (test "POST /api/echo returns JSON with correct content"
    '((message . "Hello Alice"))
    (read-json (test-route-body test-app 'POST "/api/echo?name=Alice")))

  (test "404 on unknown route"
    #f
    (test-route test-app 'GET "/unknown")))

;; Run: csi -s test-routes.scm
;; Output:
;; -- testing Schematra Routes --------------------------------------------------
;; GET /hello returns response tuple with chiccup ....................... [ PASS]
;; GET /users/:id extracts params and wraps with middleware ............. [ PASS]
;; Can extract just the chiccup body from response tuple ................ [ PASS]
;; POST /api/echo returns JSON with correct content ..................... [ PASS]
;; 404 on unknown route ................................................. [ PASS]
;; 5 tests completed in 0.001 seconds.
;; 5 out of 5 (100%) tests passed.
;; -- done testing Schematra Routes ---------------------------------------------
;;
;; Benefits:
;; ✓ No HTTP server - tests run in milliseconds
;; ✓ Test against response tuples: (status body headers)
;; ✓ Assert on chiccup structure, not HTML strings
;; ✓ Middleware can inspect and modify response tuples
;; ✓ Complete isolation - each test gets its own app
;; ✓ Verify params, routing, status codes, and response bodies
;; ✓ Can still test rendered HTML with (current-body)
EXAMPLE
))
