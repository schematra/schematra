#!/usr/bin/env csi -s
;; Schematra Test Suite

(import scheme)
(import
 chicken.base
 chicken.string
 chicken.format
 chicken.process-context
 spiffy
 (rename intarweb (headers intarweb:headers))
 uri-common
 srfi-69
 medea
 schematra
 schematra.test
 schematra.body-parser
 schematra-session
 test)

;; ============================================================
;; 1. Basic Routing
;; ============================================================
(test-group "Basic routing"
  (let ((app (with-test-app a
               (get "/" "root")
               (get "/hello" "Hello World")
               (post "/submit" "submitted")
               (put "/update" "updated")
               (delete "/remove" "removed"))))
    (test "GET /" 'ok (test-route-status app 'GET "/"))
    (test "GET /hello body" "Hello World" (test-route-body app 'GET "/hello"))
    (test "POST /submit" 'ok (test-route-status app 'POST "/submit"))
    (test "PUT /update" 'ok (test-route-status app 'PUT "/update"))
    (test "DELETE /remove" 'ok (test-route-status app 'DELETE "/remove"))
    (test "GET unknown returns #f" #f (test-route app 'GET "/nonexistent"))
    (test "Wrong method returns #f" #f (test-route app 'POST "/hello"))))

;; ============================================================
;; 2. Route Parameters
;; ============================================================
(test-group "Route parameters"
  (let ((app (with-test-app a
               (get "/users/:id"
                    (let ((id (alist-ref "id" (current-params) equal?)))
                      (string-append "user:" id)))
               (get "/users/:uid/posts/:pid"
                    (let* ((params (current-params))
                           (uid (alist-ref "uid" params equal?))
                           (pid (alist-ref "pid" params equal?)))
                      (string-append uid ":" pid))))))
    (test "Single param" "user:42" (test-route-body app 'GET "/users/42"))
    (test "Multiple params" "alice:7" (test-route-body app 'GET "/users/alice/posts/7"))))

;; ============================================================
;; 3. Query Parameters
;; ============================================================
(test-group "Query parameters"
  (let ((app (with-test-app a
               (get "/search"
                    (let ((q (alist-ref 'q (current-params))))
                      (string-append "q=" (or q "none")))))))
    (test "Query param" "q=hello" (test-route-body app 'GET "/search?q=hello"))))

;; ============================================================
;; 4. Response Types
;; ============================================================
(test-group "Response types"
  (let ((app (with-test-app a
               (get "/string" "plain string")
               (get "/tuple" '(created "done" ()))
               (get "/with-headers"
                    '(ok "data" ((x-custom "yes")))))))
    (test "String response status" 'ok (test-route-status app 'GET "/string"))
    (test "String response body" "plain string" (test-route-body app 'GET "/string"))
    (test "Tuple status" 'created (test-route-status app 'GET "/tuple"))
    (test "Tuple body" "done" (test-route-body app 'GET "/tuple"))
    (test "Tuple with headers status" 'ok (test-route-status app 'GET "/with-headers"))))

;; ============================================================
;; 5. Halt
;; ============================================================
(test-group "Halt"
  (let ((app (with-test-app a
               (get "/halt-simple" (halt 'not-found "nope"))
               (get "/halt-headers"
                    (halt 'bad-request "bad" '((x-err "yes")))))))
    (test "Halt status" 'not-found (test-route-status app 'GET "/halt-simple"))
    (test "Halt body" "nope" (test-route-body app 'GET "/halt-simple"))
    (test "Halt with headers status" 'bad-request
          (test-route-status app 'GET "/halt-headers"))))

;; ============================================================
;; 6. Redirect
;; ============================================================
(test-group "Redirect"
  (let ((app (with-test-app a
               (get "/go" (redirect "/target"))
               (get "/go-permanent" (redirect "/target" 'moved-permanently)))))
    (test "Redirect status" 'found (test-route-status app 'GET "/go"))
    (test "Redirect location" "/target"
          (test-route-redirect-location app 'GET "/go"))
    (test "Redirect custom status" 'moved-permanently
          (test-route-status app 'GET "/go-permanent"))))

;; ============================================================
;; 7. Cookies
;; ============================================================
(test-group "Cookies"
  (let ((app (with-test-app a
               (get "/set-cookie"
                    (cookie-set! "flavor" "chocolate")
                    "done")
               (get "/read-cookie"
                    (let ((val (cookie-ref "flavor")))
                      (string-append "flavor=" (or val "none")))))))
    (test "Set cookie captured" "chocolate"
          (response-cookie-value
           (test-route-full app 'GET "/set-cookie")
           "flavor"))
    (test "Read cookie from request header" "flavor=chocolate"
          (test-route-body app 'GET "/read-cookie"
                           cookies: '(("flavor" . "chocolate"))))))

;; ============================================================
;; 8. Session Middleware
;; ============================================================
(test-group "Session middleware"
  (let* ((secret "test-secret-key")
         (app (with-test-app a
                (use-middleware! (session-middleware secret))
                (get "/session-set"
                     (session-set! "user" "alice")
                     "set")
                (get "/session-get"
                     (let ((user (session-get "user")))
                       (string-append "user=" (or user "none")))))))
    (let* ((full (test-route-full app 'GET "/session-set"))
           (session-cookie-name (session-key))
           (cookie-val (response-cookie-value full session-cookie-name)))
      (test "Session set returns ok" 'ok (car full))
      (test "Session cookie is set" #t (string? cookie-val))
      (when (string? cookie-val)
        (test "Session get reads value" "user=alice"
              (test-route-body app 'GET "/session-get"
                               cookies: (list (cons session-cookie-name cookie-val))))))))

;; ============================================================
;; 9. Middleware Pipeline
;; ============================================================
(test-group "Middleware pipeline"
  (let ((app (with-test-app a
               (use-middleware!
                (lambda (next)
                  (let ((result (next)))
                    (if (string? result)
                        (string-append "[" result "]")
                        result))))
               (get "/wrapped" "inner"))))
    (test "Middleware wraps response" "[inner]"
          (test-route-body app 'GET "/wrapped")))

  ;; Short-circuit via halt
  (let ((app (with-test-app a
               (use-middleware!
                (lambda (next)
                  (halt 'forbidden "blocked")))
               (get "/secret" "should not reach"))))
    (test "Middleware halt status" 'forbidden
          (test-route-status app 'GET "/secret"))))

;; ============================================================
;; 10. JSON Responses
;; ============================================================
(test-group "JSON responses"
  (let ((app (with-test-app a
               (get "/json"
                    (send-json-response '((name . "alice")))))))
    (test "JSON status" 'ok (test-route-status app 'GET "/json"))
    (let ((body (test-route-body app 'GET "/json")))
      (test "JSON body parses" "alice"
            (alist-ref 'name (read-json body))))))

;; ============================================================
;; 11. Body Parser
;; ============================================================
(test-group "Body parser"
  (let ((app (with-test-app a
               (use-middleware! (body-parser-middleware))
               (post "/form"
                     (let ((name (alist-ref 'name (current-params))))
                       (string-append "name=" (or name "none"))))
               (post "/raw"
                     (or (current-raw-body) "no-body"))
               (post "/webhook"
                     (or (current-raw-body) "no-body")))))
    (test "Form body parsed" "name=Alice"
          (test-route-body app 'POST "/form"
                           headers: '((content-type application/x-www-form-urlencoded))
                           body: "name=Alice"))
    (test "Raw body available after form parse" "name=Alice"
          (test-route-body app 'POST "/raw"
                           headers: '((content-type application/x-www-form-urlencoded))
                           body: "name=Alice"))
    (test "Raw body available for non-form content type" "{\"event\":\"push\"}"
          (test-route-body app 'POST "/webhook"
                           headers: '((content-type application/json))
                           body: "{\"event\":\"push\"}"))))

;; ============================================================
;; Results
;; ============================================================
(test-exit)
