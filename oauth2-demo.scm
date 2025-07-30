(import
 schematra
 chiccup
 sessions
 oauthtoothy
 chicken.process-context
 srfi-69)

;; simple in-memory storage for demo purposes
(define user-store (make-hash-table))
(define (save-user user-id user-data)
  (hash-table-set! user-store user-id user-data))
(define (load-user user-id)
  (hash-table-ref/default user-store user-id #f))

;; should map the provider-specific data into something the rest of
;; oauthtoothy understands
(define (parse-google-user json-response)
  `((id . ,(alist-ref 'id json-response))
    (name . ,(alist-ref 'name json-response))
    (email . ,(alist-ref 'email json-response))))

(define (google-provider #!key client-id client-secret)
  `((name . "google")
    (client-id . ,client-id)
    (client-secret . ,client-secret)
    (auth-url . "https://accounts.google.com/o/oauth2/auth")
    (token-url . "https://oauth2.googleapis.com/token" ;;"http://localhost:5500"
	       )
    (user-info-url . "https://www.googleapis.com/oauth2/v2/userinfo")
    (scopes . "profile email")
    (user-info-parser . ,parse-google-user)))

;; oauthtoothy requires sessions
(use-middleware! (session-middleware "my secret key"))
(use-middleware!
 (oauthtoothy-middleware
  (list (google-provider
	 client-id: (get-environment-variable "GOOGLE_CLIENT_ID")
	 client-secret: (get-environment-variable "GOOGLE_CLIENT_SECRET")))
  save-proc: save-user
  load-proc: load-user))

(get "/profile"
     (lambda (request params)
       (let ((auth (current-auth)))
	 (if (alist-ref 'authenticated? auth)
	     (begin
	       (ccup/html `[h1 ,(string-append "welcome " (alist-ref 'name auth))]))
	     ;; trigger the auth sequence
	     (redirect "/auth/google")))))

(get "/logout"
     (lambda (request params)
       (session-destroy!)
       (redirect "/")))

(schematra-install)
(schematra-start development?: #t)
