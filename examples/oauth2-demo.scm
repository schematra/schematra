(import
 scheme
 schematra
 schematra-session
 chiccup
 oauthtoothy
 chicken.process-context
 srfi-69)

;; simple in-memory storage for demo purposes
(define user-store (make-hash-table))
(define (save-user provider-id user-data)
  (hash-table-set! user-store provider-id user-data))
(define (load-user provider-id)
  (hash-table-ref/default user-store provider-id #f))

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

(with-schematra-app
 (schematra/make-app)

 ;; oauthtoothy requires sessions
 (use-middleware! (session-middleware "my secret key"))
 (use-middleware!
  (oauthtoothy-middleware
   (list (google-provider
	  client-id: (get-environment-variable "GOOGLE_CLIENT_ID")
	  client-secret: (get-environment-variable "GOOGLE_CLIENT_SECRET")))
   success-redirect: "/profile"
   save-proc: (lambda (user-id _ user-info-alist _) (user-save user-id user-info-alist))
   load-proc: (lambda (user-id provider-name) (load-user user-id))))

 (get "/profile"
      (let ((auth (current-auth)))
	(if (alist-ref 'authenticated? auth)
	    (begin
	      (ccup->html `[h1 ,(string-append "welcome " (alist-ref 'name auth))]))
	    ;; trigger the auth sequence
	    (redirect "/auth/google"))))

 (get "/logout"
      (session-destroy!)
      (redirect "/"))

 (schematra-install)
 (schematra-start development?: #f))
