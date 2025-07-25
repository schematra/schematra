(module
 sessions
 (
  ;; procedures
  session-middleware
  session-get session-set! session-delete!
  ;; parameters
  session-max-age
  session-key
  session-dirty-key
 );; end export list

 (import scheme)
 (import
  chicken.base
  chicken.condition
  chicken.port
  format
  srfi-69
  schematra)

 (define session-max-age (make-parameter (* 24 60 60)))
 (define session-key (make-parameter "schematra.session_id"))

 (define session (make-parameter #f))
 (define session-dirty-key '__dirty)

 ;; Create session middleware for managing HTTP sessions
 ;;
 ;; This function creates middleware that provides session management capabilities
 ;; for web applications. Sessions are stored in HTTP cookies and automatically
 ;; serialized/deserialized on each request. The middleware handles session
 ;; creation, loading, and persistence transparently.
 ;;
 ;; Parameters:
 ;;   secret-key: string - Secret key used for session serialization/security
 ;;               This key should be kept secret and consistent across server restarts
 ;;               to maintain session continuity. Use a strong, random string.
 ;;
 ;; Returns:
 ;;   A middleware function that can be used with use-middleware!
 ;;
 ;; Session Lifecycle:
 ;;   1. On incoming requests, checks for existing session cookie
 ;;   2. If cookie exists, deserializes session data into hash table
 ;;   3. If no cookie, creates new empty session hash table
 ;;   4. Makes session data available via session-get/session-set!/session-delete!
 ;;   5. After request processing, saves modified sessions back to cookie
 ;;   6. Only saves cookie if session was modified (marked with dirty flag)
 ;;
 ;; Cookie Configuration:
 ;;   - Cookie name: controlled by (session-key) parameter (default: "schematra.session_id")
 ;;   - Max age: controlled by (session-max-age) parameter (default: 24 hours)
 ;;   - HTTP-only: true (prevents JavaScript access for security)
 ;;   - Secure: not set (can be enhanced for HTTPS-only environments)
 ;;
 ;; Security Considerations:
 ;;   - Session data is serialized as Scheme s-expressions in the cookie
 ;;   - The secret-key parameter is currently used for identification but not encryption
 ;;   - Sessions are stored client-side, so avoid storing sensitive data
 ;;   - Consider implementing proper encryption/signing for production use
 ;;
 ;; Example usage:
 ;;   ;; Install session middleware with a secret key
 ;;   (use-middleware! (session-middleware "my-secret-key-12345"))
 ;;
 ;;   ;; In route handlers, use session functions:
 ;;   (get "/login" 
 ;;        (lambda (req params)
 ;;          (session-set! "user-id" "12345")
 ;;          (session-set! "username" "alice")
 ;;          "Logged in successfully"))
 ;;
 ;;   (get "/profile"
 ;;        (lambda (req params)
 ;;          (let ((user-id (session-get "user-id")))
 ;;            (if user-id
 ;;                (format "Welcome user ~A" user-id)
 ;;                "Please log in"))))
 (define (session-middleware secret-key)
   (lambda (req params next)
     (let* ((session-cookie (cookie-ref (session-key)))
	    (session-data (if session-cookie
			      (deserialize-session session-cookie secret-key)
			      (make-hash-table))))
       (parameterize ((session session-data))
	 (let ((response (next)))
	   ;; save session back to cookie if needed
	   (if (hash-table-exists? session-data session-dirty-key)
	       (cookie-set! (session-key)
			    (serialize-session session-data secret-key)
			    http-only: #t
			    max-age: (* 24 60 60)))
	   response)))))

 (define (serialize-session session-hash secret-key)
   ;; don't serialize the modified key
   (hash-table-delete! session-hash session-dirty-key)
   (let ((alist (hash-table->alist session-hash)))
     (with-output-to-string
       (lambda () (write alist)))))

 (define (deserialize-session cookie-value secret-key)
   (condition-case
    (let ((alist (with-input-from-string cookie-value read)))
      (alist->hash-table alist))
    (e (_exn) (make-hash-table))))

 ;; helpers to get/set/delete values in the session
 (define (session-get key #!optional default)
   (hash-table-ref/default (session) key default))

 (define (session-set! key value)
   (hash-table-set! (session) key value)
   (hash-table-set! (session) session-dirty-key #t))

 (define (session-delete! key)
   (hash-table-delete! (session) key)
   (hash-table-set! (session) session-dirty-key #t)))
