(module
 sessions
 (
  ;; procedures
  session-middleware
  session-get session-set! session-delete!
  session-destroy!
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
  chicken.string
  format
  srfi-69
  message-digest
  hmac sha2 base64
  schematra)

 ;;; Parameter that controls how long session cookies remain valid in the client's browser.
 ;;;
 ;;; The value is specified in seconds and determines the 'max-age' attribute of the
 ;;; Set-Cookie header when sessions are saved. When a session cookie expires, the browser
 ;;; will automatically delete it and subsequent requests will start with a fresh, empty session.
 ;;;
 ;;; ### Parameters
 ;;;   - `seconds`: integer - Cookie lifetime in seconds (default: 86400 for 24 hours)
 ;;;
 ;;; ### Common Values
 ;;;   - 3600: 1 hour
 ;;;   - 86400: 1 day (default)
 ;;;   - 604800: 1 week
 ;;;   - 2592000: 30 days
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Set sessions to expire after 2 hours
 ;;; (session-max-age (* 2 60 60))
 ;;;
 ;;; ;; Set sessions to expire after 1 week
 ;;; (session-max-age (* 7 24 60 60))
 ;;; ```
 (define session-max-age (make-parameter (* 24 60 60)))

 ;;; Parameter that defines the cookie name used to store session data.
 ;;;
 ;;; This parameter defines the cookie name that will appear in the browser's cookie
 ;;; storage and in HTTP Cookie/Set-Cookie headers. The session middleware uses this
 ;;; name to identify which cookie contains the serialized session data.
 ;;;
 ;;; ### Parameters
 ;;;   - `name`: string - Cookie name (default: "schematra.session_id")
 ;;;
 ;;; ### Cookie Naming Guidelines
 ;;; The cookie name should:
 ;;;   - Be unique to avoid conflicts with other applications
 ;;;   - Follow HTTP cookie naming conventions (alphanumeric, dots, underscores)
 ;;;   - Be descriptive enough to identify its purpose
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Use a custom session cookie name
 ;;; (session-key "myapp_session")
 ;;;
 ;;; ;; Use environment-specific names
 ;;; (session-key "myapp_dev_session")  ; for development
 ;;; (session-key "myapp_prod_session") ; for production
 ;;; ```
 (define session-key (make-parameter "schematra.session_id"))

 ;;; Internal symbol used to track session modifications.
 ;;;
 ;;; This symbol is used internally by the session middleware to determine whether
 ;;; a session has been modified during request processing. When `session-set!` or
 ;;; `session-delete!` is called, this key is automatically added to the session
 ;;; hash table to mark it as "dirty".
 ;;;
 ;;; The middleware checks for the presence of this key after request processing
 ;;; to decide whether to save the session back to a cookie. This optimization
 ;;; prevents unnecessary cookie updates when sessions are only read from.
 ;;;
 ;;; ### Value
 ;;;   `'__dirty` (symbol)
 ;;;
 ;;; ### Usage Notes
 ;;;   - This is an internal implementation detail and should not be used directly in application code
 ;;;   - The `session-get`, `session-set!`, and `session-delete!` functions handle dirty tracking automatically
 ;;;   - This key is automatically removed from the session data before serialization, so it never appears in the actual cookie value
 (define session-dirty-key '__dirty)

 ;; this is the placeholder for the hash-table that will hold the
 ;; session data through a request.
 (define session (make-parameter #f))
 
 ;;; Creates session middleware for managing HTTP sessions.
 ;;;
 ;;; This function creates middleware that provides session management capabilities
 ;;; for web applications. Sessions are stored in HTTP cookies and automatically
 ;;; serialized/deserialized on each request. The middleware handles session
 ;;; creation, loading, and persistence transparently.
 ;;;
 ;;; ### Parameters
 ;;;   - `secret-key`: string - Secret key used for session serialization/security.
 ;;;                   This key should be kept secret and consistent across server restarts
 ;;;                   to maintain session continuity. Use a strong, random string.
 ;;;
 ;;; ### Returns
 ;;;   A middleware function that can be used with `use-middleware!`
 ;;;
 ;;; ### Session Lifecycle
 ;;;   1. On incoming requests, checks for existing session cookie
 ;;;   2. If cookie exists, deserializes session data into hash table
 ;;;   3. If no cookie, creates new empty session hash table
 ;;;   4. Makes session data available via `session-get`/`session-set!`/`session-delete!`
 ;;;   5. After request processing, saves modified sessions back to cookie
 ;;;   6. Only saves cookie if session was modified (marked with dirty flag)
 ;;;
 ;;; ### Cookie Configuration
 ;;;   - Cookie name: controlled by `(session-key)` parameter (default: "schematra.session_id")
 ;;;   - Max age: controlled by `(session-max-age)` parameter (default: 24 hours)
 ;;;   - HTTP-only: true (prevents JavaScript access for security)
 ;;;   - Secure: not set (can be enhanced for HTTPS-only environments)
 ;;;
 ;;; ### Security Considerations
 ;;;   - Session data is serialized as Scheme s-expressions in the cookie
 ;;;   - The secret-key parameter is currently used for identification but not encryption
 ;;;   - Sessions are stored client-side, so avoid storing sensitive data
 ;;;   - Consider implementing proper encryption/signing for production use
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Install session middleware with a secret key
 ;;; (use-middleware! (session-middleware "my-secret-key-12345"))
 ;;;
 ;;; ;; In route handlers, use session functions:
 ;;; (get ("/login")
 ;;;      (session-set! "user-id" "12345")
 ;;;      (session-set! "username" "alice")
 ;;;      "Logged in successfully")
 ;;;
 ;;; (get ("/profile")
 ;;;      (let ((user-id (session-get "user-id")))
 ;;;        (if user-id
 ;;;            (format "Welcome user ~A" user-id)
 ;;;            "Please log in")))
 ;;; ```
 (define (session-middleware secret-key)
   (lambda (next)
     (let* ((session-cookie (cookie-ref (session-key)))
	    (session-data (if session-cookie
			      (deserialize-session session-cookie secret-key)
			      (make-hash-table))))
       (parameterize ((session session-data))
	 (dynamic-wind
	     (lambda () #f)     ;; before thunk - do nothing
	     (lambda () (next)) ;; main thun - continue the middleware stack
	     (lambda ()         ;; after thunk - always run
	       (if (hash-table-exists? (session) session-dirty-key)
		   (cookie-set! (session-key)
				(serialize-session (session) secret-key)
				http-only: #t
				max-age: (session-max-age)))))))))

 (define (serialize-session session-hash secret-key)
   ;; don't serialize the modified key
   (hash-table-delete! session-hash session-dirty-key)
   ;; we need to convert the string from the alist to signature
   ;; . base64(alist)
   (let* ((alist        (hash-table->alist session-hash))
	  (alist-str    (with-output-to-string
			  (lambda () (write alist))))
	  (alist-base64 (base64-encode alist-str))
	  (prim         (hmac-primitive secret-key (sha256-primitive)))
	  (signature    (message-digest-string prim alist-base64)))
     (string-append signature "." alist-base64)))

 (define (deserialize-session cookie-value secret-key)
   (condition-case
    (let* ((cookie       (string-split cookie-value "."))
	   (signature    (car cookie))
	   (alist-base64 (cadr cookie))
	   (prim         (hmac-primitive secret-key (sha256-primitive)))
	   (valid-sign?  (string=? signature (message-digest-string prim alist-base64))))
      (if valid-sign?
	  (alist->hash-table (with-input-from-string (base64-decode alist-base64) read))
	  (make-hash-table)))
    (e (_exn) (begin
		(log-err "Error deserializing cookie: ~A" cookie-value)
		(make-hash-table)))))

 ;;; Retrieve a value from the current session.
 ;;;
 ;;; Gets a value from the session hash table using the specified key.
 ;;; If the key doesn't exist, returns the default value.
 ;;;
 ;;; ### Parameters
 ;;;   - `key`: string - The session key to look up
 ;;;   - `default`: any - Value to return if key is not found (default: #f)
 ;;;
 ;;; ### Returns
 ;;;   The value associated with the key, or the default value if key not found
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Get user ID from session
 ;;; (let ((user-id (session-get "user-id")))
 ;;;   (if user-id
 ;;;       (format "Welcome user ~A" user-id)
 ;;;       "Please log in"))
 ;;;
 ;;; ;; Get with custom default
 ;;; (session-get "theme" "light")
 ;;; ```
 (define (session-get key #!optional default)
   (hash-table-ref/default (session) key default))

 ;;; Store a value in the current session.
 ;;;
 ;;; Sets a key-value pair in the session hash table and marks the session
 ;;; as modified so it will be saved back to the cookie after request processing.
 ;;;
 ;;; ### Parameters
 ;;;   - `key`: string - The session key to set
 ;;;   - `value`: any - The value to store (must be serializable as Scheme data)
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Store user information
 ;;; (session-set! "user-id" "12345")
 ;;; (session-set! "username" "alice")
 ;;; (session-set! "preferences" '((theme . "dark") (lang . "en")))
 ;;; ```
 (define (session-set! key value)
   (hash-table-set! (session) key value)
   (hash-table-set! (session) session-dirty-key #t))

 ;;; Remove a key-value pair from the current session.
 ;;;
 ;;; Deletes the specified key from the session hash table and marks the session
 ;;; as modified so the change will be saved back to the cookie.
 ;;;
 ;;; ### Parameters
 ;;;   - `key`: string - The session key to remove
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Remove user data on logout
 ;;; (session-delete! "user-id")
 ;;; (session-delete! "username")
 ;;; ```
 (define (session-delete! key)
   (hash-table-delete! (session) key)
   (hash-table-set! (session) session-dirty-key #t))

 ;;; Clear all data from the current session.
 ;;;
 ;;; Replaces the current session hash table with a new empty one, effectively
 ;;; clearing all session data. The session is marked as modified so the empty
 ;;; session will be saved back to the cookie.
 ;;;
 ;;; This is useful for logout functionality where you want to completely clear
 ;;; all user session data.
 ;;;
 ;;; ### Examples
 ;;; ```scheme
 ;;; ;; Complete logout - clear all session data
 ;;; (post ("/logout")
 ;;;       (session-destroy!)
 ;;;       (redirect "/"))
 ;;; ```
 (define (session-destroy!)
   (session (make-hash-table))
   (hash-table-set! (session) session-dirty-key #t)))
