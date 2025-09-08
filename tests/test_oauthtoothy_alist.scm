;; Test for oauthtoothy alist construction bug fix using SRFI-64
;; This tests the actual oauthtoothy module to ensure the fix works

(import scheme chicken.base test)
(import oauthtoothy schematra-session)

;; Mock session functions since we need them for oauthtoothy
(define session-data (make-parameter '()))

(define (session-get key)
  (alist-ref key (session-data) equal?))

(define (session-set! key value)
  (session-data (cons (cons key value) (session-data))))

;; Helper to check if alist is well-formed (proper list of pairs)
(define (alist-wellformed? alist)
  (define (check-pairs lst)
    (cond 
     ((null? lst) #t)
     ((not (pair? lst)) #f)  ;; Found improper list termination
     ((not (pair? (car lst))) #f)  ;; Car is not a pair
     (else (check-pairs (cdr lst)))))
  (check-pairs alist))

;; Mock user load procedure that returns #f (triggering the bug)
(define (mock-load-proc-returns-false user-id provider-name)
  #f)

;; Mock user load procedure that returns user data
(define (mock-load-proc-returns-data user-id provider-name)
  '((name . "John Doe") (email . "john@example.com")))

;; Mock user load procedure that returns provider-specific data
(define (mock-load-proc-provider-specific user-id provider-name)
  (cond 
    ((equal? provider-name "google")
     '((name . "John from Google") (email . "john@gmail.com") (provider . "google")))
    ((equal? provider-name "github")  
     '((name . "John from GitHub") (login . "johndev") (provider . "github")))
    (else #f)))

;; Mock save procedures to capture parameters
(define saved-params (make-parameter #f))

(define (mock-save-proc user-id provider-name user-data token)
  (saved-params (list user-id provider-name user-data token)))

(define (test-equal name expected expr)
  (test-assert (equal? expected expr)))

(test-begin "oauthtoothy middleware")

;; Test case 1: user-id exists but load-proc returns #f (bug trigger case)
(test-group "user-id exists but load-proc returns #f"
	    ;; Set up session with user-id
	    (session-data '())
	    (session-set! "user-id" "user123")
  
	    ;; Create middleware with load-proc that returns #f
	    (let ((middleware (oauthtoothy-middleware 
			       '() ; no providers needed for this test
			       load-proc: mock-load-proc-returns-false)))
    
	      ;; Mock next function to capture current-auth
	      (let ((captured-auth #f))
		(define (mock-next)
		  (set! captured-auth (current-auth)))
      
		;; Run the middleware
		(middleware mock-next)
      
		;; Test the captured auth alist
		(test-assert "current-auth is well-formed alist"
			     (alist-wellformed? captured-auth))
      
		(test-assert "alist-ref works for authenticated?"
			     (eq? #t (alist-ref 'authenticated? captured-auth)))
      
		(test-equal "alist-ref works for user-id"
			    "user123" (alist-ref 'user-id captured-auth)))))

;; Test case 2: user-id exists and load-proc returns data
(test-group "user-id exists and load-proc returns data"
  ;; Set up session with user-id
  (session-data '())
  (session-set! "user-id" "user456")
  
  ;; Create middleware with load-proc that returns data
  (let ((middleware (oauthtoothy-middleware 
                     '() ; no providers needed for this test
                     load-proc: mock-load-proc-returns-data)))
    
    ;; Mock next function to capture current-auth
    (let ((captured-auth #f))
      (define (mock-next)
        (set! captured-auth (current-auth)))
      
      ;; Run the middleware
      (middleware mock-next)
      
      ;; Test the captured auth alist
      (test-assert "current-auth is well-formed alist"
                   (alist-wellformed? captured-auth))
      
      (test-assert "alist-ref works for authenticated?"
                   (eq? #t (alist-ref 'authenticated? captured-auth)))
      
      (test-equal "alist-ref works for user-id"
                  "user456" (alist-ref 'user-id captured-auth))
      
      (test-equal "alist-ref works for name from user data"
                  "John Doe" (alist-ref 'name captured-auth))
      
      (test-equal "alist-ref works for email from user data"
                  "john@example.com" (alist-ref 'email captured-auth)))))

;; Test case 3: no user-id in session (unauthenticated)
(test-group "no user-id in session"
  ;; Clear session
  (session-data '())
  
  ;; Create middleware
  (let ((middleware (oauthtoothy-middleware 
                     '() ; no providers needed for this test
                     load-proc: mock-load-proc-returns-data)))
    
    ;; Mock next function to capture current-auth
    (let ((captured-auth #f))
      (define (mock-next)
        (set! captured-auth (current-auth)))
      
      ;; Run the middleware
      (middleware mock-next)
      
      ;; Test the captured auth alist
      (test-assert "current-auth is well-formed alist"
                   (alist-wellformed? captured-auth))
      
      (test-assert "alist-ref works for authenticated?"
                   (eq? #f (alist-ref 'authenticated? captured-auth))))))

;; Test case 4: provider-specific user data loading
(test-group "provider-specific user data loading"
  ;; Set up session with user-id and provider-name
  (session-data '())
  (session-set! "user-id" "user789")
  (session-set! "oauth-provider-name" "google")
  
  ;; Create middleware with provider-aware load-proc
  (let ((middleware (oauthtoothy-middleware 
                     '() ; no providers needed for this test
                     load-proc: mock-load-proc-provider-specific)))
    
    ;; Mock next function to capture current-auth
    (let ((captured-auth #f))
      (define (mock-next)
        (set! captured-auth (current-auth)))
      
      ;; Run the middleware
      (middleware mock-next)
      
      ;; Test that provider-specific data was loaded
      (test-assert "current-auth is well-formed alist"
                   (alist-wellformed? captured-auth))
      
      (test-equal "loads google-specific user data"
                  "John from Google" (alist-ref 'name captured-auth))
      
      (test-equal "loads google-specific email"
                  "john@gmail.com" (alist-ref 'email captured-auth))
      
      (test-equal "includes provider information"
                  "google" (alist-ref 'provider captured-auth)))))

;; Test case 5: different provider loads different data
(test-group "different provider loads different data"
  ;; Set up session with GitHub provider
  (session-data '())
  (session-set! "user-id" "user789")
  (session-set! "oauth-provider-name" "github")
  
  ;; Create middleware with provider-aware load-proc
  (let ((middleware (oauthtoothy-middleware 
                     '() ; no providers needed for this test
                     load-proc: mock-load-proc-provider-specific)))
    
    ;; Mock next function to capture current-auth
    (let ((captured-auth #f))
      (define (mock-next)
        (set! captured-auth (current-auth)))
      
      ;; Run the middleware
      (middleware mock-next)
      
      ;; Test that GitHub-specific data was loaded
      (test-equal "loads github-specific user data"
                  "John from GitHub" (alist-ref 'name captured-auth))
      
      (test-equal "loads github-specific login"
                  "johndev" (alist-ref 'login captured-auth))
      
      (test-equal "includes provider information"
                  "github" (alist-ref 'provider captured-auth)))))

;; Test case 6: unknown provider returns no additional data
(test-group "unknown provider handling"
  ;; Set up session with unknown provider
  (session-data '())
  (session-set! "user-id" "user999")
  (session-set! "oauth-provider-name" "unknown")
  
  ;; Create middleware with provider-aware load-proc
  (let ((middleware (oauthtoothy-middleware 
                     '() ; no providers needed for this test
                     load-proc: mock-load-proc-provider-specific)))
    
    ;; Mock next function to capture current-auth
    (let ((captured-auth #f))
      (define (mock-next)
        (set! captured-auth (current-auth)))
      
      ;; Run the middleware
      (middleware mock-next)
      
      ;; Test that only basic auth info is present
      (test-assert "current-auth is well-formed alist"
                   (alist-wellformed? captured-auth))
      
      (test-assert "user is still authenticated"
                   (eq? #t (alist-ref 'authenticated? captured-auth)))
      
      (test-equal "user-id is present"
                  "user999" (alist-ref 'user-id captured-auth))
      
      ;; No additional user data should be present
      (test-assert "no name from unknown provider"
                   (not (alist-ref 'name captured-auth)))
      
      (test-assert "no email from unknown provider"
                   (not (alist-ref 'email captured-auth))))))

(test-end "oauthtoothy middleware")
