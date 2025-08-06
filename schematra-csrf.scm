(module
 schematra-csrf

 (
  csrf-middleware
  csrf-token-key
  csrf-form-field
  csrf-get-token
  ) ;; export list

 (import scheme)
 (import
  chicken.base
  chicken.blob
  openssl.random
  base64
  intarweb
  spiffy ;; current-request
  schematra
  sessions)

 (define csrf-token-key (make-parameter "csrf-token"))
 (define csrf-form-field (make-parameter "_csrf_token"))

 ;; (generate-csrf-token)
 (define (generate-csrf-token #!optional (size 64))
   (base64-encode (blob->string (random-bytes size))))

 (define (csrf-get-token)
   (or (session-get (csrf-token-key))
       (let ((token (generate-csrf-token)))
	 (session-set! (csrf-token-key) token)
	 token)))

 (define (csrf-token-valid? submitted-token)
   (let ((session-token (session-get (csrf-token-key))))
     (and session-token
          submitted-token
          (string=? session-token submitted-token))))

 (define (extract-csrf-from-request request params)
   ;; Check form data first, then headers
   (or (alist-ref (csrf-form-field) params equal?)
       (let ((csrf-header (header-value 'x-csrf-token (request-headers request))))
         csrf-header)))

 (define (csrf-middleware)
   (lambda (params next)
     (let* ((request (current-request))
	    (method (request-method request)))
       (cond
	;; Safe methods don't need CSRF protection
	[(memq method '(GET HEAD OPTIONS TRACE))
	 (next)]
	;; Unsafe methods need valid CSRF token
	[else
	 (let ((submitted-token (extract-csrf-from-request request params)))
           (if (csrf-token-valid? submitted-token)
               (next)
               (halt 'forbidden "CSRF token missing or invalid")))])))))
