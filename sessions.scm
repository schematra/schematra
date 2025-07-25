(module
 sessions
 (
  ;; procedures
  session-middleware
  session-get session-set! session-delete!
 );; end export list

 (import scheme)
 (import
  chicken.base
  chicken.condition
  chicken.port
  format
  srfi-69
  schematra)

 (define session (make-parameter #f))
 (define dirty_key '__dirty)

 (define (session-middleware secret-key)
   (lambda (req params next)
     (let* ((session-cookie (cookie-ref (schematra-session-cookie-key)))
	    (session-data (if session-cookie
			      (deserialize-session session-cookie secret-key)
			      (make-hash-table))))
       (parameterize ((session session-data))
	 (let ((response (next)))
	   ;; save session back to cookie if needed
	   (if (hash-table-exists? session-data dirty_key)
	       (cookie-set! (schematra-session-cookie-key)
			    (serialize-session session-data secret-key)
			    http-only: #t
			    max-age: (* 24 60 60)))
	   response)))))

 (define (serialize-session session-hash secret-key)
   ;; don't serialize the modified key
   (hash-table-delete! session-hash dirty_key)
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
   (hash-table-set! (session) dirty_key #t))

 (define (session-delete! key)
   (hash-table-delete! (session) key)
   (hash-table-set! (session) dirty_key #t)))
