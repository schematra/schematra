(module
 schematra-body-parser
 (body-parser-middleware)

 (import scheme)
 (import
  chicken.base
  chicken.string
  intarweb
  spiffy       ;; current-request
  schematra)

 ;; if the body has form-encoded params, add them to the params alist.
 ;; keys will be symbols
 (define (body-parser-middleware)
   (lambda (next)
     (let* ((request (current-request))
	    (headers (request-headers request)))
       (when (and (request-has-message-body? request)
		  (eq? 'application/x-www-form-urlencoded
		       (header-value 'content-type headers)))
	 ;; update params with parsed data from request
	 (current-params (append (read-urlencoded-request-data request) (current-params))))
       (next))))
 )
