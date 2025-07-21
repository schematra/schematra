(load "schematra.scm")
(import schematra format)

(get "/"
     (lambda (request #!optional params)
       "welcome to schematra"))

(define (lookup key alist)
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
        #f)))

(get "/users/:user-id/posts/:post-id"
     (lambda (req params)
       (let ((user-id (lookup "user-id" params))
             (post-id (lookup "post-id" params))
	     (q       (lookup 'kk params)))
	 (log-dbg "[DBG] params: ~A" params)
         (format "User: ~A, Post: ~A, q: ~A\n" user-id post-id q))))

(schematra-install)
(schematra-start development?: #t)
