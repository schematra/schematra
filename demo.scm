(load "schematra.scm")
(load "chiccup.scm")
(import schematra chiccup format)

(define welcome-page
  (ccup/html `[h1 "hello"]))

(get "/"
     (lambda (request #!optional params)
       (ccup/html `[h1#hero "hey, welcome"])))

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
