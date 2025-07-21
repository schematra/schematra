(load "schematra.scm")
(import schematra format)

(get "/"
     (lambda (request #!optional params)
       "welcome to schematra"))

(get "/users/:user-id/posts/:post-id"
     (lambda (req params)
       (let ((user-id (alist-ref "user-id" params equal?))
             (post-id (alist-ref "post-id" params equal?)))
         (format "User: ~A, Post: ~A\n" user-id post-id))))


(schematra-install)
(schematra-start development?: #t)
