(load "schematra.scm")
(import schematra)

(get "/"
     (lambda (request #!optional params)
       "welcome to schematra"))

(schematra-install)
(schematra-start development?: #t)
