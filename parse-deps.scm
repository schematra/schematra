;; try to parse deps from the egg file, to install them apart
(import scheme chicken.string)
(let* ((sexp (with-input-from-file "/schematra/schematra.egg" read))
       (pair (assoc 'dependencies sexp))
       (deps (and pair (cdr pair))))
  (print (string-intersperse
          (map (lambda (x) (if (symbol? x) (symbol->string x) x)) deps)
          " ")))
