;; try to parse deps from the egg file, to install them apart
(import scheme chicken.string)

;; simple filter to avoid srfi-1 dep
(define (filter proc elems)
  (let loop ((output '())
	     (rest elems))
    (if (null? rest)
	output
	(let ((cur (car rest)))
	  (if (proc cur)
	      (loop (cons cur output) (cdr rest))
	      (loop output (cdr rest)))))))

(map (lambda (file)
       (let* ((sexp (with-input-from-file file read))
	      (pair (assoc 'dependencies sexp))
	      (deps (and pair (cdr pair))))
	 (print (string-intersperse
		 (filter (lambda (dep) (not (substring=? "schematra" dep)))
			 (map symbol->string deps))))))
     '("schematra.egg"
       "schematra-session.egg"
       "oauthtoothy.egg"
       "chiccup.egg"))
