(import
 chicken.io
 chicken.string
 chicken.port
 chicken.process
 chicken.pretty-print
 lowdown ;; to parse markdown in comments -> ccup
 format
 srfi-13 srfi-1
 sxml-transforms)

;; returns a chiccup header
(define (make-markdown-header doc-elt)
  (display (conc ">> " doc-elt "; " (list? doc-elt) "\n") ##sys#standard-error)
  (cond
   ((pair? doc-elt)
    (let* ((type (car doc-elt))
	   (symbol (cond ((eq? type 'parameter) (cdr doc-elt))
			 ((eq? type 'procedure) (cadr doc-elt))
			 (else "don't know this one"))))
      '[h2 (string-append (symbol->string symbol) " (" (symbol->string type) ")")]))
   ((symbol? doc-elt)
    '[h2 (symbol->string doc-elt)])
   (else
    '[h2 (format "~a" doc-elt)])))

(define (read-docs file)
  ;; not the best approach, but works for now
  (let ((cmd (conc "cat " file ".scm |"
		   " ~/.cache/chicken-install/schematic/schematic-extract -c ';;;'")))
    (with-input-from-pipe cmd
			  (lambda ()
			    (let loop ((acc '()))
			      (let ((last-read (read)))
				(if (eof-object? last-read)
				    (reverse acc)
				    (loop (cons last-read acc)))))))))

(define files '("schematra" "chiccup" "schematra-csrf" "sessions" "oauthtoothy"))

;; join adjacent string content
;; (join-string-content '(a b (foo bar) "aa" " " "c" (1 2)))
;; => '(a b (foo bar) "aa c" (1 2)
(define (join-string-content content)
  (let loop ((items content) (acc '()) (string-acc '()))
    (cond
     ;; end of list - flush accumulated strings if any
     ((null? items) (reverse (if (null? string-acc)
				 acc
				 (cons (string-join (reverse string-acc) "") acc))))
     ;; current item is a string - accumulate it
     ((string? (car items))
      (loop (cdr items) acc (cons (car items) string-acc)))
     ;; current item is not a string - flush it and add the item
     (else
      (let ((new-acc (if (null? string-acc)
			 (cons (car items) acc)
			 (cons (car items)
			       (cons (string-join (reverse string-acc) "") acc)))))
	(loop (cdr items) new-acc '()))))))

(define sxml->chiccup-rules
  `( ;; Document root - just process children
    (*TOP* . ,(lambda (tag children) children))
    (document . ,(lambda (tag children) children))
    
    ;; Headings
    (heading . ,(lambda (tag body)
                  (let ((level (car body))
                        (content (cadr body)))
                    (let ((h-tag (string->symbol (conc "h" level))))
                      `[,h-tag ,(string-join (filter string? content) "")]))))

    ;; Paragraphs
    (paragraph . ,(lambda (tag body)
                    `[p ,@(join-string-content body)]))

    ;; Text content
    (text . ,(lambda (tag body)
               (if (null? body)
                   ""
                   (string-join (filter string? body) ""))))

    ;; Code blocks
    (code_block . ,(lambda (tag body)
                     (let ((info (if (null? body) "" (car body)))
                           (content (if (null? body) '() (cdr body))))
                       `[pre [code ,(string-join (filter string? content) "")]])))

    ;; Inline code
    (code . ,(lambda (tag body)
               `[code ,(string-join (filter string? body) "")]))

    ;; Links
    (explicit-link . ,(lambda (tag attrs)
			(let* ((flat-attrs (reduce (lambda (q acc) (cons (car q) (cons (cdr q) acc))) '() attrs))
			       (href (memq 'href flat-attrs))
			       (title (memq 'title flat-attrs))
			       (label (memq 'label flat-attrs)))
			  `[a ((href . ,(cadr href))
			       ,@(if (and title (cadr title)) `((title . ,(cadr title))) '()))
			      ,(if label (car (join-string-content (cadr label))) "")])))

    ;; Emphasis
    (emphasis . ,(lambda (tag body)
                   `[em ,(string-join (filter string? body) "")]))

    ;; Strong
    (strong . ,(lambda (tag body)
                 `[strong ,(string-join (filter string? body) "")]))

    ;; Lists
    (bullet-list . ,(lambda (tag body)
		      `[ul ,@body]))
    (ordered-list . ,(lambda (tag body)
		      `[ol ,@body]))

    (item . ,(lambda (tag body)
               `[li ,(car (join-string-content body))]))

    ;; Line breaks
    (linebreak . ,(lambda (tag body) `[br]))

    ,@alist-conv-rules*))

(define (sxml->chiccup sxml #!optional (container '.w-full.p-4))
  `[,container ,@(pre-post-order* sxml sxml->chiccup-rules)])

;; (import chiccup)
(with-input-from-file "docs/test.md"
  (lambda ()
    (let ((doc (markdown->sxml*)))
      (ccup/html (sxml->chiccup doc)))))

(for-each
 (lambda (f)
   (with-output-to-file (conc "docs/" f ".md")
     (lambda ()
       (display '[h1 (string-titlecase f)]) (newline)
       (let ((doc-elements (read-docs f)))
	 (for-each
	  (lambda (doc-element)
	    (let ((doc-str (car doc-element))
		  (doc-elt (cadr doc-element)))
	      (display (make-markdown-header doc-elt)) (newline)
	      (display '[p doc-str]) (newline) (newline)))
	  doc-elements)))))
 files)
