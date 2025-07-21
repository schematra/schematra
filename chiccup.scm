(module chiccup
 (
  ccup/html
 ) ;; end export list

 (import scheme)
 (import format
	 srfi-1
	 srfi-13
	 (chicken base)
	 (chicken string))

 ;; Return a list of substrings of STR split on '.'.
 ;; e.g. "foo.bar" => '("foo" "bar")
 (define (split-on-dot str)
   (let ((len (string-length str)))
     (let loop ((i 0) (start 0) (acc '()))
       (if (= i len)
	   (reverse (cons (substring str start len) acc))
	   (if (char=? (string-ref str i) #\.)
	       (loop (add1 i) (add1 i) (cons (substring str start i) acc))
	       (loop (add1 i) start acc))))))

 ;; Parse a CSS-style selector string or symbol into parts (tag, classes, id)
 ;;
 ;; Examples:
 ;; (parse-selector "div") => ("div" () #f)
 ;; (parse-selector 'div.cls) => ("div" ("cls") #f)
 ;; (parse-selector ".cls") => ("div" ("cls") #f)
 ;; (parse-selector "div#id") => ("div" () "id")
 ;; (parse-selector "#id") => ("div" () "id")
 ;; Note: Following CSS conventions, ID (#id) must come last in the selector.
 (define (parse-selector selector)
   (let* ((selector-str (if (symbol? selector)
                            (symbol->string selector)
                            selector))
          ;; parse id part
	  (id-parts (string-split selector-str "#" #t))
	  (has-id? (> (length id-parts) 1))
	  (id-str (if has-id? (cadr id-parts) #f))
	  (before-id (car id-parts))
	  (starts-with-hash? (string=? before-id ""))
	  ;; parse class part
	  (class-parts (string-split (if starts-with-hash? "" before-id) "." #t))
	  (first-part (car class-parts))
	  (class-list (cdr class-parts))
	  (starts-with-dot? (string=? first-part ""))
	  ;; extract tag name
	  (tag-name (cond
		     ((or starts-with-hash? starts-with-dot? (string=? first-part "")) "div")
		     (else first-part))))
     (list tag-name class-list id-str)))

 ;; Convert a parsed selector specification into an attribute alist.
 ;;
 ;; SPEC-LIST should be a list of the form (tag classes id) where:
 ;;   - tag is a string representing the HTML tag name
 ;;   - classes is a list of strings representing CSS classes
 ;;   - id is a string or #f representing the element ID
 ;;
 ;; Returns an alist of (attr-name . attr-value) pairs suitable for HTML element
 ;; attributes. Empty when given invalid input.
 ;;
 ;; Example: '("div" ("btn" "primary") "submit") =>
 ;;          '(("class" . "btn primary") ("id" . "submit"))
 (define (build-attrs spec-list)
   (if (= (length spec-list) 3)
       (let ((classes (cadr spec-list))
	     (id (caddr spec-list)))
	 (append
	  (if (and classes
		   (list? classes) ;; ensure classes is a list
		   (not (null? classes)))
	      `(("class" . ,(string-join classes " ")))
	      '())
	  (if id `(("id" . ,id)) '())))
       '()))

 (define (merge-attrs spec-attrs explicit-attrs)
   (let* ((spec-class (assoc "class" spec-attrs))
          (explicit-class (assoc "class" explicit-attrs))
          (merged-class-attr
           (cond
            ((and spec-class explicit-class)
             `(("class" . ,(string-append (cdr spec-class) " " (cdr explicit-class)))))
            (spec-class `((,spec-class)))
            (explicit-class `((,explicit-class)))
            (else '())))
          (non-class-spec (filter (lambda (attr) (not (string=? (car attr) "class"))) spec-attrs))
          (non-class-explicit (filter (lambda (attr) (not (string=? (car attr) "class"))) explicit-attrs)))
     (append merged-class-attr non-class-spec non-class-explicit)))

 ;; Generate an HTML tag from an element-spec list.
 ;;
 ;; ELEMENT-SPEC is a list with at least one item that defines a
 ;; css-inspired selector. If the tag name is not specified it's assumed
 ;; to be a `div`. The second element can be an alist with the extra
 ;; attributes and the last element is expected to be the body, which can
 ;; be nested element-specs.
 ;;
 ;; Examples:
 ;; (ccup/html `[.h-4.w-4 "content"]) -> <div class="h-4 w-4">content</div>
 ;; (ccup/html
 ;;   `[ul#foo ({ hx-post . "/my/endpoint"})
 ;;     [li "some item"]])
 ;; -> <ul id="foo" hx-post="/my/endpoint"><li>some item</li></ul>
 (define (ccup/html element-spec)
   (let* ( ;; parse the selector
	  (parsed (parse-selector (car element-spec)))
	  (tag (car parsed))
	  (spec-attrs (build-attrs parsed))
	  ;; detect whether the caller passed an explicit alist as second element
	  (second-arg (if (> (length element-spec) 1) (cadr element-spec) '()))
	  (has-explicit-attrs?
	   (and (list? second-arg)
		(not (null? second-arg))
		(pair? (car second-arg))))
	  ;; merge the two possible attribute lists
	  (merged-attrs
	   (if has-explicit-attrs? (merge-attrs spec-attrs second-arg) spec-attrs))
	  ;; collect the body parts, which can be a string or another
	  ;; element-spec
	  (body-parts
	   (if has-explicit-attrs? 
	       (cddr element-spec)  ; skip selector and attrs, take rest as body
	       (cdr element-spec))) ; skip selector, take rest as body
	  ;; build the final attribute string
	  (attr-str (if (null? merged-attrs)
			""
			(string-append " "
				       (string-join
					(map (lambda (pr)
					       (string-append (car pr) "=\"" (cdr pr) "\""))
					     merged-attrs)
					" "))))
	  ;; build the body
	  (body-str (if (null? body-parts)
			""
			(apply string-append
			       (map (lambda (part)
				      (cond
				       ((string? part) part)
				       ;; assume a list is another element spec
				       ((list? part) (ccup/html part))
				       ;; anything else, just try its string representation
				       (else (format "~A" part))))
				    body-parts)))))
     ;; finally render
     (string-append "<" tag attr-str ">" body-str "</" tag ">")))
 ) ;; end module
