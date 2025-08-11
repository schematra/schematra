;; Chiccup - part of Schematra. Chiccup is a very simple HTML
;; rendering system inspired in both haml and hiccup.
;; Copyright (c) 2025 Rolando Abarca <cpm.rolandoa@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

(module
 chiccup
 (
  ccup/html
  ccup/doctype
  ) ;; end export list

 (import scheme)
 (import format
	 srfi-1
	 srfi-13
	 chicken.base
	 chicken.string)

 ;; https://developer.mozilla.org/en-US/docs/Glossary/Void_element
 (define void-elements
   '(area base br col embed hr img input link meta source track wbr))

 ;; Parse a CSS-style selector string or symbol into parts (tag, classes, id)
 ;;
 ;; ## Examples
 ;; ```scheme
 ;; (parse-selector "div")     => ("div" () #f)
 ;; (parse-selector 'div.cls)  => ("div" ("cls") #f)
 ;; (parse-selector ".cls")    => ("div" ("cls") #f)
 ;; (parse-selector "div#id")  => ("div" () "id")
 ;; (parse-selector "#id")     => ("div" () "id")
 ;; ```
 ;;
 ;; **Note:** Following CSS conventions, ID (#id) must come last in the selector.
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
     (list (string->symbol (string-downcase tag-name)) class-list id-str)))

 ;; Convert a parsed selector specification into an attribute alist.
 ;;
 ;; ## Parameters
 ;; `SPEC-LIST` should be a list of the form `(tag classes id)` where:
 ;; - `tag` is a symbol representing the HTML tag name
 ;; - `classes` is a list of strings representing CSS classes
 ;; - `id` is a string or `#f` representing the element ID
 ;;
 ;; ## Returns
 ;; An alist of `(attr-name . attr-value)` pairs suitable for HTML element
 ;; attributes. Empty when given invalid input.
 ;;
 ;; ## Example
 ;; ```scheme
 ;; '(div ("btn" "primary") "submit") => '(("class" . "btn primary") ("id" . "submit"))
 ;; ```
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
            (spec-class `(,spec-class))
            (explicit-class `(,explicit-class))
            (else '())))
          (non-class-spec (filter (lambda (attr) (not (string=? (car attr) "class"))) spec-attrs))
          (non-class-explicit (filter (lambda (attr) (not (string=? (car attr) "class"))) explicit-attrs))
	  (explicit-keys (map car non-class-explicit))
	  (spec-pruned
	   (filter (lambda (attr) (not (member (car attr) explicit-keys string=?)))
		   non-class-spec)))
     (append merged-class-attr spec-pruned non-class-explicit)))

 ;; copied from Spiffy
 (define (htmlize str)
   (string-translate* (format "~A" str)
		      '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;") ("\"" . "&quot;") ("'" . "&#x27;"))))

 (define ccup/doctype (make-parameter "<!doctype html>"))

 (define (attr-key->string k)
   (cond ((string? k) k)
	 ((symbol? k) (string-translate (string-downcase (symbol->string k)) "_" "-"))
	 (else (format "~A" k))))

 (define (normalize-attrs alist)
   (map (lambda (p) (cons (attr-key->string (car p)) (cdr p))) alist))

 (define (render-attr attr)
   (let ((k (car attr))
	 (v (cdr attr)))
     (cond
      ((eq? v #t) (attr-key->string k)) ;; boolean attribute
      ((or (eq? v #f) (not v)) #f) ;; omit false/nil
      (else (string-append (attr-key->string k)
			   "=\"" (htmlize v) "\"")))))

;;; Generate an HTML tag from an element-spec list.
;;;
;;; ### Parameters
;;; `ELEMENT-SPEC` is a list with at least one item that defines a
;;; CSS-inspired selector. If the tag name is not specified it's assumed
;;; to be a `div`. The second element can be an alist with the extra
;;; attributes and the last element is expected to be the body, which can
;;; be nested element-specs.
;;;
;;; ### Security
;;; String content is automatically HTML-sanitized to prevent XSS attacks.
;;; To inject raw HTML without sanitization, wrap it with `(raw "content")`.
;;;
;;; ### Examples
;;; ```scheme
;;; (ccup/html `[.h-4.w-4 "content"])
;;; ;; => <div class="h-4 w-4">content</div>
;;;
;;; (ccup/html `[div "< & >"])
;;; ;; => <div>&lt; &amp; &gt;</div>
;;;
;;; (ccup/html `[div (raw "<em>italic</em>")])
;;; ;; => <div><em>italic</em></div>
;;;
;;; (ccup/html
;;;   `[ul#foo ({ hx-post . "/my/endpoint"})
;;;     [li "some item"]])
;;; ;; => <ul id="foo" hx-post="/my/endpoint"><li>some item</li></ul>
;;;
;;; (ccup/html
;;;   `[button ((disabled . #t)) "the button"])
;;; ;; => <button disabled>the button</button>
;;; ```
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
	  (explicit-attrs* (if has-explicit-attrs? (normalize-attrs second-arg) '()))
	  (merged-attrs (merge-attrs spec-attrs explicit-attrs*))
	  ;; collect the body parts, which can be a string or another
	  ;; element-spec
	  (body-parts
	   (if has-explicit-attrs? 
	       (cddr element-spec) ; skip selector and attrs, take rest as body
	       (cdr element-spec))) ; skip selector, take rest as body
	  ;; build the final attribute string
	  (rendered-attrs (filter identity (map render-attr merged-attrs)))
	  (attr-str (if (null? rendered-attrs)
			""
			(string-append " " (string-join rendered-attrs))))
	  ;; build the body
	  (body-str (if (null? body-parts)
			""
			(apply string-append
			       (map (lambda (part)
				      (cond
				       ;; sanitize string
				       ((string? part) (htmlize part))
				       ;; skip empty parts
				       ((null? part) "")
				       ;; allow raw strings [tag ('raw "<em>text</em>")]
				       ((and (list? part) (eq? (car part) 'raw)) (cadr part))
				       ;; assume a list might be another element spec
				       ((list? part) (ccup/html part))
				       ;; anything else, just try its string representation
				       (else (format "~A" part))))
				    body-parts)))))
     ;; finally render
     (if (member tag void-elements)
	 (begin
	   (when (not (null? body-parts))
	     (error (format "Void element '~a' cannot have body content: ~a" tag body-parts)))
	   (string-append "<" (symbol->string tag) attr-str ">"))
	 (let ((tag-str (symbol->string tag)))
	   (if (eq? tag 'html)
	       (string-append (ccup/doctype) "<html" attr-str ">" body-str "</html>")
	       (string-append "<" tag-str attr-str ">" body-str "</" tag-str ">"))))))
 ;; end module
 )
