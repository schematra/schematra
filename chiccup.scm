;; Chiccup - part of Schematra. Chiccup is a very simple HTML
;; rendering system inspired in both haml and hiccup.
;; Copyright 2025 Rolando Abarca
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following
;; disclaimer in the documentation and/or other materials provided
;; with the distribution.
;;
;; 3. Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived
;; from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(module
 chiccup
 (
  ccup->html
  ccup/doctype
  ccup->sxml
  ccup/raw-content-tags
  ) ;; end export list

 (import scheme)
 (import srfi-1
	 srfi-13
	 chicken.base
	 chicken.string
	 chicken.port
	 chicken.pretty-print
	 sxml-transforms)

 ;; Parse a CSS-style selector string or symbol into parts (tag, classes, id)
 ;;
 ;; ## Examples
 ;; ```scheme
 ;; (parse-selector "div")     => (div () #f)
 ;; (parse-selector 'div.cls)  => (div ("cls") #f)
 ;; (parse-selector ".cls")    => (div ("cls") #f)
 ;; (parse-selector "div#id")  => (div () "id")
 ;; (parse-selector "#id")     => (div () "id")
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

 ;; Convert a parsed selector specification into SXML attributes
 (define (build-sxml-attrs spec-list)
   (if (= (length spec-list) 3)
       (let ((classes (cadr spec-list))
	     (id (caddr spec-list)))
	 (append
	  (if (and classes
		   (list? classes)
		   (not (null? classes)))
	      `((class ,(string-join classes " ")))
	      '())
	  (if id `((id ,id)) '())))
       '()))

 ;; Merge selector attributes with explicit @ attributes
 (define (merge-sxml-attrs spec-attrs explicit-attrs)
   (let* ((spec-class (assoc 'class spec-attrs))
          (explicit-class (assoc 'class explicit-attrs))
          (merged-class-attr
           (cond
            ((and spec-class explicit-class)
             `((class ,(string-append (cadr spec-class) " " (cadr explicit-class)))))
            (spec-class `(,spec-class))
            (explicit-class `(,explicit-class))
            (else '())))
          (non-class-spec (filter (lambda (attr) (not (eq? (car attr) 'class))) spec-attrs))
          (non-class-explicit (filter (lambda (attr) (not (eq? (car attr) 'class))) explicit-attrs))
	  (explicit-keys (map car non-class-explicit))
	  (spec-pruned
	   (filter (lambda (attr) (not (member (car attr) explicit-keys)))
		   non-class-spec)))
     (append merged-class-attr spec-pruned non-class-explicit)))

 ;; Convert chiccup element to SXML
 (define (ccup->sxml element-spec)
   (cond
    ;; Handle raw content - mark with special tag for processing
    ((and (list? element-spec) (eq? (car element-spec) 'raw))
     `(*RAW* ,(cadr element-spec)))
    ;; Handle regular chiccup elements
    ((list? element-spec)
     (let* ((parsed (parse-selector (car element-spec)))
	    (tag (car parsed))
	    (spec-attrs (build-sxml-attrs parsed))
	    (rest-parts (cdr element-spec))
	    ;; Check if second element is @ attributes
	    (has-at-attrs? (and (not (null? rest-parts))
				(list? (car rest-parts))
				(not (null? (car rest-parts)))
				(eq? (caar rest-parts) '@)))
	    (explicit-attrs (if has-at-attrs? (cdar rest-parts) '()))
	    (merged-attrs (merge-sxml-attrs spec-attrs explicit-attrs))
	    (body-parts (if has-at-attrs? (cdr rest-parts) rest-parts))
	    (sxml-body (map ccup->sxml body-parts)))
       (if (null? merged-attrs)
	   `(,tag ,@sxml-body)
	   `(,tag (@ ,@merged-attrs) ,@sxml-body))))
    ;; Handle strings and other content
    (else element-spec)))

 (define ccup/doctype (make-parameter "<!doctype html>"))
 (define ccup/raw-content-tags (make-parameter '(script style)))

 ;; HTML void elements that should not have closing tags
 (define ccup/void-elements
   '(area base br col embed hr img input link meta param source track wbr))

 ;; Custom enattr that properly escapes attribute values
 (define (ccup/enattr attr-key value)
   (if (null? value)
       (list #\space attr-key)
       (let ((val-str (if (list? value) (car value) value)))
         (list #\space attr-key "=\"" (string->goodHTML val-str) #\"))))

 (define (escape-content tag content)
   (cond
    ;; just return the content for these tags
    ((member tag (ccup/raw-content-tags))
     content)
    ((string? content)
     (string->goodHTML content))
    (else
     (map (lambda (itm) (if (string? itm) (string->goodHTML itm) itm)) content))))

 ;; Custom entag that removes the leading newline and handles text escaping
 (define (ccup/entag tag elems)
   (cond
    ;; For *RAW* tags, return the structure as-is for later processing
    ((eq? tag '*RAW*)
     (list tag (car elems)))
    ;; Regular case: generate HTML tags, escape based on raw-content-tags list
    (else
     (let* ((is-void? (member tag ccup/void-elements))
            (has-attrs? (and (pair? elems) (pair? (car elems)) (eq? '@ (caar elems))))
            (attrs (if has-attrs? (cdar elems) '()))
            (body (if has-attrs? (cdr elems) elems)))
       (if is-void?
           ;; Void element - no closing tag
           (if has-attrs?
               (list #\< tag attrs #\>)
               (list #\< tag #\>))
           ;; Non-void element - always include closing tag
           (if has-attrs?
               (list #\< tag attrs #\>
                     (if (pair? body)
                         (list (escape-content tag body) "</" tag #\>)
                         (list "</" tag #\>)))
               (list #\< tag #\>
                     (if (pair? elems)
                         (list (escape-content tag elems) "</" tag #\>)
                         (list "</" tag #\>)))))))))

 ;; Custom SXML->HTML without newlines and with raw content support
 (define (ccup/SXML->HTML tree)
   (define (unwrap-raw content)
     (cond
      ((and (list? content) 
            (not (null? content)) 
            (eq? (car content) '*RAW*))
       (cadr content))  ; Unwrap raw content
      ((and (list? content) (not (null? content)))
       (map unwrap-raw content))  ; Recursively process non-empty lists
      (else content)))  ; Pass through everything else (strings, chars, etc.)
   
   (let ((processed-tree (pre-post-order* tree
                                          `((@
                                             ((*default* . ,(lambda (attr-key value) (ccup/enattr attr-key value))))
                                             . ,(lambda (trigger value) (cons '@ value)))
                                            (*default* . ,(lambda (tag elems)
                                                            (ccup/entag tag elems)))
                                            (*text* . ,(lambda (trigger str) str))))))
     (SRV:send-reply (unwrap-raw processed-tree))))

 ;; Generate HTML from a chiccup element-spec by converting to SXML first.
 ;;
 ;; ### Parameters
 ;; `ELEMENT-SPEC` is a list with at least one item that defines a
 ;; CSS-inspired selector. If the tag name is not specified it's assumed
 ;; to be a `div`. The second element can optionally be `(@ ...)` with SXML
 ;; attributes, and the remaining elements are the body content.
 ;;
 ;; ### Security
 ;; String content is automatically HTML-sanitized to prevent XSS attacks.
 ;; To inject raw HTML without sanitization, wrap it with `(raw "content")`.
 ;;
 ;; ### Examples
 ;; ```scheme
 ;; (ccup/html `[.h-4.w-4 "content"])
 ;; ;; => <div class="h-4 w-4">content</div>
 ;;
 ;; (ccup/html `[div "< & >"])
 ;; ;; => <div>&lt; &amp; &gt;</div>
 ;;
 ;; (ccup/html `[div (raw "<em>italic</em>")])
 ;; ;; => <div><em>italic</em></div>
 ;;
 ;; (ccup/html `[button (@ (disabled) (type "submit")) "Click me"])
 ;; ;; => <button disabled type="submit">Click me</button>
 ;;
 ;; (ccup/html `[ul#foo (@ (hx-post "/endpoint")) [li "item"]])
 ;; ;; => <ul id="foo" hx-post="/endpoint"><li>item</li></ul>
 ;; ```
 (define (ccup->html element-spec)
   (let* ((sxml-tree (ccup->sxml element-spec))
	  (html-content (with-output-to-string
			  (lambda ()
			    (ccup/SXML->HTML sxml-tree)))))
     (if (eq? (car sxml-tree) 'html)
	 ;; Special handling for html tag to include doctype
	 (string-append (ccup/doctype) html-content)
	 ;; Regular element
	 html-content)))

 ;; end module
 )
