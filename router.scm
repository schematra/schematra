(import
 spiffy
 format
 intarweb
 uri-common
 srfi-13
 srfi-18)

(define (schematra-default-handler)
  (lambda (#!optional params)
    "Welcome to Schematra"))

(define (make-path-tree)
  `("/" ,schematra-default-handler))

;; find a resource based on a path on a tree. Returns the handler if
;; found, #f otherwise.
;; TODO: implement a version of this. Assume path is a list with strings and tree is a tree-like list, where each node can be compared to a path element. The leaf nodes should have a `procedure` that can be returned. If nothing is found, then it should return #f
(define (find-resource path tree)
  (cond
    ;; If tree is not a list or is empty, no match
    [(not (and (list? tree) (>= (length tree) 2)))
     #f]
    ;; Try to match first path element with tree node
    [(string=? (car path) (car tree))
     ;; If we've matched and this is the last path element
     (if (null? (cdr path))
         ;; Check if second element is a procedure (leaf node)
         (if (procedure? (cadr tree))
             (cadr tree)
             #f)
         ;; Otherwise, continue searching in subtrees
         (let loop ((subtrees (cddr tree)))
           (cond
             [(null? subtrees) #f]
             [(list? (car subtrees))
              (let ((result (find-resource (cdr path) (car subtrees))))
                (if result result (loop (cdr subtrees))))]
             [else (loop (cdr subtrees))])))]
    ;; No match with current tree node
    [else #f]))

;; TODO: implement in a way that works with find-resource
(define (add-resource path tree handler)
  (if (null? path)
      tree  ; Empty path, return tree as-is
      (let ((target-segment (car path))
            (remaining-path (cdr path)))
        (cond
          ;; If tree matches current segment
          [(and (list? tree) (>= (length tree) 2) (string=? target-segment (car tree)))
           (if (null? remaining-path)
               ;; Last segment, update handler but preserve existing subtrees
               (append (list (car tree) handler) (cddr tree))
               ;; More segments, recursively add to subtrees
               (let loop ((subtrees (cddr tree)) (result (list (car tree) (cadr tree))))
                 (cond
                   [(null? subtrees)
                    ;; No matching subtree, create new one
                    (append result (list (add-resource remaining-path (list (car remaining-path) #f) handler)))]
                   [(and (list? (car subtrees)) (string=? (car remaining-path) (caar subtrees)))
                    ;; Found matching subtree
                    (append result 
                            (list (add-resource remaining-path (car subtrees) handler))
                            (cdr subtrees))]
                   [else
                    ;; Keep looking
                    (loop (cdr subtrees) (append result (list (car subtrees))))])))]
          ;; Tree doesn't match, create new tree for this path
          [else
           (if (null? remaining-path)
               (list target-segment handler)
               (list target-segment #f (add-resource remaining-path (list (car remaining-path) #f) handler)))]))))

;; empty routes. each verb has a list of routes
(define schematra-get-routes (make-path-tree))
(define schematra-post-routes (make-path-tree))
(define schematra-vhost-default ".*")

(define (normalize-path path)
  (let* ((path-list (uri-path (uri-reference path)))
         ;; Ensure all elements are strings, converting symbols to strings
         (normalized-path (map (lambda (segment)
                                (if (symbol? segment)
                                    (symbol->string segment)
                                    segment))
                              path-list)))
    normalized-path))

;; Register a GET route handler
;; path: string representing the URL path (e.g., "/users", "/api/posts")
;; body: procedure that takes a request object and optional params, returns response
(define (get path body)
  (let ((normalized-path (normalize-path path)))
    (hash-table-set! schematra-get-routes normalized-path body)))

;; Register a POST route handler  
;; path: string representing the URL path (e.g., "/users", "/api/posts")
;; body: procedure that takes a request object and optional params, returns response
(define (post path body)
  (let ((normalized-path (normalize-path path)))
    (hash-table-set! schematra-post-routes normalized-path body)))

(define (alist? x)
  (and (list? x)
       (every pair? x)))

(define (is-response? list)
  (and
   ;; is this a list?
   (list? list)
   ;; we need a pair of at least two items: status & body
   (>= (length list) 2)
   ;; first element should be a valid status symbol
   (symbol? (car list))
   ;; next item should be a valid body-type
   (string? (cadr list))
   ;; should check for headers next
   (if (= 3 (length list)) (alist? (list-ref list 2)) #t)))

(define (schematra-parse-response resp)
  (cond
   [(string? resp) (send-response status: 'ok body: resp)]
   [else
    (send-response
     status: 'error
     body: (format #f "Error: response type not supported (~A)" resp))]))

(define (schematra-request-augment request)
  request)

(define (schematra-router continue)
  (let* ((request (current-request))
	 (method (request-method request))
	 (path (uri-path (request-uri request)))
	 (route-handlers
	  (cond
	   [(eq? method 'GET) schematra-get-routes]
	   [(eq? method 'POST) schematra-post-routes]
	   [else (error "no handlers for this method")]))
	 (handler (hash-table-ref/default route-handlers path #f)))
    (format #t "Req: ~A. Path: ~A. Method: ~A\n" request path method)
    (if handler (schematra-parse-response (handler request)) (continue))))

;; install vhost handler
(vhost-map
 `((,schematra-vhost-default . ,(lambda (continue) (schematra-router continue)))))

(get "/foo/:bar"
     (lambda (request #!optional (params '()))
       "OK"))

(thread-start!
 (lambda ()
   (start-server port: 8080)))
