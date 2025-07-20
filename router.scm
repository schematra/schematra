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
  '("/" schematra-default-handler))

;; find a resource based on a path on a tree. Returns the handler if
;; found, #f otherwise.
;; TODO: implement a version of this. Assume path is a list with strings and tree is a tree-like list, where each node can be compared to a path element. The leaf nodes should have a `procedure` that can be returned. If nothing is found, then it should return #f
(define (find-resource path tree)
  (cond
    ;; If path is empty, we've matched completely
    [(null? path)
     ;; Check if current tree node is a leaf with a procedure
     (if (and (list? tree) (= (length tree) 2) (procedure? (cadr tree)))
         (cadr tree)  ; return the procedure
         #f)]         ; not a valid leaf
    ;; If tree is not a list or is empty, no match
    [(not (and (list? tree) (>= (length tree) 2)))
     #f]
    ;; Try to match first path element with tree node
    [(string=? (car path) (car tree))
     ;; If this is a leaf node (has procedure), check if path is consumed
     (if (procedure? (cadr tree))
         (if (null? (cdr path))
             (cadr tree)  ; path fully matched, return procedure
             #f)          ; path not fully consumed, no match
         ;; Otherwise, continue searching in subtrees
         (let loop ((subtrees (cdr tree)))
           (cond
             [(null? subtrees) #f]
             [(list? (car subtrees))
              (let ((result (find-resource (cdr path) (car subtrees))))
                (if result result (loop (cdr subtrees))))]
             [else (loop (cdr subtrees))])))]
    ;; No match with current tree node
    [else #f]))

;; empty routes. each verb has a list of routes
(define schematra-get-routes '())
(define schematra-post-routes '())
(define schematra-vhost-default ".*")

(define (get path body)
  (let ((path-list (uri-path (uri-reference path))))
    (hash-table-set! schematra-get-routes path-list body)))

(define (post path body)
  (let ((path-list (uri-path (uri-reference path))))
    (hash-table-set! schematra-post-routes path-list body)))

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
