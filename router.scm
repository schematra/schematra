(module schematra
(;; Parameters
 schematra-default-handler
 schematra-default-vhost
 ;; Procedures
 get post
 schematra-install
 )
)

(import
 spiffy
 format
 intarweb
 uri-common
 srfi-13
 srfi-18)

;; Default handler for unmatched routes
;; 
;; This handler is called when no specific route matches the incoming request.
;; It serves as a fallback and returns a simple welcome message.
;; 
;; Parameters:
;;   params: optional parameter alist (ignored by default handler)
;; 
;; Returns:
;;   A string containing the default welcome message
(define (schematra-default-handler _request #!optional params)
  "Welcome to Schematra, Sinatra's weird friend.")

(define (make-path-tree)
  `("/" ,schematra-default-handler))

;; find a resource based on a path on a tree. Returns the handler if
;; found, #f otherwise.
(define (find-resource path tree)
  (find-resource-with-params path tree '()))

(define (find-resource-with-params path tree params)
  (cond
    ;; If tree is not a list or is empty, no match
    [(not (and (list? tree) (>= (length tree) 2)))
     #f]
    ;; Try to match first path element with tree node
    [(or (string=? (car path) (car tree))
         (and (string-prefix? ":" (car tree)) (not (null? path))))
     ;; Collect parameter if this is a param segment
     (let ((new-params (if (string-prefix? ":" (car tree))
                           (cons (cons (substring (car tree) 1) (car path)) params)
                           params)))
       ;; If we've matched and this is the last path element
       (if (null? (cdr path))
           ;; Check if second element is a procedure (leaf node)
           (if (procedure? (cadr tree))
               (list (cadr tree) (reverse new-params))  ; Return handler and params
               #f)
           ;; Otherwise, continue searching in subtrees
           (let loop ((subtrees (cddr tree)))
             (cond
               [(null? subtrees) #f]
               [(list? (car subtrees))
                (let ((result (find-resource-with-params (cdr path) (car subtrees) new-params)))
                  (if result result (loop (cdr subtrees))))]
               [else (loop (cdr subtrees))]))))]
    ;; No match with current tree node
    [else #f]))

(define (add-resource! path tree handler)
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
(define get-routes (make-path-tree))
(define post-routes (make-path-tree))
(define schematra-default-vhost ".*")

(define (normalize-path path-list)
  (let* (;; Ensure path-list is actually a list
         (path-as-list (if (list? path-list) path-list (list path-list)))
         ;; Ensure all elements are strings, converting symbols to strings
         (string-path (map (lambda (segment)
                            (if (symbol? segment)
                                (symbol->string segment)
                                segment))
                          path-as-list))
         ;; Filter out empty strings
         (normalized-path (filter (lambda (segment)
                                   (not (string=? segment "")))
                                 string-path)))
    normalized-path))

;; Register a GET route handler
;; path: string representing the URL path (e.g., "/users", "/api/posts")
;; body: procedure that takes a request object and optional params, returns response
(define (get path body)
  (let ((raw-uri-path (uri-path (uri-reference path))))
    (set! get-routes
	  (add-resource! (normalize-path raw-uri-path) get-routes body))))

;; Register a POST route handler  
;; path: string representing the URL path (e.g., "/users", "/api/posts")
;; body: procedure that takes a request object and optional params, returns response
(define (post path body)
  (let ((raw-uri-path (uri-path (uri-reference path))))
    (set! post-routes
	  (add-resource! (normalize-path raw-uri-path) post-routes body))))

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
	 (normalized-path (normalize-path (uri-path (request-uri request))))
	 (route-handlers
	  (cond
	   [(eq? method 'GET) get-routes]
	   [(eq? method 'POST) post-routes]
	   [else (error "no handlers for this method")]))
	 (result (find-resource normalized-path route-handlers)))
    (format #t "Req: ~A. Path: ~A. Method: ~A\n" request normalized-path method)
    (if result 
        (let ((handler (car result))
              (params (cadr result)))
          (schematra-parse-response (handler request params)))
        (continue))))

;; Install the Schematra router as a virtual host handler
;; 
;; This function configures the Spiffy web server to use Schematra's routing system.
;; It maps the default virtual host pattern (.*) to the schematra-router function,
;; which will handle all incoming HTTP requests by matching them against registered
;; routes and calling the appropriate handlers.
;;
;; Call this function before starting the server to enable route handling.
;; Example usage:
;;   (schematra-install)
;;   (start-server port: 8080)
(define (schematra-install)
  (vhost-map
   `((,schematra-default-vhost . ,(lambda (continue) (schematra-router continue))))))

(thread-start!
 (lambda ()
   (start-server port: 8080)))
