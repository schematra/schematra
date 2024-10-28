#!/usr/bin/env csi -s
;; Chiccup Benchmark Suite
;; Measures performance of chiccup HTML generation to determine if caching is needed

(import chiccup
        srfi-1
        srfi-18
        srfi-48
        chicken.string
        chicken.time)

;; Benchmark configuration
(define *warmup-iterations* 300)
(define *benchmark-iterations* 20000)

;; Benchmark helpers
(define (benchmark-fn name fn iterations)
  "Run a function multiple times and measure execution time"
  (let* ((start (current-milliseconds))
         (_ (let loop ((i 0))
              (when (< i iterations)
                (fn)
                (loop (+ i 1)))))
         (end (current-milliseconds))
         (elapsed-ms (- end start))
         (ops-per-sec (/ (* iterations 1000.0) elapsed-ms))
         (avg-us (/ (* elapsed-ms 1000.0) iterations)))
    (list name iterations elapsed-ms ops-per-sec avg-us)))

(define (print-benchmark-result result)
  "Print benchmark results in a formatted table row"
  (let ((name (list-ref result 0))
        (iterations (list-ref result 1))
        (elapsed-ms (list-ref result 2))
        (ops-per-sec (list-ref result 3))
        (avg-us (list-ref result 4)))
    (format #t "~40F | ~10F | ~10,2F ms | ~12,0F ops/s | ~10,3F μs~%"
            name
            iterations
            elapsed-ms
            ops-per-sec
            avg-us)))

;; Test data structures
(define simple-element
  `[div "hello"])

(define element-with-class
  `[.container.main "content"])

(define element-with-id-and-class
  `[.container#main "content"])

(define element-with-attributes
  `[.container (@ (data-id "123") (hx-post "/api/endpoint") (data-value "test")) "content"])

(define nested-element-shallow
  `[.container
    [h1 "Title"]
    [p "Paragraph"]
    [button "Click me"]])

(define nested-element-deep
  `[.wrapper
    [.container
     [.header
      [nav.nav
       [ul
        [li [a (@ (href "/")) "Home"]]
        [li [a (@ (href "/about")) "About"]]
        [li [a (@ (href "/contact")) "Contact"]]]]]
     [.content
      [article
       [h1.title "Article Title"]
       [p.intro "Introduction paragraph"]
       [.body
        [p "First paragraph"]
        [p "Second paragraph"]
        [p "Third paragraph"]]]]
     [.footer
      [p "Footer content"]]]])

(define list-generation
  `[ul.list
    ,@(map (lambda (i) `[li.item (@ (data-id ,(number->string i))) ,(format "Item ~a" i)])
           (iota 20))])

(define table-generation
  `[table.data-table
    [thead
     [tr
      [th "ID"]
      [th "Name"]
      [th "Value"]]]
    [tbody
     ,@(map (lambda (i)
              `[tr
                [td ,(number->string i)]
                [td ,(format "Name ~a" i)]
                [td ,(format "Value ~a" i)]])
            (iota 50))]])

(define form-with-inputs
  `[form (@ (action "/submit") (method "post"))
    [.form-group
     [label (@ (for "username")) "Username"]
     [input (@ (type "text") (id "username") (name "username") (required))]]
    [.form-group
     [label (@ (for "email")) "Email"]
     [input (@ (type "email") (id "email") (name "email") (required))]]
    [.form-group
     [label (@ (for "password")) "Password"]
     [input (@ (type "password") (id "password") (name "password") (required))]]
    [.form-group
     [label (@ (for "bio")) "Bio"]
     [textarea (@ (id "bio") (name "bio") (rows "5"))]]
    [.form-group
     [input (@ (type "checkbox") (id "terms") (name "terms"))]
     [label (@ (for "terms")) "I agree to terms"]]
    [button (@ (type "submit")) "Submit"]])

(define html-document
  `[html
    [head
     [meta (@ (charset "utf-8"))]
     [meta (@ (name "viewport") (content "width=device-width, initial-scale=1"))]
     [title "Benchmark Page"]
     [link (@ (rel "stylesheet") (href "/css/style.css"))]]
    [body
     [div.container
      [header.header
       [h1 "Welcome"]
       [nav [ul [li [a (@ (href "/")) "Home"]]]]]
      [main.content
       [article
        [h2 "Article"]
        [p "Content"]]]
      [footer.footer
       [p "Footer"]]]]])

;; Elements with special escaping requirements
(define element-with-special-chars
  `[div "< > & \" '"])

(define element-with-script-content
  `[script "if (x < 5 && y > 3) { console.log(\"hello\"); }"])

(define element-with-raw-content
  `[div (raw "<em>italic</em> <strong>bold</strong>")])

;; Complex real-world example: blog post list
(define blog-post-list
  `[.posts-container
    ,@(map (lambda (i)
             `[article.post
               [h2.post-title [a (@ (href ,(format "/post/~a" i))) ,(format "Blog Post ~a" i)]]
               [div.post-meta
                [span.author "Author Name"]
                [span.date ,(format "2025-01-~a" (+ i 1))]]
               [p.post-excerpt ,(format "This is an excerpt from blog post ~a. It gives a preview of the content..." i)]
               [a.read-more (@ (href ,(format "/post/~a" i))) "Read more →"]])
           (iota 10))])

;; Run benchmarks
(define (run-benchmarks)
  (display "\n╔═══════════════════════════════════════════════════════════════════════════════════════════╗\n")
  (display "║                            Chiccup Performance Benchmark                                  ║\n")
  (display "╠═══════════════════════════════════════════════════════════════════════════════════════════╣\n")
  (format #t "║ Warmup iterations: ~d                                                                     ║~%" *warmup-iterations*)
  (format #t "║ Benchmark iterations: ~d                                                                  ║~%" *benchmark-iterations*)
  (display "╚═══════════════════════════════════════════════════════════════════════════════════════════╝\n\n")

  ;; Warmup phase
  (display "Warming up...\n")
  (ccup->html simple-element)
  (let loop ((i 0))
    (when (< i *warmup-iterations*)
      (ccup->html nested-element-deep)
      (loop (+ i 1))))

  (display "\nRunning benchmarks...\n\n")
  (format #t "~40F | ~10F | ~13F | ~15F | ~F~%"
          "Test Name" "Iterations" "Total Time" "Ops/Second" "Avg Time")
  (display (make-string 120 #\-))
  (display "\n")

  (let ((results '()))
    ;; Basic element tests
    (set! results (cons (benchmark-fn "Simple element"
                                      (lambda () (ccup->html simple-element))
                                      *benchmark-iterations*)
                        results))

    (set! results (cons (benchmark-fn "Element with class"
                                      (lambda () (ccup->html element-with-class))
                                      *benchmark-iterations*)
                        results))

    (set! results (cons (benchmark-fn "Element with ID and class"
                                      (lambda () (ccup->html element-with-id-and-class))
                                      *benchmark-iterations*)
                        results))

    (set! results (cons (benchmark-fn "Element with attributes"
                                      (lambda () (ccup->html element-with-attributes))
                                      *benchmark-iterations*)
                        results))

    ;; Nesting tests
    (set! results (cons (benchmark-fn "Shallow nesting (3 levels)"
                                      (lambda () (ccup->html nested-element-shallow))
                                      *benchmark-iterations*)
                        results))

    (set! results (cons (benchmark-fn "Deep nesting (6+ levels)"
                                      (lambda () (ccup->html nested-element-deep))
                                      *benchmark-iterations*)
                        results))

    ;; Generation tests
    (set! results (cons (benchmark-fn "List generation (20 items)"
                                      (lambda () (ccup->html list-generation))
                                      *benchmark-iterations*)
                        results))

    (set! results (cons (benchmark-fn "Table generation (50 rows)"
                                      (lambda () (ccup->html table-generation))
                                      *benchmark-iterations*)
                        results))

    ;; Form test
    (set! results (cons (benchmark-fn "Form with inputs"
                                      (lambda () (ccup->html form-with-inputs))
                                      *benchmark-iterations*)
                        results))

    ;; Full document
    (set! results (cons (benchmark-fn "HTML document"
                                      (lambda () (ccup->html html-document))
                                      *benchmark-iterations*)
                        results))

    ;; Special cases
    (set! results (cons (benchmark-fn "Special char escaping"
                                      (lambda () (ccup->html element-with-special-chars))
                                      *benchmark-iterations*)
                        results))

    (set! results (cons (benchmark-fn "Script content (no escape)"
                                      (lambda () (ccup->html element-with-script-content))
                                      *benchmark-iterations*)
                        results))

    (set! results (cons (benchmark-fn "Raw content"
                                      (lambda () (ccup->html element-with-raw-content))
                                      *benchmark-iterations*)
                        results))

    ;; Real-world example
    (set! results (cons (benchmark-fn "Blog post list (10 posts)"
                                      (lambda () (ccup->html blog-post-list))
                                      *benchmark-iterations*)
                        results))

    ;; Print all results
    (for-each print-benchmark-result (reverse results))

    ;; Calculate summary statistics
    (let* ((ops-per-sec-list (map (lambda (r) (list-ref r 3)) results))
           (avg-us-list (map (lambda (r) (list-ref r 4)) results))
           (min-ops (apply min ops-per-sec-list))
           (max-ops (apply max ops-per-sec-list))
           (avg-ops (/ (apply + ops-per-sec-list) (length ops-per-sec-list)))
           (min-time (apply min avg-us-list))
           (max-time (apply max avg-us-list))
           (avg-time (/ (apply + avg-us-list) (length avg-us-list))))

      (display "\n")
      (display (make-string 120 #\-))
      (display "\n\n")
      (display "Summary Statistics:\n")
      (format #t "  Operations/second: min=~10,0F, max=~10,0F, avg=~10,0F~%"
              min-ops max-ops avg-ops)
      (format #t "  Avg time (μs):     min=~10,3F, max=~10,3F, avg=~10,3F~%"
              min-time max-time avg-time)
      (display "\n")

      ;; Caching recommendation
      (display "Caching Recommendation:\n")
      (cond
       ((< avg-ops 10000)
        (display "  ⚠️  CONSIDER CACHING: Performance is below 10,000 ops/sec.\n")
        (display "      Caching static templates could provide significant benefits.\n"))
       ((< avg-ops 50000)
        (display "  ℹ️  CACHING OPTIONAL: Performance is moderate (10k-50k ops/sec).\n")
        (display "      Cache only if rendering is a bottleneck in profiling.\n"))
       (else
        (display "  ✅ CACHING NOT NEEDED: Performance is excellent (>50k ops/sec).\n")
        (display "      Current performance should be sufficient for most use cases.\n")))

      (display "\nNotes:\n")
      (display "  - These benchmarks measure pure rendering performance\n")
      (display "  - Real-world performance depends on template complexity and data\n")
      (display "  - Consider caching if the same templates are rendered repeatedly\n")
      (display "  - For dynamic content, focus on optimizing data fetching instead\n")
      (display "\n"))))

;; Run the benchmarks
(run-benchmarks)
