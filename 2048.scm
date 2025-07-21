(load "schematra.scm")
(load "chiccup.scm")
(import schematra chiccup format)

;; Game state - 4x4 grid initialized with zeros
(define game-grid (make-vector 16 0))

;; Initialize game with two random tiles
(define (init-game!)
  (vector-fill! game-grid 0)
  (add-random-tile!)
  (add-random-tile!))

;; Add a random tile (2 or 4) to an empty position
(define (add-random-tile!)
  (let ((empty-positions (filter (lambda (i) (= (vector-ref game-grid i) 0))
                                (iota 16))))
    (when (not (null? empty-positions))
      (let ((pos (list-ref empty-positions (random (length empty-positions))))
            (value (if (< (random 10) 9) 2 4)))
        (vector-set! game-grid pos value)))))

;; Convert 1D index to 2D coordinates
(define (index->coords i)
  (list (quotient i 4) (remainder i 4)))

;; Convert 2D coordinates to 1D index
(define (coords->index row col)
  (+ (* row 4) col))

;; Get tile color class based on value
(define (tile-color value)
  (case value
    ((0) "bg-gray-200")
    ((2) "bg-yellow-100 text-gray-800")
    ((4) "bg-yellow-200 text-gray-800")
    ((8) "bg-orange-300 text-white")
    ((16) "bg-orange-400 text-white")
    ((32) "bg-red-400 text-white")
    ((64) "bg-red-500 text-white")
    ((128) "bg-yellow-400 text-white")
    ((256) "bg-yellow-500 text-white")
    ((512) "bg-yellow-600 text-white")
    ((1024) "bg-purple-500 text-white")
    ((2048) "bg-purple-600 text-white")
    (else "bg-black text-white")))

;; Render a single tile
(define (render-tile value)
  `[div (("class" . ,(string-append "w-16 h-16 rounded-lg flex items-center justify-center font-bold text-lg " (tile-color value))))
    ,(if (= value 0) "" (number->string value))])

;; Render the game grid
(define (render-grid)
  `[.grid.grid-cols-4.gap-2.bg-gray-300.p-4.rounded-lg
    ,@(map (lambda (i) (render-tile (vector-ref game-grid i)))
           (iota 16))])

;; Move tiles left in a row
(define (move-row-left row)
  (let* ((non-zero (filter (lambda (x) (> x 0)) row))
         (merged '())
         (i 0))
    (let loop ((tiles non-zero) (result '()) (skip-next? #f))
      (cond
        ((null? tiles) 
         (append (reverse result) (make-list (- 4 (length result)) 0)))
        ((or skip-next? (null? (cdr tiles)))
         (loop (cdr tiles) (cons (car tiles) result) #f))
        ((= (car tiles) (cadr tiles))
         (loop (cddr tiles) (cons (* 2 (car tiles)) result) #f))
        (else
         (loop (cdr tiles) (cons (car tiles) result) #f))))))

;; Move tiles in specified direction
(define (move-tiles direction)
  (case direction
    ((left)
     (do ((row 0 (+ row 1)))
         ((= row 4))
       (let* ((start (* row 4))
              (row-values (map (lambda (i) (vector-ref game-grid (+ start i))) (iota 4)))
              (new-row (move-row-left row-values)))
         (do ((col 0 (+ col 1)))
             ((= col 4))
           (vector-set! game-grid (+ start col) (list-ref new-row col))))))
    ((right)
     (do ((row 0 (+ row 1)))
         ((= row 4))
       (let* ((start (* row 4))
              (row-values (reverse (map (lambda (i) (vector-ref game-grid (+ start i))) (iota 4))))
              (new-row (reverse (move-row-left row-values))))
         (do ((col 0 (+ col 1)))
             ((= col 4))
           (vector-set! game-grid (+ start col) (list-ref new-row col))))))
    ((up)
     (do ((col 0 (+ col 1)))
         ((= col 4))
       (let* ((col-values (map (lambda (row) (vector-ref game-grid (coords->index row col))) (iota 4)))
              (new-col (move-row-left col-values)))
         (do ((row 0 (+ row 1)))
             ((= row 4))
           (vector-set! game-grid (coords->index row col) (list-ref new-col row))))))
    ((down)
     (do ((col 0 (+ col 1)))
         ((= col 4))
       (let* ((col-values (reverse (map (lambda (row) (vector-ref game-grid (coords->index row col))) (iota 4))))
              (new-col (reverse (move-row-left col-values))))
         (do ((row 0 (+ row 1)))
             ((= row 4))
           (vector-set! game-grid (coords->index row col) (list-ref new-col row))))))))

;; Game page content
(define game-2048-content
  `[.min-h-screen.bg-gradient-to-br.from-blue-400.to-purple-600.flex.items-center.justify-center.p-4
    [.text-center
     [h1.text-4xl.font-bold.text-white.mb-8 "🎮 2048 Game"]
     [div (("id" . "game-container"))
      ,(render-grid)]
     [.mt-8.space-x-4
      [button.bg-green-500.hover:bg-green-600.text-white.font-bold.py-2.px-4.rounded
               (("hx-post" . "/2048/new-game")
                ("hx-target" . "#game-container")
                ("hx-swap" . "innerHTML"))
       "New Game"]
      [.mt-4.text-white
       [p "Use arrow keys or buttons to move tiles"]
       [.grid.grid-cols-2.gap-2.mt-4.max-w-xs.mx-auto
        [button.bg-blue-500.hover:bg-blue-600.text-white.font-bold.py-2.px-4.rounded
                 (("hx-post" . "/2048/move/up")
                  ("hx-target" . "#game-container")
                  ("hx-swap" . "innerHTML"))
         "↑"]
        [div]
        [button.bg-blue-500.hover:bg-blue-600.text-white.font-bold.py-2.px-4.rounded
                 (("hx-post" . "/2048/move/left")
                  ("hx-target" . "#game-container")
                  ("hx-swap" . "innerHTML"))
         "←"]
        [button.bg-blue-500.hover:bg-blue-600.text-white.font-bold.py-2.px-4.rounded
                 (("hx-post" . "/2048/move/right")
                  ("hx-target" . "#game-container")
                  ("hx-swap" . "innerHTML"))
         "→"]
        [div]
        [button.bg-blue-500.hover:bg-blue-600.text-white.font-bold.py-2.px-4.rounded
                 (("hx-post" . "/2048/move/down")
                  ("hx-target" . "#game-container")
                  ("hx-swap" . "innerHTML"))
         "↓"]]]]]])

;; Routes
(get "/2048"
     (lambda (request #!optional params)
       (init-game!)
       (html-layout "2048 Game" game-2048-content)))

(post "/2048/new-game"
      (lambda (request #!optional params)
        (init-game!)
        (ccup/html (render-grid))))

(post "/2048/move/:direction"
      (lambda (request params)
        (let ((direction (string->symbol (lookup "direction" params))))
          (move-tiles direction)
          (add-random-tile!)
          (ccup/html (render-grid)))))

(schematra-install)
(schematra-start)
