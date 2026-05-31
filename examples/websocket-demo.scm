(import
 srfi-18
 srfi-1
 chicken.string
 chicken.random
 schematra
 schematra.ws
 chiccup)

(define clients-mutex (make-mutex))
(define clients '())

(define adjectives
  '(
    "Brave" "Cosmic" "Dapper" "Electric" "Fuzzy" "Gentle" "Lucky" "Neon"
    "Quiet" "Rapid" "Sunny" "Tiny"))

(define animals
  '(
    "Badger" "Capybara" "Dragonfly" "Fox" "Gecko" "Heron" "Mantis" "Otter"
    "Panda" "Raven" "Salmon" "Turtle"))

(define (pick items)
  (list-ref items (pseudo-random-integer (length items))))

(define (random-username)
  (conc (pick adjectives) (pick animals) (pseudo-random-integer 1000)))

(define (with-clients proc)
  (mutex-lock! clients-mutex)
  (dynamic-wind
    void
    proc
    (lambda () (mutex-unlock! clients-mutex))))

(define (add-client! ws username)
  (with-clients
   (lambda ()
     (set! clients (cons (cons ws username) clients)))))

(define (remove-client! ws)
  (with-clients
   (lambda ()
     (set! clients (filter (lambda (client) (not (eq? (car client) ws))) clients)))))

(define (client-name ws)
  (with-clients
   (lambda ()
     (let ((client (find (lambda (client) (eq? (car client) ws)) clients)))
       (and client (cdr client))))))

(define (broadcast! message)
  (let ((snapshot (with-clients (lambda () clients))))
    (for-each
     (lambda (client)
       (parameterize ((current-websocket (car client)))
         (condition-case
           (send-text message)
           (exn () (remove-client! (car client))))))
     snapshot)))

(with-schematra-app (schematra/make-app)
  (lambda ()
    (static "/assets" ".")

    (get "/"
       (ccup->html
        `[html
           [head
            [title "Schematra WebSocket Demo"]
            [script (@ (src "https://cdn.tailwindcss.com"))]
            [script (@ (src "/assets/websocket-demo.js") (defer))]]
          [body.bg-slate-950.text-slate-100.min-h-screen
           [.max-w-2xl.mx-auto.p-6.space-y-4
            [header.space-y-2
             [p.text-sm.uppercase.tracking-widest.text-cyan-300 "Schematra"]
             [h1.text-4xl.font-bold "WebSocket Chat"]
             [p.text-slate-300 "Open this page in multiple tabs and send messages between them."]]
             [.rounded-xl.border.border-slate-700.bg-slate-900.shadow-xl.overflow-hidden
               [div.p-4.border-b.border-slate-700.flex.items-center.justify-between
                [span "Status"]
                [span.text-sm.text-amber-300
                 (@ (id "status"))
                 "connecting"]]
              [div.p-4
               [div.h-80.overflow-y-auto.space-y-2.text-sm
                (@ (id "messages"))]]
              [form.p-4.border-t.border-slate-700.flex.items-center.gap-3
               (@ (id "form"))
               [input.flex-1.min-w-0.bg-slate-800.text-slate-100.placeholder-slate-500.border.border-slate-600.rounded-lg.px-3.py-2.outline-none.focus:ring-2.focus:ring-cyan-400.focus:border-cyan-400
                (@ (id "message")
                   (type "text")
                   (autocomplete "off")
                   (placeholder "Type a message..."))]
                [button.bg-cyan-400.text-slate-950.font-semibold.rounded-lg.px-4.py-2.h-10.leading-none.hover:bg-cyan-300
                 (@ (type "submit"))
                "Send"]]]]]]))

    (websocket "/ws"
    (on-open
     (let ((ws (current-websocket))
           (username (random-username)))
       (add-client! ws username)
       (send-text (conc "You are " username))
       (broadcast! (conc username " joined the chat"))))

    (on-text message
     (cond
      ((string=? message "/fail")
       (error 'websocket-demo "Intentional WebSocket message failure"))
      ((string=? message "/kick")
       ;; Demonstrate force-close from outside the handler context. Each
       ;; client connection is closed with a custom code/reason; their
       ;; on-close cleanup still runs because close-websocket! tears down
       ;; the read loop.
       (let ((me (current-websocket)))
         (broadcast! (conc "[" (or (client-name me) "Someone") "] kicked everyone"))
         (for-each
          (lambda (entry)
            (let ((other (car entry)))
              (unless (eq? other me)
                (close-websocket! 1000 "kicked" other))))
          (with-clients (lambda () clients)))))
      ((string=? message "/leave")
       ;; Graceful in-handler close: the loop will run on-close on next iter.
       (close-websocket! 1000 "bye"))
      (else
       (let ((username (or (client-name (current-websocket)) "Someone")))
         (broadcast! (conc "[" username "] " message))))))

    (on-close code reason
     (let* ((ws (current-websocket))
            (username (or (client-name ws) "A client")))
       (remove-client! ws)
       (broadcast! (conc username " left the chat"))))

    (on-error exn
     (let ((username (or (client-name (current-websocket)) "A client")))
       (broadcast! (conc "[server] " username " triggered an error: " (->string exn))))))

    (schematra-install)
    (schematra-start)))
