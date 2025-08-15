(import
 srfi-18        ;; threads
 chicken.string ;; conc
 intarweb       ;; read-urlencoded-request-data
 spiffy         ;; current-response?
 format
 schematra
 schematra-body-parser
 chiccup
 )

;; used in the post
(use-middleware! (body-parser-middleware))

(define broadcast-mutex (make-mutex))
(define broadcast-condition (make-condition-variable))
(define last-message #f)
(define message-id 0)

(define (broadcast-message! msg)
  (mutex-lock! broadcast-mutex)
  (set! last-message msg)
  (set! message-id (+ message-id 1))
  (condition-variable-broadcast! broadcast-condition)
  (mutex-unlock! broadcast-mutex))

(define (send-form)
  `[form.flex.gap-2
    ({"hx-post" . "/send"}
     {"hx-swap" . "none"}
     {"hx-on::after-request" . "this.reset()"})
    [input.flex-1.px-3.py-2.border.border-gray-300.rounded-md.focus:outline-none.focus:ring-2.focus:ring-blue-500.focus:border-transparent
     ({"type" . "text"}
      {"name" . "message"}
      {"placeholder" . "Type your message..."})]
    [button.px-4.py-2.bg-blue-600.text-white.rounded-md.hover:bg-blue-700.focus:outline-none.focus:ring-2.focus:ring-blue-500.focus:ring-offset-2.transition-colors
     "Send"]])

;; instructions:
;; open one browser pointing to this app, then use another browser or an incognito session and open the app.
;; You should be able to chat between the different tabs/windows.
;; By default spiffy allows up to 1024 simultaneous connections. You're using one on each SSE route.
;; To change the amount of max connections you can use the parameter `max-connections` (see: https://wiki.call-cc.org/eggref/5/spiffy#configuration-parameters)
(get ("/")
     (ccup/html
      `[html
        [head
         [script ({"src" . "https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js"})]
         [script ({"src" . "https://cdn.jsdelivr.net/npm/htmx-ext-sse@2.2.2"})]
         [script (("src" . "https://cdn.tailwindcss.com"))]]
        [body.bg-gray-100.min-h-screen
         ({"hx-ext" . "sse"})
         [.max-w-2xl.mx-auto.p-6
          [.bg-white.rounded-lg.shadow-lg.overflow-hidden
           [.bg-blue-600.text-white.p-4
            [h1.text-xl.font-bold "Live Chat"]]
           [.p-4
            [\#history.space-y-2.mb-4.h-64.overflow-y-auto.border.border-gray-200.rounded.p-3.bg-gray-50
             ({"sse-connect" . "/chatroom"}
              {"sse-swap" . "message"}
              {"hx-swap" . "beforeend"})]]
           ,(send-form)]]]]))

(post ("/send")
      (let ((msg (alist-ref 'message (current-params))))
        (when msg (broadcast-message! msg))
        ""))

(define (render-msg msg)
  (ccup/html
   `[.p-2.my-2.bg-white.rounded.border-l-4.border-blue-500.shadow-sm
     [span.text-gray-800 ,msg]]))

(sse "/chatroom"
     (lambda ()
       (let ((last-seen-id 0))
         (let loop ()
           (mutex-lock! broadcast-mutex)

           ;; confirm that we should send a message
           (let wait-for-message ()
             (when (<= message-id last-seen-id)
               (mutex-unlock! broadcast-mutex broadcast-condition)
               (mutex-lock! broadcast-mutex)
               (wait-for-message)))

           ;; send the message
           (let ((msg-to-send (render-msg last-message))
                 (current-msg-id message-id))
             (mutex-unlock! broadcast-mutex)
             (when (write-sse-data msg-to-send event: "message")
               (set! last-seen-id current-msg-id)
               (loop)))))
       ;; in theory we should never reach this point
       "done"))

(schematra-install)
(schematra-start development?: #t)
