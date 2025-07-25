(import schematra sessions chiccup format intarweb)

(define (html-layout title body)
  (ccup/html
   `[html (("lang" . "en"))
      [head
       [meta (("charset" . "utf-8"))]
       [meta (("name" . "viewport") ("content" . "width=device-width, initial-scale=1"))]
       [title ,title]
       [script (("src" . "https://cdn.tailwindcss.com"))]]
      [body ,body]]))

(define welcome-page-content
  `[.min-h-screen.bg-gradient-to-br.from-purple-400.via-pink-500.to-red-500.flex.items-center.justify-center.p-4
    [.max-w-4xl.mx-auto.text-center.text-white
     [.mb-8
      [h1.text-6xl.font-bold.mb-4.animate-pulse "🤖 SillyBot AI"]
      [p.text-xl.mb-8.opacity-90 "The AI that thinks it's funnier than it actually is"]]
     [.bg-white.bg-opacity-20.backdrop-blur-lg.rounded-3xl.p-8.mb-8.shadow-2xl
      [h2.text-2xl.font-semibold.mb-4 "What SillyBot Can Do:"]
      [.grid.md:grid-cols-3.gap-6.text-left
       [.bg-white.bg-opacity-10.rounded-xl.p-4
        [.text-3xl.mb-2 "🎭"]
        [h3.font-bold.mb-2 "Tell Bad Jokes"]
        [p.text-sm.opacity-80 "Guaranteed to make you groan, not laugh"]]
       [.bg-white.bg-opacity-10.rounded-xl.p-4
        [.text-3xl.mb-2 "🔮"]
        [h3.font-bold.mb-2 "Predict Nothing"]
        [p.text-sm.opacity-80 "Our AI is 50% accurate, 100% of the time"]]
       [.bg-white.bg-opacity-10.rounded-xl.p-4
        [.text-3xl.mb-2 "🎨"]
        [h3.font-bold.mb-2 "Create Chaos"]
        [p.text-sm.opacity-80 "Turn your organized life into beautiful disorder"]]]]
     [.space-y-4
      [button.bg-yellow-400.hover:bg-yellow-300.text-black.font-bold.py-4.px-8.rounded-full.text-lg.transform.hover:scale-105.transition-all.duration-200.shadow-lg
       "Start Being Silly"]
      [p.text-sm.opacity-70 "Warning: May cause uncontrollable eye-rolling"]]]])

(define welcome-page
  (html-layout "SillyBot AI - The Silliest AI Ever" welcome-page-content))

;; testing middleware
(use-middleware! (session-middleware "foobar"))

(define (valid-token? header)
  (and (list? header)
       (= 1 (length header))
       (vector? (car header))
       (string=? (symbol->string (get-value (car header))) "bearer")
       (string=? (symbol->string (caar (get-params (car header)))) "secret")))

;; detail of the headers content: https://wiki.call-cc.org/eggref/5/intarweb#headers
(define (auth-middleware request params next)
  (let ((auth-header (header-contents 'authorization (request-headers request))))
    (if (and auth-header (valid-token? auth-header))
        ;; Continue to next middleware or route
        (next)
        ;; Return error response
        '(unauthorized "You don't belong here"))))

(use-middleware! auth-middleware)

(get "/"
     (lambda (request #!optional params)
       (let ((cookie-val (cookie-ref "test"))
	     (session-val (session-get "foo")))
	 (display (format "Cookie: ~A; session[foo]: ~A\n" cookie-val session-val)))
       (cookie-set! "test" "this is a test")
       (session-set! "foo" 42)
       welcome-page))

(define (lookup key alist)
  (let ((pair (assoc key alist)))
    (if pair
        (cdr pair)
        #f)))

(get "/users/:user-id/posts/:post-id"
     (lambda (req params)
       (let ((user-id (lookup "user-id" params))
             (post-id (lookup "post-id" params))
	     (q       (lookup 'kk params)))
	 (log-dbg "[DBG] params: ~A" params)
         (format "User: ~A, Post: ~A, q: ~A\n" user-id post-id q))))

(post "/test"
      (lambda (request params)
	(let ((body (request-body-string request))
	      (content-type (header-value 'content-type (request-headers request))))
	  (format "Body: ~A; content-type: ~A" body content-type))))
(get "/tw-demo"
     (lambda (req params)
       (ccup/html
	`[html
	  [head [script (("src" . "https://cdn.tailwindcss.com"))]]
	  [body.bg-gray-100.p-8 [h1.text-3xl.font-bold.text-blue-600 "Hello, Tailwind!"]]])))

(get "/htmx-demo"
     (lambda (req params)
       (ccup/html
	`[html
	  [head [script (("src" . "https://cdn.jsdelivr.net/npm/htmx.org@2.0.6/dist/htmx.min.js"))]]
	  [body
	   [button (("hx-get" . "/clicked") ("hx-target" . "#result")) "Click me!"]
	   [\#result]]])))

(get "/clicked"
     (lambda (req params)
       (ccup/html `[p "Button was clicked!"])))

(schematra-install)
(schematra-start development?: #t)
