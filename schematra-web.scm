;; Schematra - a very simple web framework for scheme inspired in
;; Sinatra
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

(import
  chicken.port
  chicken.string
  chicken.pretty-print
  srfi-13
  schematra
  schematra-body-parser
  chiccup)

(use-middleware! (body-parser-middleware))

(define (add-google-font  name #!optional (weight 400))
  `[link ((rel . "stylesheet")
	  (href . ,(conc "https://fonts.googleapis.com/css2?family=" name "&display=swap")))])

(define (add-style)
  `[style ".cookie-regular {
  font-family: "Cookie", cursive;
  font-weight: 400;
  font-style: normal;
}"])

(define ex1 '(#<<EXAMPLE
;; Complete web app in just a few lines
(import schematra chiccup sessions)

(use-middleware! (session-middleware "secret-key"))

(get ("/")
     (let ((user (session-get "username")))
       (if user
           (ccup/html `[h1 ,(format "Welcome back, ~a!" user)])
           (redirect "/login"))))

(get ("/login")
     (ccup/html 
      `[form ((method . "POST") (action . "/login"))
        [input ((type . "text") (name . "username") 
                (placeholder . "Username"))]
        [button "Login"]]))

(post ("/login")
      (let ((username (alist-ref "username" (current-params) equal?)))
        (session-set! "username" username)
        (redirect "/")))

(schematra-install)
(schematra-start)
EXAMPLE
))

(define ex2 '(#<<EXAMPLE
;; Powerful middleware for cross-cutting concerns
(define (auth-middleware next)
  (let ((token (cdr (assoc 'token (current-params)))))
    (if (and token (valid-token? token))
        (next)  ; Continue to route handler
        '(unauthorized "Invalid token"))))

(define (logging-middleware next)
  (let* ((request (current-request))
         (method (request-method request))
         (path (uri-path (request-uri request))))
    (log-dbg "~A ~A" method path)
    (next)))

(use-middleware! logging-middleware)
(use-middleware! auth-middleware)

;; Now all routes are logged and require auth
(get ("/api/users")
     '(ok "{\"users\": [...]}" 
          ((content-type application/json))))
EXAMPLE
))

(define ex3 '(#<<EXAMPLE
;; Chiccup: HTML that looks like your data
(define (render-todo todo)
  `[.todo-item.p-4.border.rounded
    [h3.font-bold ,(todo-title todo)]
    [p.text-gray-600 ,(todo-description todo)]
    [.flex.gap-2.mt-2
     [button.bg-green-500.text-white.px-3.py-1.rounded
      ((onclick . ,(format "completeTodo(~a)" (todo-id todo))))
      "Complete"]
     [button.bg-red-500.text-white.px-3.py-1.rounded
      ((onclick . ,(format "deleteTodo(~a)" (todo-id todo))))
      "Delete"]]])

(get ("/todos")
     (let ((todos (get-user-todos (session-get "user-id"))))
       (ccup/html
        `[.container.mx-auto.p-6
          [h1.text-2xl.mb-4 "My Todos"]
          ,@(map render-todo todos)])))
EXAMPLE
))

(define ex4 '(#<<EXAMPLE
;; JSON APIs made effortless
(post ("/api/users")
      (let* ((params (current-params))
	     (name (alist-ref "name" params equal?))
             (email (alist-ref "email" params equal?)))
        (if (and name email (valid-email? email))
            (let ((user-id (create-user! name email)))
              (send-json-response
                'created
                `((id . ,user-id)
                  (message . "User created")
                  (email . ,email))))
            (send-json-response
              'bad-request
              '((error . "Invalid name or email")
                (required . ("name" "email")))))))

(get ("/api/users")
     (let ((users (get-all-users)))
       (send-json-response 
         'ok 
         `((users . ,(map user->alist users))
           (count . ,(length users))))))
EXAMPLE
))

(define (code-box title example subtext)
  `[.bg-white.p-4.sm:p-6.rounded-lg.shadow-sm.border.border-teal-100
    [h3.text-base.sm:text-lg.font-semibold.text-teal-900.mb-3.sm:mb-4 ,title]
    [pre.bg-teal-50.p-3.sm:p-4.rounded.text-xs.sm:text-sm.overflow-x-auto
     [code.language-scheme ,(car example)]]
    [p.text-sm.sm:text-base.text-teal-700.mt-2.sm:mt-3 ,subtext]])

(define (footer)
  `[footer.bg-teal-800.text-white.mt-12.sm:mt-20
    [.max-w-4xl.mx-auto.px-4.sm:px-6.py-8.sm:py-12
     [.grid.grid-cols-1.sm:grid-cols-2.md:grid-cols-3.gap-6.sm:gap-8
      [div
       [h3.text-lg.font-semibold.mb-4 "Schematra"]
       [p.text-teal-200.mb-4 "A modern web framework for Scheme developers who value simplicity and elegance."]
       [p.text-sm.text-teal-300 "Built with ❤ and lots of parentheses"]]
      [div
       [h3.text-lg.font-semibold.mb-4 "Resources"]
       [ul.space-y-2
        [li [a.text-teal-200.hover:text-white.transition-colors (("href" . "https://github.com/schematra/schematra/blob/main/docs/docs.md")) "Documentation"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (("href" . "#examples")) "Examples"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (("href" . "/api")) "API Reference"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (("href" . "#community")) "Community"]]]]
      [div
       [h3.text-lg.font-semibold.mb-4 "Connect"]
       [ul.space-y-2
        [li [a.text-teal-200.hover:text-white.transition-colors (("href" . "https://github.com/schematra/schematra")) "GitHub"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (("href" . "https://github.com/schematra/schematra/issues")) "Report Issues"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (("href" . "https://github.com/schematra/schematra?tab=readme-ov-file#contributing")) "Contribute"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (("href" . "https://raw.githubusercontent.com/schematra/schematra/refs/heads/main/LICENSE.md")) "License"]]]]]
     [.border-t.border-teal-700.mt-6.sm:mt-8.pt-6.sm:pt-8.text-center
      [p.text-teal-300.text-xs.sm:text-sm "© 2025 Rolando Abarca. Released under the GNU General Public License v3.0"]]]])

(define (layout page)
  `[html ((lang . "en-US"))
	 [head
	  [meta ((charset . "utf-8"))]
	  [meta ((name . "viewport") (content . "width=device-width, initial-scale=1"))]
	  [title "Schematra - Scheme Web Framework"]
	  [script ((src . "https://cdn.tailwindcss.com"))]
	  [script ((src . "https://unpkg.com/htmx.org@1.9.10"))]
	  [link ((rel . "preconnect") (href . "https://fonts.googleapis.com"))]
	  [link ((rel . "preconnect") (href . "https://fonts.gstatic.com"))]
	  ;; add highlight.js
	  [link ((rel . "stylesheet") (href . "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/default.min.css"))]
	  ,(add-google-font "Cookie")
	  ,(add-style)]
	 [body.bg-teal-50.min-h-screen
	  [.flex.flex-col.min-h-screen
	   [main.flex-1.py-8.px-4.sm:px-6.lg:px-8
	    ,page]]
	  ,(footer)
	  [script ((src . "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js"))]
	  [script ((src . "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/scheme.min.js"))]
	  [script "hljs.highlightAll();"]]])

(define landing-page
  `[.max-w-4xl.mx-auto.text-center
    [img.mx-auto.mb-6.h-16.w-auto.sm:h-20.lg:h-24 (("src" . "/static/logo.png") ("alt" . "Schematra Logo"))]
    [h1.text-3xl.sm:text-4xl.lg:text-5xl.font-bold.text-teal-900.mb-4.sm:mb-6.cookie-regular "(Schematra)"]
    [p.text-lg.sm:text-xl.text-teal-700.mb-6.sm:mb-8.max-w-2xl.mx-auto.px-4 
     "The web framework that makes complex things simple. Authentication in 3 lines. Middleware that's actually composable. HTML that looks like your data. Built for developers who value elegance over boilerplate."]
    [.flex.flex-col.sm:flex-row.gap-3.sm:gap-4.justify-center.mb-8.sm:mb-12.px-4
     [a.bg-teal-600.text-white.px-6.sm:px-8.py-3.rounded-lg.font-semibold.hover:bg-teal-700.transition-colors.text-center
      (("href" . "#getting-started")) "Get Started"]
     [a.border.border-teal-300.text-teal-700.px-6.sm:px-8.py-3.rounded-lg.font-semibold.hover:bg-teal-50.transition-colors.text-center
      (("href" . "https://github.com/schematra/schematra/blob/main/docs/docs.md")) "Documentation"]]
    [.grid.grid-cols-1.md:grid-cols-3.gap-4.sm:gap-6.lg:gap-8.text-left.px-4
     [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
      [h3.text-lg.font-semibold.text-teal-900.mb-3 "Zero Config Sessions"]
      [p.text-teal-700 "Cookie-based sessions work immediately. No setup, no database, no complexity. " [code.text-sm.bg-gray-100.px-1.rounded "session-set!"] " and you're done."]]
     [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
      [h3.text-lg.font-semibold.text-teal-900.mb-3 "HTML as Data"]
      [p.text-teal-700 "Write " [code.text-sm.bg-gray-100.px-1.rounded "`[.card [h1 \"Title\"]]"] " instead of wrestling with template engines. Your HTML structure mirrors your data structure."]]
     [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
      [h3.text-lg.font-semibold.text-teal-900.mb-3 "3-Line Middleware"]
      [p.text-teal-700 "Real middleware that composes. Write a function, call " [code.text-sm.bg-gray-100.px-1.rounded "use-middleware!"] ", done. No decorators, no magic, just functions."]]]
    
    [.mt-12.sm:mt-16.text-left.px-4
     [h2.text-2xl.sm:text-3xl.font-bold.text-teal-900.mb-6.sm:mb-8.text-center "Stop Fighting Your Framework"]
     [.grid.grid-cols-1.md:grid-cols-2.gap-6.sm:gap-8.mb-8.sm:mb-12
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "🚀 Express in 50 Lines vs Schematra in 15"]
       [p.text-teal-700 "What takes Express 50+ lines of boilerplate (sessions, middleware, auth) takes Schematra 15 lines of clean, readable Scheme. Zero magic, maximum clarity."]]
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "⚡ Templates That Don't Lie"]
       [p.text-teal-700 "Forget Handlebars, Jinja, and ERB. Your HTML structure is your data structure. No context switching, no template syntax to learn, no surprises."]]
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "🎯 Middleware Done Right"]
       [p.text-teal-700 "Flask decorators? Express app.use()? Schematra middleware is just " [code.text-sm.bg-gray-100.px-1.rounded "(lambda (params next) ...)"] ". Functions all the way down."]]
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "🔧 Deploy Anywhere"]
       [p.text-teal-700 "Compile to a single binary. No Python virtual environments, no Node.js versions, no Docker complexity. If it runs C, it runs Schematra."]]]]
    
    [.mt-12.sm:mt-16.text-left.px-4
     [h2.text-2xl.sm:text-3xl.font-bold.text-teal-900.mb-6.sm:mb-8.text-center#getting-started "Getting Started"]
     [.bg-white.p-6.rounded-lg.shadow-sm.mb-8.border.border-teal-100
      [h3.text-lg.font-semibold.text-teal-900.mb-4 "Installation"]
      [ol.list-decimal.list-inside.space-y-3.text-teal-700
       [li "Clone the Schematra repository:"
	   [pre.bg-gray-100.p-3.rounded.mt-2.text-sm.overflow-x-auto
	    [code "git clone https://github.com/schematra/schematra.git"]]]
       [li "Install Schematra and its dependencies:"
	   [pre.bg-gray-100.p-3.rounded.mt-2.text-sm.overflow-x-auto
	    [code "cd schematra && chicken-install"]]]
       [li "Create your first app by creating a new file (e.g., " [code.bg-gray-100.px-2.py-1.rounded "app.scm"] ") and start coding!"]]]
     
     [h2.text-2xl.sm:text-3xl.font-bold.text-teal-900.mb-6.sm:mb-8.text-center.mt-12 "Try Chiccup Live"]
     [.bg-white.p-4.sm:p-6.rounded-lg.shadow-sm.border.border-teal-100.mb-8.sm:mb-12
      [.grid.grid-cols-1.lg:grid-cols-2.gap-4
       [div
        [.flex.justify-between.items-center.mb-3
         [h3.text-lg.font-semibold.text-teal-900 "Chiccup Code"]
         [.space-x-2
          [button.text-sm.bg-teal-100.text-teal-700.px-3.py-1.rounded.hover:bg-teal-200
           ((hx-get . "/playground/card") (hx-target . "#chiccup-input") (hx-swap . "outerHTML")) "Card"]
          [button.text-sm.bg-teal-100.text-teal-700.px-3.py-1.rounded.hover:bg-teal-200
           ((hx-get . "/playground/form") (hx-target . "#chiccup-input") (hx-swap . "outerHTML")) "Form"]
          [button.text-sm.bg-teal-100.text-teal-700.px-3.py-1.rounded.hover:bg-teal-200
           ((hx-get . "/playground/list") (hx-target . "#chiccup-input") (hx-swap . "outerHTML")) "List"]]]
        [form ((hx-post . "/playground/render") (hx-target . "#html-preview"))
         [textarea.w-full.h-80.p-3.border.rounded.font-mono.text-sm.resize-none.focus:outline-none.focus:ring-2.focus:ring-teal-500#chiccup-input
          ((name . "chiccup") (placeholder . "Try editing the Chiccup code...")
           (hx-get . "/playground/card") (hx-trigger . "load") (hx-swap . "outerHTML") (hx-target . "#chiccup-input"))]
         [.mt-3
          [button.bg-teal-600.text-white.px-4.py-2.rounded.hover:bg-teal-700.font-semibold
           ((type . "submit")) "Render Preview"]]]]
       [div
        [h3.text-lg.font-semibold.text-teal-900.mb-3 "Live Preview"]
        [.w-full.h-80.p-3.border.rounded.bg-gray-50.overflow-auto
         [\#html-preview
          [.text-center.text-gray-500.py-8 "Click 'Render Preview' to see the output"]]]]]
      [p.text-sm.text-teal-600.mt-4.text-center
       "✨ This playground uses real Schematra server-side rendering - what you see is what you get!"]]
     
     [h2.text-2xl.sm:text-3xl.font-bold.text-teal-900.mb-6.sm:mb-8.text-center.mt-12 "See More Examples"]
     [.space-y-6.sm:space-y-8
      ,(code-box "Complete Web App" ex1 "A full authentication flow with sessions, forms, and redirects - all in a few lines of clean Scheme code.")
      ,(code-box "Middleware Magic" ex2 "Compose powerful middleware for logging, authentication, and more. Each middleware is just a simple function.")
      ,(code-box "Chiccup Power" ex3 "HTML templates that look like your data structures. Map over lists, compose functions, and build UIs functionally.")
      ,(code-box "JSON APIs Made Easy" ex4 "Write APIs that work with data, not strings. send-json-response handles serialization and headers automatically.")]]])

(static "/static" "./public")

;; Playground examples
(define playground-examples 
  '((card . "[.max-w-md.mx-auto.bg-white.rounded-xl.shadow-md.overflow-hidden
  [.p-6
    [h1.text-2xl.font-bold.text-gray-900 \"Beautiful Card\"]
    [p.text-gray-600.mt-2 \"This card was built with Chiccup syntax.\"]
    [.mt-4
      [button.bg-teal-600.text-white.px-4.py-2.rounded.hover:bg-teal-700
        \"Learn More\"]]]]")
    (form . "[.max-w-md.mx-auto.bg-white.p-6.rounded-lg.shadow-md
  [h2.text-xl.font-bold.mb-4.text-gray-900 \"Contact Form\"]
  [form ((action . \"/submit\") (method . \"POST\"))
    [.mb-4
      [label.block.text-gray-700.text-sm.font-bold.mb-2 \"Name\"]
      [input.shadow.border.rounded.w-full.py-2.px-3.text-gray-700.focus:outline-none.focus:shadow-outline 
        ((type . \"text\") (name . \"name\") (placeholder . \"Your Name\"))]]
    [.mb-4
      [label.block.text-gray-700.text-sm.font-bold.mb-2 \"Email\"]
      [input.shadow.border.rounded.w-full.py-2.px-3.text-gray-700.focus:outline-none.focus:shadow-outline
        ((type . \"email\") (name . \"email\") (placeholder . \"your@email.com\"))]]
    [button.bg-blue-500.hover:bg-blue-700.text-white.font-bold.py-2.px-4.rounded.focus:outline-none.focus:shadow-outline
      \"Submit\"]]]")
    (list . "[.max-w-md.mx-auto.bg-white.p-6.rounded-lg.shadow-md
  [h2.text-xl.font-bold.mb-4.text-gray-900 \"Todo List\"]
  [ul.divide-y.divide-gray-200
    [li.py-4.flex.items-center.space-x-3
      [input.h-4.w-4.text-teal-600.rounded ((type . \"checkbox\") (checked . \"true\"))]
      [span.flex-1.text-gray-900 \"Learn Chiccup syntax\"]]
    [li.py-4.flex.items-center.space-x-3
      [input.h-4.w-4.text-teal-600.rounded ((type . \"checkbox\"))]
      [span.flex-1.text-gray-500 \"Build amazing web apps\"]]
    [li.py-4.flex.items-center.space-x-3
      [input.h-4.w-4.text-teal-600.rounded ((type . \"checkbox\"))]
      [span.flex-1.text-gray-500 \"Deploy with Schematra\"]]]
  [.mt-4
    [button.bg-green-500.hover:bg-green-700.text-white.font-bold.py-2.px-4.rounded
      \"Add New Todo\"]]]]")))

;; Playground endpoints
(get ("/playground/:example")
     (let* ((example-name (alist-ref "example" (current-params) equal?))
            (example-code (alist-ref (string->symbol example-name) playground-examples)))
       (ccup/html
        (if example-code
            `[textarea.w-full.h-80.p-3.border.rounded.font-mono.text-sm.resize-none.focus:outline-none.focus:ring-2.focus:ring-teal-500#chiccup-input
              ((name . "chiccup") (placeholder . "Try editing the Chiccup code..."))
              ,example-code]
            `[div#chiccup-input.w-full.h-80.p-3.border.rounded.bg-red-50.text-red-600.flex.items-center.justify-center
              "Example not found"]))))

(post ("/playground/render")
      ;; got the 'chiccup param from the body, using the body-parser middleware
      (let ((chiccup-code (alist-ref 'chiccup (current-params))))
        (if chiccup-code
            (let ((cleaned-code (string-trim chiccup-code)))
              (condition-case
               (ccup/html (with-input-from-string cleaned-code read))
               (e () (ccup/html 
                      `[.text-red-600.p-4.bg-red-50.rounded.border.border-red-200
                        [h4.font-bold "Syntax Error"]
                        [p "Invalid Chiccup syntax. Please check your brackets and formatting."]]))))
            (ccup/html `[p "No code provided"]))))

(get ("/")
     (ccup/html (layout landing-page)))

(get ("/api")
     (redirect "https://github.com/schematra/schematra"))

(schematra-install)
(schematra-start development?: #t nrepl?: #f)

