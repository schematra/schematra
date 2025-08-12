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
  chiccup)

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
(import schematra chiccup)
(get ("/" params)
     (ccup/html
      `[html
        [head
         [title "Hello Schematra!"]]
        [body
         [h1 "hello world!"]]]))
(schematra-install)
(schematra-start)
EXAMPLE
))

(define ex2 '(#<<EXAMPLE
(get ("/user/:name" params)
     (let ((name (cdr (assoc "name" params))))
       (ccup/html
        `[html
          [body
           [h1 ,(string-append "Hello," name "!")]
           [p "Welcome to Schematra"]]])))
EXAMPLE
))

(define ex3 '(#<<EXAMPLE
(ccup/html
 `[.container.mx-auto.p-4
   [h1.text-2xl.font-bold "My App"]
   [ul.list-disc.ml-6
    [li "Item 1"]
    [li "Item 2"]]
   [button.bg-blue-500.text-white.px-4.py-2.rounded
    "Click me"]])
EXAMPLE
))

(define ex4 '(#<<EXAMPLE
(static "/assets" "./public")
(static "/css" "./styles")
;; Now /assets/logo.png serves ./public/logo.png
;; and /css/main.css serves ./styles/main.css
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
        [li [a.text-teal-200.hover:text-white.transition-colors (("href" . "#documentation")) "Documentation"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (("href" . "#examples")) "Examples"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (("href" . "#api")) "API Reference"]]
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
     "A modern web framework for Scheme that combines simplicity with power. Build web applications with elegant Lisp syntax and modern tooling."]
    [.flex.flex-col.sm:flex-row.gap-3.sm:gap-4.justify-center.mb-8.sm:mb-12.px-4
     [a.bg-teal-600.text-white.px-6.sm:px-8.py-3.rounded-lg.font-semibold.hover:bg-teal-700.transition-colors.text-center
      (("href" . "#getting-started")) "Get Started"]
     [a.border.border-teal-300.text-teal-700.px-6.sm:px-8.py-3.rounded-lg.font-semibold.hover:bg-teal-50.transition-colors.text-center
      (("href" . "#documentation")) "Documentation"]]
    [.grid.grid-cols-1.md:grid-cols-3.gap-4.sm:gap-6.lg:gap-8.text-left.px-4
     [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
      [h3.text-lg.font-semibold.text-teal-900.mb-3 "Simple Syntax"]
      [p.text-teal-700 "Write clean, expressive web applications using Scheme's elegant syntax with modern web development patterns."]]
     [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
      [h3.text-lg.font-semibold.text-teal-900.mb-3 "Chiccup rendering"]
      [p.text-teal-700 "Build HTML with Lisp-style syntax inspired by Hiccup, building html feels natural in Scheme."]]
     [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
      [h3.text-lg.font-semibold.text-teal-900.mb-3 "Modern Features"]
      [p.text-teal-700 "Includes routing, rendering HTML, and database integration with Redis support out of the box."]]]
    
    [.mt-12.sm:mt-16.text-left.px-4
     [h2.text-2xl.sm:text-3xl.font-bold.text-teal-900.mb-6.sm:mb-8.text-center "Why Schematra?"]
     [.grid.grid-cols-1.md:grid-cols-2.gap-6.sm:gap-8.mb-8.sm:mb-12
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "🚀 Functional by Design"]
       [p.text-teal-700 "Built for Scheme developers who appreciate functional programming principles. No object-oriented complexity - just clean, composable functions."]]
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "⚡ Minimal Overhead"]
       [p.text-teal-700 "Unlike heavyweight frameworks, Schematra stays out of your way. Write web apps with the same elegance you'd write any Scheme program."]]
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "🎯 Sinatra-Inspired"]
       [p.text-teal-700 "Familiar routing patterns from Sinatra, adapted for Scheme. If you've used Sinatra, Flask, or Express, you'll feel right at home."]]
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "🔧 CHICKEN Scheme Power"]
       [p.text-teal-700 "Leverages CHICKEN Scheme's mature ecosystem and excellent C interop. Deploy anywhere CHICKEN runs - from embedded systems to cloud servers."]]]]
    
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
     
     [h2.text-2xl.sm:text-3xl.font-bold.text-teal-900.mb-6.sm:mb-8.text-center.mt-12 "Quick Start Examples"]
     [.grid.grid-cols-1.lg:grid-cols-2.gap-4.sm:gap-6.lg:gap-8
      ,(code-box "Hello World" ex1 "A minimal Schematra application that serves a hello world page.")
      ,(code-box "Dynamic Routes" ex2 "Handle dynamic URL parameters with ease using Schematra's routing system.")
      ,(code-box "Chiccup Templates" ex3 "Write HTML with Lisp syntax and CSS classes directly in the element selector.")
      ,(code-box "Static Files" ex4 "Serve static files like images, CSS, and JavaScript with simple directory mapping.")]]])

(static "/static" "./public")

(get ("/" params)
     (ccup/html (layout landing-page)))

(get ("/api-reference" _)
     (ccup/html '[h1 "not yet"]))

(schematra-install)
(schematra-start development?: #t nrepl?: #f)

