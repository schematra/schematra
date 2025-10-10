(define (add-google-font  name #!optional (weight 400))
  `[link (@ (rel "stylesheet")
            (href ,(conc "https://fonts.googleapis.com/css2?family=" name "&display=swap")))])

(define (add-style)
  `[style ".cookie-regular {
  font-family: \"Cookie\", cursive;
  font-weight: 400;
  font-style: normal;
}

/* Override prose styles for code blocks to work with highlight.js */
.prose pre {
  background-color: transparent !important;
  color: rgb(36, 41, 46) !important;
  padding: 0 !important;
}

.prose pre code {
  background-color: transparent !important;
  color: inherit !important;
  font-size: inherit !important;
  padding: 0 !important;
}"])


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
        [li [a.text-teal-200.hover:text-white.transition-colors (@ (href "https://github.com/schematra/schematra/blob/main/docs/docs.md")) "Documentation"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (@ (href "/blog")) "Blog"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (@ (href "#see-more-examples")) "Examples"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (@ (href "/api")) "API Reference"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (@ (href "#community")) "Community"]]]]
      [div
       [h3.text-lg.font-semibold.mb-4 "Connect"]
       [ul.space-y-2
        [li [a.text-teal-200.hover:text-white.transition-colors (@ (href "https://github.com/schematra/schematra")) "GitHub"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (@ (href "https://github.com/schematra/schematra/issues")) "Report Issues"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (@ (href "https://github.com/schematra/schematra?tab=readme-ov-file#contributing")) "Contribute"]]
        [li [a.text-teal-200.hover:text-white.transition-colors (@ (href "https://raw.githubusercontent.com/schematra/schematra/refs/heads/main/LICENSE.md")) "License"]]]]]
     [.border-t.border-teal-700.mt-6.sm:mt-8.pt-6.sm:pt-8.text-center
      [p.text-teal-300.text-xs.sm:text-sm "© 2025 Rolando Abarca. Released under BSD-3-Clause License - Schematra logo released under CC BY-NC 4.0"]]]])

(define (layout page #!key (meta-title #f) (meta-description #f) (meta-image #f) (meta-url #f) (meta-type "website"))
  (let ((title (or meta-title "Schematra - Scheme Web Framework"))
        (description (or meta-description "A modern web framework for Scheme developers who value simplicity and elegance."))
        (image (or meta-image "https://s3-us-west-1.amazonaws.com/assets.schematra.com/public/images/schematra-og.png"))
        (url (or meta-url "https://schematra.com")))
    `[html (@ (lang "en-US"))
           [head
            [meta (@ (charset "utf-8"))]
            [meta (@ (name "viewport") (content "width=device-width, initial-scale=1"))]
            [title ,title]
            [meta (@ (name "description") (content ,description))]
            ;; Open Graph tags
            [meta (@ (property "og:title") (content ,title))]
            [meta (@ (property "og:description") (content ,description))]
            [meta (@ (property "og:image") (content ,image))]
            [meta (@ (property "og:url") (content ,url))]
            [meta (@ (property "og:type") (content ,meta-type))]
            ;; Twitter Card tags
            [meta (@ (name "twitter:card") (content "summary_large_image"))]
            [meta (@ (name "twitter:title") (content ,title))]
            [meta (@ (name "twitter:description") (content ,description))]
            [meta (@ (name "twitter:image") (content ,image))]
            [script (@ (src "https://cdn.tailwindcss.com?plugins=typography"))]
            [script (@ (src "https://unpkg.com/htmx.org@1.9.10"))]
            [link (@ (rel "preconnect") (href "https://fonts.googleapis.com"))]
            [link (@ (rel "preconnect") (href "https://fonts.gstatic.com"))]
            ;; add highlight.js with a lighter theme
            [link (@ (rel "stylesheet") (href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/github.min.css"))]
            ,(add-google-font "Cookie")
            ,(add-style)]
         [body.bg-teal-50.min-h-screen
          [.flex.flex-col.min-h-screen
           [main.flex-1.py-8.px-4.sm:px-6.lg:px-8
            ,page]]
          ,(footer)
          [script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js"))]
          [script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/scheme.min.js"))]
          [script "hljs.highlightAll();"]]]))

(define (landing-page)
  `[.max-w-4xl.mx-auto.text-center
    [img.mx-auto.mb-6.h-16.w-auto.sm:h-20.lg:h-24 (@ (src "/static/logo.png") (alt "Schematra Logo"))]
    [h1.text-3xl.sm:text-4xl.lg:text-5xl.font-bold.text-teal-900.mb-4.sm:mb-6.cookie-regular "(Schematra)"]
    [p.text-lg.sm:text-xl.text-teal-700.mb-6.sm:mb-8.max-w-2xl.mx-auto.px-4 
     "Write web apps the way you think. Express HTML as data with Chiccup. Build components that compose naturally. Create powerful middleware with simple functions. Authentication in 3 lines, not 30."]
    [.flex.flex-col.sm:flex-row.gap-3.sm:gap-4.justify-center.mb-8.sm:mb-12.px-4
     [a.bg-teal-600.text-white.px-6.sm:px-8.py-3.rounded-lg.font-semibold.hover:bg-teal-700.transition-colors.text-center
      (@ (href "#getting-started")) "Get Started"]
     [a.border.border-teal-300.text-teal-700.px-6.sm:px-8.py-3.rounded-lg.font-semibold.hover:bg-teal-50.transition-colors.text-center
      (@ (href "https://github.com/schematra/schematra/blob/main/docs/docs.md")) "Documentation"]]
    [.grid.grid-cols-1.md:grid-cols-3.gap-4.sm:gap-6.lg:gap-8.text-left.px-4
     [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
      [h3.text-lg.font-semibold.text-teal-900.mb-3 "Zero Config Sessions"]
      [p.text-teal-700 "Cookie-based sessions work immediately. No setup, no database, no complexity. " [code.text-sm.bg-gray-100.px-1.rounded "session-set!"] " and you're done."]]
     [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
      [h3.text-lg.font-semibold.text-teal-900.mb-3 "Chiccup: HTML as Data"]
      [p.text-teal-700 "No more template syntax headaches. Write " [code.text-sm.bg-gray-100.px-1.rounded "`[.card [h1 \"Title\"]]"] " and get clean HTML. Map over lists, compose functions, build UIs that make sense."]]
     [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
      [h3.text-lg.font-semibold.text-teal-900.mb-3 "3-Line Middleware"]
      [p.text-teal-700 "Real middleware that composes. Write a function, call " [code.text-sm.bg-gray-100.px-1.rounded "use-middleware!"] ", done. No decorators, no magic, just functions."]]]
    
    [.mt-12.sm:mt-16.text-left.px-4
     [h2.text-2xl.sm:text-3xl.font-bold.text-teal-900.mb-6.sm:mb-8.text-center "Why Developers Choose Schematra"]
     [.grid.grid-cols-1.md:grid-cols-2.gap-6.sm:gap-8.mb-8.sm:mb-12
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "🚀 Write Less, Build More"]
       [p.text-teal-700 "Complete auth flows in 20 lines. Middleware in 3 lines. Components that compose naturally with Chiccup. Zero boilerplate, maximum clarity."]]
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "⚡ Chiccup Magic"]
       [p.text-teal-700 "Your HTML structure " [em "is"] " your data structure. No template engines, no context switching, no surprises. Just pure functional UI composition."]]
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "🎯 Functions All The Way"]
       [p.text-teal-700 "Middleware is just " [code.text-sm.bg-gray-100.px-1.rounded "(lambda (next) ...)"] ". Routes are functions. Components are functions. Simple, composable, testable."]]
      [.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100
       [h3.text-lg.font-semibold.text-teal-900.mb-3 "🔧 Deploy Anywhere"]
       [p.text-teal-700 "Compile to a single binary. No runtime dependencies, no complex deployments. If it runs C, it runs Schematra."]]]]
    
    
    [.mt-12.sm:mt-16.text-left.px-4
     [h2.text-2xl.sm:text-3xl.font-bold.text-teal-900.mb-6.sm:mb-8.text-center "Latest from the Blog"]
     [.grid.grid-cols-1.md:grid-cols-2.gap-6.sm:gap-8.mb-8.sm:mb-12
      ,@(let ((posts (get-blog-posts)))
          (if (null? posts)
              '([.col-span-2.text-center.text-teal-600 "No blog posts yet. Stay tuned!"])
              (map blog-post-card (take posts (min 2 (length posts))))))]
     [.text-center
      [a.text-teal-600.hover:text-teal-700.font-semibold (@ (href "/blog")) "View All Posts →"]]]

    [.mt-12.sm:mt-16.text-left.px-4
     [.text-center.mb-8
      [p.text-lg.text-teal-600 "Ready to write web apps that make sense?"]]
     [h2.text-2xl.sm:text-3xl.font-bold.text-teal-900.mb-6.sm:mb-8.text-center#getting-started "Getting Started"]
     [.bg-white.p-6.rounded-lg.shadow-sm.mb-8.border.border-teal-100
      [h3.text-lg.font-semibold.text-teal-900.mb-4 "Installation"]
      [.mb-4
       [h4.font-semibold.text-teal-800.mb-2 "Option 1: Install with CHICKEN"]
       [pre.bg-gray-100.p-3.rounded.text-sm.overflow-x-auto
        [code "chicken-install schematra"]]]
      [.mb-4
       [h4.font-semibold.text-teal-800.mb-2 "Option 2: Try with Docker"]
       [pre.bg-gray-100.p-3.rounded.text-sm.overflow-x-auto
        [code "docker run --rm -it ghcr.io/schematra/schematra:latest csi"]]]
      [p.text-teal-700.mt-4 "Then create your first app by creating a new file (e.g., " [code.bg-gray-100.px-2.py-1.rounded "app.scm"] ") and start coding!"]]
     
     [h2.text-2xl.sm:text-3xl.font-bold.text-teal-900.mb-2.text-center.mt-12 "Experience Chiccup"]
     [p.text-center.text-teal-600.mb-6.sm:mb-8 "See how HTML-as-data transforms the way you build components"]
     [.bg-white.p-4.sm:p-6.rounded-lg.shadow-sm.border.border-teal-100.mb-8.sm:mb-12
      [.grid.grid-cols-1.lg:grid-cols-2.gap-4
       [div
        [.flex.justify-between.items-center.mb-3
         [h3.text-lg.font-semibold.text-teal-900 "Chiccup Code"]
         [.space-x-2
          [button.text-sm.bg-teal-100.text-teal-700.px-3.py-1.rounded.hover:bg-teal-200
           (@ (hx-get "/playground/card") (hx-target "#chiccup-input") (hx-swap "outerHTML")) "Card"]
          [button.text-sm.bg-teal-100.text-teal-700.px-3.py-1.rounded.hover:bg-teal-200
           (@ (hx-get "/playground/form") (hx-target "#chiccup-input") (hx-swap "outerHTML")) "Form"]
          [button.text-sm.bg-teal-100.text-teal-700.px-3.py-1.rounded.hover:bg-teal-200
           (@ (hx-get "/playground/list") (hx-target "#chiccup-input") (hx-swap "outerHTML")) "List"]]]
        [form (@ (hx-post "/playground/render") (hx-target "#html-preview"))
              ,(chiccup-csrf-hidden-input)
              [textarea.w-full.h-80.p-3.border.rounded.font-mono.text-sm.resize-none.focus:outline-none.focus:ring-2.focus:ring-teal-500#chiccup-input
               (@ (name "chiccup") (placeholder "Try editing the Chiccup code...")
                  (hx-get "/playground/card") (hx-trigger "load")
                  (hx-swap "outerHTML") (hx-target "#chiccup-input"))]
              [.mt-3
               [button.bg-teal-600.text-white.px-4.py-2.rounded.hover:bg-teal-700.font-semibold
                (@ (type "submit")) "Render Preview"]]]]
       [div
        [h3.text-lg.font-semibold.text-teal-900.mb-3 "Live Preview"]
        [.w-full.h-80.p-3.border.rounded.bg-gray-50.overflow-auto
         [\#html-preview
          [.text-center.text-gray-500.py-8 "Click 'Render Preview' to see the output"]]]]]
      [p.text-sm.text-teal-600.mt-4.text-center
       "✨ Live Chiccup rendering! Edit the code above and watch HTML structure mirror your data structure in real-time."]]
     
     [h2.text-2xl.sm:text-3xl.font-bold.text-teal-900.mb-6.sm:mb-8.text-center.mt-12#see-more-examples "See More Examples"]
     [.space-y-6.sm:space-y-8
      ,(code-box "Chiccup Components" ex3 "Build dynamic UIs with pure functions. Map over data, compose components, and create interactive interfaces that feel natural.")
      ,(code-box "Simple Middleware" ex2 "Compose powerful middleware for logging, authentication, and more. Each middleware is just a simple function.")
      ,(code-box "Complete Web App" ex1 "A full authentication flow with sessions, forms, and redirects. Notice how natural HTML generation feels with Chiccup.")
      ,(code-box "JSON APIs Made Easy" ex4 "Write APIs that work with data, not strings. send-json-response handles serialization and headers automatically.")
      ,(code-box "Testing Without a Server" ex6 "Test your routes in milliseconds with isolated app instances. No HTTP server needed—just pure, fast unit tests.")
      ,(code-box "OAuth2 Authentication" ex5 "Add Google OAuth2 login to your app with oauthtoothy. Complete social authentication in under 20 lines.")]]])

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
  [form (@ (action \"/submit\") (method \"POST\"))
    [.mb-4
      [label.block.text-gray-700.text-sm.font-bold.mb-2 \"Name\"]
      [input.shadow.border.rounded.w-full.py-2.px-3.text-gray-700.focus:outline-none.focus:shadow-outline 
        (@ (type \"text\") (name \"name\") (placeholder \"Your Name\"))]]
    [.mb-4
      [label.block.text-gray-700.text-sm.font-bold.mb-2 \"Email\"]
      [input.shadow.border.rounded.w-full.py-2.px-3.text-gray-700.focus:outline-none.focus:shadow-outline
        (@ (type \"email\") (name \"email\") (placeholder \"your@email.com\"))]]
    [button.bg-blue-500.hover:bg-blue-700.text-white.font-bold.py-2.px-4.rounded.focus:outline-none.focus:shadow-outline
      \"Submit\"]]]")
    (list . "[.max-w-md.mx-auto.bg-white.p-6.rounded-lg.shadow-md
  [h2.text-xl.font-bold.mb-4.text-gray-900 \"Todo List\"]
  [ul.divide-y.divide-gray-200
    [li.py-4.flex.items-center.space-x-3
      [input.h-4.w-4.text-teal-600.rounded (@ (type \"checkbox\") (checked \"true\"))]
      [span.flex-1.text-gray-900 \"Learn Chiccup syntax\"]]
    [li.py-4.flex.items-center.space-x-3
      [input.h-4.w-4.text-teal-600.rounded (@ (type \"checkbox\"))]
      [span.flex-1.text-gray-500 \"Build amazing web apps\"]]
    [li.py-4.flex.items-center.space-x-3
      [input.h-4.w-4.text-teal-600.rounded (@ (type \"checkbox\"))]
      [span.flex-1.text-gray-500 \"Deploy with Schematra\"]]]
  [.mt-4
    [button.bg-green-500.hover:bg-green-700.text-white.font-bold.py-2.px-4.rounded
      \"Add New Todo\"]]]]")))

;; Blog functionality
(define (blog-post-card post)
  (let ((slug (alist-ref 'slug post))
        (title (alist-ref 'title post))
        (date (alist-ref 'date post))
        (excerpt (alist-ref 'excerpt post))
        (tags (alist-ref 'tags post)))
    `[.bg-white.p-6.rounded-lg.shadow-sm.border.border-teal-100.hover:shadow-md.transition-shadow
      [.text-sm.text-teal-600.mb-2 ,date]
      [h3.text-lg.font-semibold.text-teal-900.mb-3
       [a.hover:text-teal-700 (@ (href ,(conc "/blog/" slug))) ,title]]
      [p.text-teal-700.mb-4 ,excerpt]
      ,(if tags
           `[.flex.flex-wrap.gap-2
             ,@(map (lambda (tag)
                      `[span.text-xs.bg-teal-100.text-teal-700.px-2.py-1.rounded ,tag])
                    tags)]
           '())]))

;; Cache blog posts - loaded once at startup
(define *blog-posts-cache* #f)

(define (load-blog-posts!)
  (set! *blog-posts-cache*
    (condition-case
     (with-input-from-file "blog/posts.scm" read)
     (e () '()))))

(define (get-blog-posts)
  (unless *blog-posts-cache*
    (load-blog-posts!))
  *blog-posts-cache*)

(define (blog-list-page)
  (let ((posts (get-blog-posts)))
    `[.max-w-4xl.mx-auto
      [.mb-8
       [h1.text-3xl.sm:text-4xl.font-bold.text-teal-900.mb-4.cookie-regular "Blog"]
       [p.text-lg.text-teal-700 "Updates, releases, and insights from the Schematra team"]]
      ,(if (null? posts)
           `[.bg-white.p-8.rounded-lg.shadow-sm.border.border-teal-100.text-center
             [p.text-teal-600 "No blog posts yet. Stay tuned for updates!"]]
           `[.space-y-6
             ,@(map blog-post-card posts)])
      [.mt-8.text-center
       [a.text-teal-600.hover:text-teal-700 (@ (href "/")) "← Back to Home"]]]))

(define (get-blog-post-by-slug slug)
  (let ((posts (get-blog-posts)))
    (let loop ((remaining posts))
      (if (null? remaining)
          #f
          (let ((post (car remaining)))
            (if (equal? (alist-ref 'slug post) slug)
                post
                (loop (cdr remaining))))))))

;; Cache for parsed markdown content (filepath -> normalized SXML)
(define *markdown-cache* '())

(define (read-markdown-file filepath)
  ;; bypass cache in dev mode
  (let ((dev-mode? (equal? (get-environment-variable "SCHEMATRA_ENV") "development")))
    (or (and (not dev-mode?) (alist-ref filepath *markdown-cache* equal?))
	(let ((content (condition-case
			(with-input-from-file filepath read-string)
			(e () #f))))
          (if content
              (let ((parsed (map normalize-sxml (markdown->sxml content))))
		(set! *markdown-cache* (cons (cons filepath parsed) *markdown-cache*))
		parsed)
              #f)))))

;; Normalize SXML from lowdown to be compatible with Chiccup
(define (normalize-sxml node)
  (cond
   ;; List of strings -> concatenate them
   ((and (list? node)
         (not (null? node))
         (every string? node))
    (string-concatenate node))
   ;; Element with children
   ((and (list? node)
         (not (null? node))
         (symbol? (car node)))
    (let ((tag (car node))
          (rest (cdr node)))
      (cons tag (map normalize-sxml rest))))
   ;; Anything else (string, symbol, etc.) pass through
   (else node)))

(define *gh-base-url* "https://github.com/schematra/schematra/blob/main/web/blog/")

(define (blog-post-page slug)
  (let ((post (get-blog-post-by-slug slug)))
    (if post
        (let ((title (alist-ref 'title post))
              (date (alist-ref 'date post))
              (author (alist-ref 'author post))
              (excerpt (alist-ref 'excerpt post))
              (image (alist-ref 'image post))
              (tags (alist-ref 'tags post))
              (file (alist-ref 'file post))
              (content (read-markdown-file (conc "blog/" (alist-ref 'file post))))
              (url (conc "https://schematra.com/blog/" slug)))
          `[.max-w-3xl.mx-auto
            [.mb-8
             [a.text-teal-600.hover:text-teal-700.mb-4.inline-block (@ (href "/blog")) "← All Posts"]
             [h1.text-3xl.sm:text-4xl.font-bold.text-teal-900.mb-3.cookie-regular ,title]
             [.flex.items-center.gap-4.mb-4
              [.text-sm.text-teal-600
               ,(if author (conc date " • " author) date)]
              ,(if tags
                   `[.flex.flex-wrap.gap-2
                     ,@(map (lambda (tag)
                              `[span.text-xs.bg-teal-100.text-teal-700.px-2.py-1.rounded ,tag])
                            tags)]
                   '())]]
            [.bg-white.p-6.sm:p-8.rounded-lg.shadow-sm.border.border-teal-100
             ,(if content
                  `[.prose.prose-teal.max-w-none
                    ,@content
		    [p.text-sm.mt-4
		     [a (@ (href ,(conc *gh-base-url* slug ".md"))
			   (target "_blank"))
			"Source to this post."]]]
                  `[p.text-red-600 "Content not found"])]
            [.mt-8.text-center
             [a.text-teal-600.hover:text-teal-700 (@ (href "/blog")) "← Back to Blog"]]])
        `[.max-w-3xl.mx-auto
          [.bg-white.p-8.rounded-lg.shadow-sm.border.border-teal-100.text-center
           [h1.text-2xl.font-bold.text-teal-900.mb-4 "Post Not Found"]
           [p.text-teal-700.mb-6 "The blog post you're looking for doesn't exist."]
           [a.text-teal-600.hover:text-teal-700 (@ (href "/blog")) "← Back to Blog"]]])))
