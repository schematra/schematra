;; Schematra - a very simple web framework for scheme inspired in
;; Sinatra
;; Copyright 2025 Rolando Abarca
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;
;; 2. Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following
;; disclaimer in the documentation and/or other materials provided
;; with the distribution.
;;
;; 3. Neither the name of the copyright holder nor the names of its
;; contributors may be used to endorse or promote products derived
;; from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; “AS IS” AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(import
 chicken.port
 chicken.string
 chicken.pretty-print
 chicken.process-context
 srfi-13
 schematra
 schematra-session
 schematra-csrf
 schematra-body-parser
 chiccup)


(include "examples")
(include "components")

(define app (schematra/make-app))

(with-schematra-app
 app
 (use-middleware! (body-parser-middleware))
 (use-middleware! (session-middleware "secret"))
 (use-middleware! (csrf-middleware))

 (static "/static" "./public")

 (get "/playground/:example"
      (let* ((example-name (alist-ref "example" (current-params) equal?))
	     (example-code (alist-ref (string->symbol example-name) playground-examples)))
	(ccup->html
	 (if example-code
	     `[textarea.w-full.h-80.p-3.border.rounded.font-mono.text-sm.resize-none.focus:outline-none.focus:ring-2.focus:ring-teal-500#chiccup-input
	       (@ (name "chiccup") (placeholder "Try editing the Chiccup code..."))
	       ,example-code]
	     `[div#chiccup-input.w-full.h-80.p-3.border.rounded.bg-red-50.text-red-600.flex.items-center.justify-center
	       "Example not found"]))))

 (post "/playground/render"
       ;; got the 'chiccup param from the body, using the body-parser middleware
       (let ((chiccup-code (alist-ref 'chiccup (current-params))))
         (if chiccup-code
             (let ((cleaned-code (string-trim chiccup-code)))
               (condition-case
                (ccup->html (with-input-from-string cleaned-code read))
                (e () (ccup->html 
                       `[.text-red-600.p-4.bg-red-50.rounded.border.border-red-200
                         [h4.font-bold "Syntax Error"]
                         [p "Invalid Chiccup syntax. Please check your brackets and formatting."]]))))
             (ccup->html `[p "No code provided"]))))

 (get "/"
      (ccup->html (layout (landing-page))))

 (get "/api"
      (redirect "https://github.com/schematra/schematra"))

 (schematra-install)
 (let* ((environment (or (get-environment-variable "SCHEMATRA_ENV") "production"))
        (dev-env?    (string=? environment "development")))
   (schematra-start development?: dev-env? nrepl?: #f)))
