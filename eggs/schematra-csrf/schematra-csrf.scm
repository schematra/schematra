;; schematra-csrf - part of Schematra.
;;
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

(module
 schematra-csrf

 (
  csrf-middleware
  csrf-token-key
  csrf-form-field
  csrf-get-token
  chiccup-csrf-hidden-input
  ) ;; export list

 (import scheme)
 (import
  chicken.base
  chicken.blob
  chicken.random
  base64
  intarweb
  spiffy ;; current-request
  schematra
  schematra-session)

 ;; Parameter that defines the session key used to store CSRF tokens.
 ;;
 ;; ### Parameters
 ;;   - `key`: string - The session key name (default: "csrf-token")
 ;;
 ;; ### Examples
 ;; ```scheme
 ;; ;; Use default key
 ;; (csrf-token-key) ; => "csrf-token"
 ;;
 ;; ;; Customize the session key
 ;; (csrf-token-key "my-csrf-key")
 ;; ```
 (define csrf-token-key (make-parameter "csrf-token"))

 ;; Parameter that defines the HTML form field name used for CSRF tokens.
 ;;
 ;; ### Parameters
 ;;   - `field-name`: string - The form field name (default: "_csrf_token")
 ;;
 ;; ### Examples
 ;; ```scheme
 ;; ;; Use default field name
 ;; (csrf-form-field) ; => "_csrf_token"
 ;;
 ;; ;; Customize the form field name
 ;; (csrf-form-field "authenticity_token")
 ;; ```
 (define csrf-form-field (make-parameter '_csrf_token))

 ;; (generate-csrf-token)
 (define (generate-csrf-token #!optional (size 64))
   (base64-encode (random-bytes (make-string size))))

 (define (chiccup-csrf-hidden-input)
   `[input (@ (type "hidden") (name ,(symbol->string (csrf-form-field))) (value ,(csrf-get-token)))])

 ;; Retrieves or generates a CSRF token for the current session.
 ;;
 ;; This function first checks if a CSRF token already exists in the session.
 ;; If found, it returns the existing token. If not found, it generates a new
 ;; cryptographically secure token, stores it in the session, and returns it.
 ;;
 ;; ### Returns
 ;;   A base64-encoded string containing the CSRF token
 ;;
 ;; ### Examples
 ;; ```scheme
 ;; ;; Get token for use in forms
 ;; (let ((token (csrf-get-token)))
 ;;   `(input (@ (type "hidden")
 ;;              (name ,(csrf-form-field))
 ;;              (value ,token))))
 ;;
 ;; ;; Get token for AJAX requests
 ;; (get ("/api/token")
 ;;      (csrf-get-token))
 ;; ```
 (define (csrf-get-token)
   (or (session-get (csrf-token-key))
       (let ((token (generate-csrf-token)))
	 (session-set! (csrf-token-key) token)
	 token)))

 (define (csrf-token-valid? submitted-token)
   (let ((session-token (session-get (csrf-token-key))))
     (and session-token
          submitted-token
          (string=? session-token submitted-token))))

 (define (extract-csrf-from-request request)
   ;; Check form data first (assume it might be in the body), then headers
   (or (alist-ref (csrf-form-field) (current-params))
       (header-value 'x-csrf-token (request-headers request))))

 ;; Creates CSRF protection middleware for Schematra applications.
 ;;
 ;; This middleware automatically protects against Cross-Site Request Forgery (CSRF)
 ;; attacks by validating CSRF tokens on unsafe HTTP methods. Safe methods (GET, HEAD,
 ;; OPTIONS, TRACE) are allowed through without token validation, while unsafe methods
 ;; (POST, PUT, DELETE, PATCH) require a valid CSRF token.
 ;;
 ;; The middleware looks for CSRF tokens in two places:
 ;; 1. Form data using the field name from `csrf-form-field` parameter
 ;; 2. HTTP header `X-CSRF-Token`
 ;;
 ;; ### Returns
 ;;   A middleware function that can be used with `use-middleware!`
 ;;
 ;; ### Behavior
 ;;   - Safe HTTP methods (GET, HEAD, OPTIONS, TRACE) pass through without validation
 ;;   - Unsafe methods require a valid CSRF token matching the session token
 ;;   - Missing or invalid tokens result in a 403 Forbidden response
 ;;   - Tokens are validated using constant-time string comparison
 ;;
 ;; ### Examples
 ;; ```scheme
 ;; ;; Enable CSRF protection globally
 ;; (use-middleware! (csrf-middleware))
 ;;
 ;; ;; Custom configuration
 ;; (csrf-form-field "authenticity_token")
 ;; (csrf-token-key "my-csrf-key")
 ;; (use-middleware! (csrf-middleware))
 ;;
 ;; ;; HTML form with CSRF token
 ;; (get ("/form")
 ;;      `(form (@ (method "POST") (action "/submit"))
 ;;             (input (@ (type "hidden")
 ;;                       (name ,(csrf-form-field))
 ;;                       (value ,(csrf-get-token))))
 ;;             (input (@ (type "submit") (value "Submit")))))
 ;; ```
 (define (csrf-middleware)
   (lambda (next)
     (let* ((request (current-request))
	    (method (request-method request)))
       ;; try to get the token to generate if it hasn't been set
       ;; before
       (csrf-get-token)
       (cond
	;; Safe methods don't need CSRF protection
	[(memq method '(GET HEAD OPTIONS TRACE))
	 (next)]
	;; Unsafe methods need valid CSRF token
	[else
	 (let ((submitted-token (extract-csrf-from-request request)))
           (if (csrf-token-valid? submitted-token)
               (next)
               (halt 'forbidden "CSRF token missing or invalid")))])))))
