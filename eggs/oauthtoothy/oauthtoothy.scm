;; simple, configurable oauth2 middleware
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
 oauthtoothy
 (
  oauthtoothy-middleware
  auth-base-url
  current-auth
 )

 (import scheme)
 (import
  chicken.base
  chicken.condition
  chicken.io
  medea
  openssl
  http-client
  intarweb
  uri-common
  ;; schematra modules
  schematra
  schematra-session)

 ;; Base URL parameter for OAuth2 callback URLs
 ;;
 ;; This parameter defines the base URL used when constructing OAuth2 callback URLs.
 ;; The callback URLs are built by appending "/auth/{provider-name}/callback" to this base URL.
 ;; This should match the URL where your application is accessible to external OAuth2 providers.
 ;;
 ;; Default: "http://localhost:8080"
 ;;
 ;; Example usage:
 ;;   ;; For production deployment
 ;;   (auth-base-url "https://myapp.example.com")
 ;;   
 ;;   ;; For local development with custom port
 ;;   (auth-base-url "http://localhost:3000")
 ;;
 ;; Note: This URL must be registered with your OAuth2 providers as an allowed callback URL.
 ;; The actual callback path will be "{base-url}/auth/{provider}/callback".
 (define auth-base-url (make-parameter "http://localhost:8080"))

 (define user-save-proc (make-parameter #f))
 (define user-load-proc (make-parameter #f))

 ;; Current authentication state parameter
 ;;
 ;; This parameter holds the authentication information for the current request context.
 ;; It contains an association list with user authentication details and status.
 ;;
 ;; Default state: `((authenticated? . #f))` - indicating no authenticated user
 ;;
 ;; Authenticated state structure:
 ;;   - authenticated?: boolean - #t if user is authenticated, #f otherwise
 ;;   - user-id: any - unique identifier for the authenticated user
 ;;   - Additional fields from the user-info-parser (typically name, email, etc.)
 ;;
 ;; Example authenticated state:
 ;;   `((user-id . "12345")
 ;;     (authenticated? . #t)
 ;;     (name . "John Doe")
 ;;     (email . "john@example.com"))
 ;;
 ;; Usage in route handlers:
 ;;   (let ((auth (current-auth)))
 ;;     (if (alist-ref 'authenticated? auth)
 ;;         ;; User is authenticated
 ;;         (format "Welcome, ~A!" (alist-ref 'name auth))
 ;;         ;; User is not authenticated
 ;;         (redirect "/login")))
 ;;
 ;; Note: This parameter is automatically managed by the oauthtoothy middleware.
 ;; Do not modify it directly; use the OAuth2 flow to authenticate users.
 (define current-auth (make-parameter `((authenticated? . #f))))

 (define (exchange-code-for-token code provider)
   (let* ((token-url (alist-ref 'token-url provider))
	  (client-id (alist-ref 'client-id provider))
	  (client-secret (alist-ref 'client-secret provider))
	  (redirect-uri (build-callback-url provider))
	  (form-data `(("grant_type" . "authorization_code")
		       ("code" . ,code)
		       ("client_id" . ,client-id)
		       ("client_secret" . ,client-secret)
		       ("redirect_uri" . ,redirect-uri))))
     (condition-case
      (call-with-input-request
       token-url
       form-data
       (lambda (response-port)
	 (let* ((response-body (read-string #f response-port))
		(json-response (read-json response-body)))
	   (unless json-response (signal (condition '(json parser-error ,response-body))))
	   json-response)))
      (exn (http) (begin
		    (display exn) (newline)
		    (halt 'bad-gateway "Failed to exchange authorization code")))
      (exn (json) (halt 'bad-gateway "Invalid response from provider")))))

 (define (build-callback-url provider)
   (let ((provider-name (alist-ref 'name provider)))
     (string-append (auth-base-url) "/auth/" provider-name "/callback")))

 (define (get-user-info token provider)
   (let* ((user-info-url (alist-ref 'user-info-url provider))
	  (access-token (alist-ref 'access_token token))
	  (request (make-request uri: (uri-reference user-info-url)
				 headers: (headers
					   `((authorization #(,(string-append "Bearer " access-token) raw)))))))
     (with-input-from-request request #f read-json)))

 ;; OAuth2 authentication middleware for Schematra
 ;;
 ;; This middleware provides complete OAuth2 authentication flow integration for web applications.
 ;; It automatically handles OAuth2 authorization, token exchange, user info retrieval, and
 ;; session management. The middleware registers routes for each provider and manages
 ;; authentication state across requests.
 ;;
 ;; Parameters:
 ;;   providers: list - List of OAuth2 provider configurations (see provider format below)
 ;;
 ;; Keyword Parameters:
 ;;   success-redirect: string - URL to redirect to after successful authentication (default: "/")
 ;;   save-proc: procedure or #f - Function to save user data (default: #f for no persistence)
 ;;   load-proc: procedure or #f - Function to load user data (default: #f for no persistence)
 ;;
 ;; Provider Configuration Format:
 ;;   Each provider should be an association list with these required keys:
 ;;   - name: string - Unique identifier for the provider (e.g., "google", "github")
 ;;   - client-id: string - OAuth2 client ID from the provider
 ;;   - client-secret: string - OAuth2 client secret from the provider
 ;;   - auth-url: string - Provider's authorization endpoint URL
 ;;   - token-url: string - Provider's token exchange endpoint URL
 ;;   - user-info-url: string - Provider's user information endpoint URL
 ;;   - scopes: string - Space-separated list of OAuth2 scopes to request
 ;;   - user-info-parser: procedure - Function to normalize provider-specific user data
 ;;
 ;; Save/Load Procedures:
 ;;   save-proc: (lambda (user-id user-data) ...) - Called to persist user information
 ;;   load-proc: (lambda (user-id) ...) - Called to retrieve user information, should return
 ;;              user data alist or #f if user not found
 ;;
 ;; Registered Routes:
 ;;   For each provider named "example", the middleware registers:
 ;;   - GET /auth/example - Initiates OAuth2 flow, redirects to provider
 ;;   - GET /auth/example/callback - Handles OAuth2 callback from provider
 ;;
 ;; Authentication State:
 ;;   The middleware manages the current-auth parameter, which contains:
 ;;   - authenticated?: boolean indicating if user is logged in
 ;;   - user-id: unique identifier for the user
 ;;   - Additional user data from the user-info-parser
 ;;
 ;; Session Integration:
 ;;   Requires session middleware to be installed. Stores user-id in session
 ;;   and uses save-proc/load-proc for additional user data persistence.
 ;;
 ;; Example usage:
 ;;   (use-middleware!
 ;;    (oauthtoothy-middleware
 ;;     (list (google-provider client-id: "..." client-secret: "..."))
 ;;     success-redirect: "/dashboard"
 ;;     save-proc: save-user-to-db
 ;;     load-proc: load-user-from-db))
 (define (oauthtoothy-middleware providers #!key
				 (success-redirect "/")
				 (save-proc #f)
				 (load-proc #f))
   (when save-proc (user-save-proc save-proc))
   (when load-proc (user-load-proc load-proc))

   (for-each
    (lambda (provider)
      (let* ((provider-name (alist-ref 'name provider))
	     (callback-path (string-append (auth-base-url) "/auth/" provider-name "/callback"))
	     (start-oauth-path (string-append "/auth/" provider-name)))
	(get (callback-path) (oauthtoothy-callback-handler provider success-redirect))
	(get (start-oauth-path)
	     (parameterize ((form-urlencoded-separator "&"))
	       (let ((redirect-url (update-uri (uri-reference (alist-ref 'auth-url provider))
					       query: `((client_id . ,(alist-ref 'client-id provider))
							(redirect_uri . ,callback-path)
							(scope . ,(alist-ref 'scopes provider))
							(response_type . "code")))))
		 (redirect (uri->string redirect-url)))))))
    providers)

   (lambda (next)
     ;; should check what state of the auth cycle we're in, then
     ;; either pass or do something else
     (let* ((user-id (session-get "user-id"))
	    (user-info (if (and user-id (user-load-proc))
			   ((user-load-proc) user-id)
			   #f)))
       (parameterize ((current-auth (if user-id
					`((user-id . ,user-id)
					  (authenticated? . #t)
					  ,@user-info)
					`((authenticated? . #f)))))
	 (next)))))

 (define (oauthtoothy-callback-handler provider success-redirect)
   (let* ((params          (current-params))
	  (code            (alist-ref 'code params))
	  (token           (exchange-code-for-token code provider))
	  (user-info       (get-user-info token provider))
	  (normalized-user ((alist-ref 'user-info-parser provider) user-info))
	  (user-id         (alist-ref 'id normalized-user)))
     (when (user-save-proc)
       ((user-save-proc) user-id normalized-user token))

     ;; store in session, only user-id
     (session-set! "user-id" (alist-ref 'id user-info))
     (redirect success-redirect))))
