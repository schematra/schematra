;; request body parser for Schematra
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
 schematra.body-parser
 (body-parser-middleware
  read-current-multipart-form-data)

 (import scheme)
 (import
   chicken.base
   chicken.string
   intarweb
   multipart-form-data
   uri-common
   spiffy ;; current-request
   schematra)

  (define (parse-form-data body-string)
    (form-urldecode body-string))

  (define (read-current-multipart-form-data #!optional max-length)
    (let* ((body (current-request-body))
           (headers (request-headers (current-request)))
           (content-type (header-value 'content-type headers #f))
           (boundary (header-param 'boundary 'content-type headers #f)))
      (unless body
        (error 'read-current-multipart-form-data "no current request body; install body-parser-middleware first"))
      (unless (eq? 'multipart/form-data content-type)
        (error 'read-current-multipart-form-data "request content type is not multipart/form-data" content-type))
      (unless boundary
        (error 'read-current-multipart-form-data "multipart/form-data request is missing a boundary"))
      (let ((port (request-body-port body)))
        (dynamic-wind
          void
          (lambda ()
            (if max-length
                (multipart-form-data-decode port boundary max-length)
                (multipart-form-data-decode port boundary)))
          (lambda () (close-input-port port))))))

  ;; Captures the request body into (current-request-body) so it can be replayed
  ;; by later consumers. For application/x-www-form-urlencoded bodies, also parses
  ;; params into (current-params) with symbol keys.
  (define (body-parser-middleware)
    (lambda (next)
      (let* ((request (current-request))
	    (headers (request-headers request))
             (captured-body #f))
        ;; RFC 7230 §3.3: a request has a body only if Content-Length or Transfer-Encoding is present
        (when (or (header-value 'content-length headers)
                  (header-value 'transfer-encoding headers))
          (let ((body (capture-request-body request)))
            (set! captured-body body)
            (current-request-body body)
            (when (eq? 'application/x-www-form-urlencoded
                       (header-value 'content-type headers))
              (current-params (append (parse-form-data (request-body-string body))
                                      (current-params))))))
        (dynamic-wind
          void
          next
          (lambda ()
            (request-body-cleanup! captured-body))))))
  )
