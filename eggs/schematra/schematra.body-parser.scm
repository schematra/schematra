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
 (body-parser-middleware)

 (import scheme)
 (import
  chicken.base
  chicken.string
  intarweb
  uri-common
  spiffy ;; current-request
  schematra)

  (define (parse-form-data body-string)
    (form-urldecode body-string))

 ;; Buffers the raw body into (current-raw-body) so it remains accessible
 ;; for signature verification after parsing. For application/x-www-form-urlencoded
 ;; bodies, also parses params into (current-params) with symbol keys.
 (define (body-parser-middleware)
   (lambda (next)
     (let* ((request (current-request))
	    (headers (request-headers request)))
       ;; RFC 7230 §3.3: a request has a body only if Content-Length or Transfer-Encoding is present
       (when (or (header-value 'content-length headers)
                 (header-value 'transfer-encoding headers))
         (let ((raw (request-body-string request)))
           (current-raw-body raw)
           (when (eq? 'application/x-www-form-urlencoded
                      (header-value 'content-type headers))
             (current-params (append (parse-form-data raw) (current-params))))))
       (next))))
 )
