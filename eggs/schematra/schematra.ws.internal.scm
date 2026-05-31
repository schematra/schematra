;; Schematra WebSocket support
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
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
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

;; schematra.ws.internal contains the full implementation and exports
;; everything — both the curated public surface and the internals that
;; tests need to reach (frame parser, unmask, close-payload parser, the
;; websocket-connection record constructor/accessors). Application code
;; should import schematra.ws, which re-exports only the public subset.
(module schematra.ws.internal
  (websocket
   websocket*
   send-text
   send-binary
   close-websocket!
   current-websocket
   websocket-max-frame-size
   websocket-max-message-size
   websocket-max-fragment-count
   ;; Exposed for tests; not part of the public API.
   unmask-payload!
   parse-close-payload
   valid-close-code?
   read-websocket-frame
   read-frame-from-port
   write-websocket-frame
   assemble-fragments
   step-fragment-state
   make-websocket-connection
   websocket-connection?
   websocket-connection-open?
   websocket-connection-open?-set!
   websocket-connection-long-connection
   websocket-connection-close-code
   websocket-connection-close-reason)

  (import scheme)
  (import
   chicken.base
   chicken.bitwise
   chicken.blob
   chicken.condition
   chicken.fixnum
   chicken.io
   chicken.string
   chicken.format
   base64
   simple-sha1
   srfi-4
   srfi-13
   srfi-18
   spiffy
   (rename intarweb (headers intarweb:headers))
   schematra
   schematra.utils
   logger)

(logger/install schematra.ws)

(define-record websocket-connection
  long-connection
  send-mutex
  open?
  close-code
  close-reason)

(define current-websocket (make-parameter #f))

(define websocket-max-frame-size (make-parameter (* 1024 1024)))

;; Maximum total bytes allowed for a fragmented message (sum of all
;; fragments). Must be >= websocket-max-frame-size — surprising 1009s
;; otherwise. Exceeding this closes with code 1009.
(define websocket-max-message-size (make-parameter (* 16 1024 1024)))

;; Cap on number of fragments per message. Prevents a peer from sending
;; millions of 1-byte fragments that stay under max-message-size but
;; exhaust cons-cell memory. Exceeding this closes with code 1009.
(define websocket-max-fragment-count (make-parameter 1024))

(define websocket-guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(define (websocket-accept-key key)
  (base64-encode (hex-string->byte-string (string->sha1sum (conc key websocket-guid)))))

(define (valid-websocket-key? key)
  (and (not (string-null? key))
       (handle-exceptions exn
           #f
         (= 16 (string-length (base64-decode key))))))

(define (valid-websocket-request? request)
  (let ((headers (request-headers request)))
    (and (eq? 'GET (request-method request))
         (header-token? 'upgrade "websocket" headers)
         (header-token? 'connection "upgrade" headers)
         (equal? "13" (header-value->string (header-value 'sec-websocket-version headers #f)))
         (valid-websocket-key? (header-value->string (header-value 'sec-websocket-key headers #f))))))

(define (send-websocket-handshake! key)
  (current-response
   (update-response
    (current-response)
    status: 'switching-protocols))
  (long-connection-response-set! (current-long-connection) (current-response))
  ((handle-access-logging))
  (let ((port (long-connection-output-port (current-long-connection))))
    (display "HTTP/1.1 101 Switching Protocols\r\n" port)
    (display "Upgrade: websocket\r\n" port)
    (display "Connection: Upgrade\r\n" port)
    (display (conc "Sec-WebSocket-Accept: " (websocket-accept-key key) "\r\n") port)
    (display "\r\n" port)
    (flush-output port)))

(define (unmask-payload! payload mask)
  (let ((len (u8vector-length payload))
        (m0 (u8vector-ref mask 0))
        (m1 (u8vector-ref mask 1))
        (m2 (u8vector-ref mask 2))
        (m3 (u8vector-ref mask 3)))
    (let ((bulk (fxand len (fxnot 3))))
      (let loop ((i 0))
        (when (fx< i bulk)
          (u8vector-set! payload i
            (fxxor (u8vector-ref payload i) m0))
          (u8vector-set! payload (fx+ i 1)
            (fxxor (u8vector-ref payload (fx+ i 1)) m1))
          (u8vector-set! payload (fx+ i 2)
            (fxxor (u8vector-ref payload (fx+ i 2)) m2))
          (u8vector-set! payload (fx+ i 3)
            (fxxor (u8vector-ref payload (fx+ i 3)) m3))
          (loop (fx+ i 4))))
      (let loop ((i bulk) (k 0))
        (when (fx< i len)
          (u8vector-set! payload i
            (fxxor (u8vector-ref payload i)
                   (case k ((0) m0) ((1) m1) (else m2))))
          (loop (fx+ i 1) (fx+ k 1)))))
    payload))

(define (read-websocket-frame ws)
  (read-frame-from-port
   (long-connection-input-port (websocket-connection-long-connection ws))))

(define (read-frame-from-port port)
  (let ((b1 (read-u8 port)))
    (if (eof-object? b1)
        #f
        (let ((b2 (read-u8 port)))
          (if (eof-object? b2)
              `(protocol-error 1002 "truncated frame header")
              (let* ((fin? (not (zero? (bitwise-and b1 #x80))))
                     (rsv (bitwise-and b1 #x70))
                     (opcode (bitwise-and b1 #x0f))
                     (masked? (not (zero? (bitwise-and b2 #x80))))
                     (initial-length (bitwise-and b2 #x7f))
                     (payload-length (cond
                                      ((< initial-length 126) initial-length)
                                      ((= initial-length 126) (read-network-integer port 2))
                                      (else (read-network-integer port 8))))
                     (control-frame? (or (= opcode #x8) (= opcode #x9) (= opcode #xA))))
          (cond
           ((not (zero? rsv)) `(protocol-error 1002 "reserved bits are not supported"))
           ((or (and (fx>= opcode #x3) (fx<= opcode #x7))
                (and (fx>= opcode #xB) (fx<= opcode #xF)))
            `(protocol-error 1002 "reserved opcode"))
           ((eof-object? payload-length) `(protocol-error 1002 "truncated frame length"))
           ((and control-frame? (not fin?)) `(protocol-error 1002 "fragmented control frame"))
           ((and control-frame? (> payload-length 125)) `(protocol-error 1002 "control frame payload too large"))
           ((and (= initial-length 126) (< payload-length 126)) `(protocol-error 1002 "non-minimal frame length"))
           ((and (= initial-length 127) (< payload-length 65536)) `(protocol-error 1002 "non-minimal frame length"))
           ((> payload-length (websocket-max-frame-size)) `(protocol-error 1009 "frame payload too large"))
           ((not masked?) `(protocol-error 1002 "client frame was not masked"))
           (else
            (let ((mask (read-u8vector-exact port 4)))
              (if (not mask)
                  `(protocol-error 1002 "masked frame is missing mask")
                  (let ((payload (if (fx= payload-length 0)
                                     (make-u8vector 0)
                                     (read-u8vector-exact port payload-length))))
                    (if (not payload)
                        `(protocol-error 1002 "truncated frame payload")
                        `(frame ,opcode ,fin? ,(unmask-payload! payload mask))))))))))))))

(define (write-websocket-frame ws opcode payload)
  (let* ((payload (cond ((not payload) "")
                        ((u8vector? payload)
                         (blob->string (u8vector->blob/shared payload)))
                        (else payload)))
         (length (string-length payload))
         (port (long-connection-output-port (websocket-connection-long-connection ws))))
    (mutex-lock! (websocket-connection-send-mutex ws))
    (dynamic-wind
      void
      (lambda ()
        (write-u8 (bitwise-ior #x80 opcode) port)
        (cond
         ((< length 126)
          (write-u8 length port))
         ((< length 65536)
          (write-u8 126 port)
          (write-u8 (bitwise-and (arithmetic-shift length -8) #xff) port)
          (write-u8 (bitwise-and length #xff) port))
         (else
          (write-u8 127 port)
          (let loop ((shift 56))
            (when (>= shift 0)
              (write-u8 (bitwise-and (arithmetic-shift length (- shift)) #xff) port)
              (loop (- shift 8))))))
        (display payload port)
        (flush-output port))
      (lambda () (mutex-unlock! (websocket-connection-send-mutex ws))))))

(define (send-text message)
  (let ((ws (current-websocket)))
    (unless ws (error 'send-text "no current WebSocket connection"))
    (write-websocket-frame ws #x1 message)))

(define (send-binary bytes)
  (let ((ws (current-websocket)))
    (unless ws (error 'send-binary "no current WebSocket connection"))
    (write-websocket-frame ws #x2 bytes)))

(define (parse-close-payload payload)
  (let ((len (u8vector-length payload)))
    (if (fx< len 2)
        (values 1000 "")
        (let ((code (fxior (fxshl (u8vector-ref payload 0) 8)
                           (u8vector-ref payload 1)))
              (reason-bytes (subu8vector payload 2 len)))
          (values code
                  (blob->string (u8vector->blob/shared reason-bytes)))))))

;; RFC 6455 §7.4 + IANA WebSocket Close Code Registry.
;;   - 1000-1003, 1007-1011: defined status codes
;;   - 1004: reserved, MUST NOT be used
;;   - 1005, 1006, 1015: reserved for internal use, MUST NOT appear on wire
;;   - 1012-1014: post-RFC IANA additions (service restart, try later, bad gw)
;;   - 1016-2999: reserved for future protocol use
;;   - 3000-3999: registered by libraries/frameworks
;;   - 4000-4999: private use
(define (valid-close-code? code)
  (cond
   ((and (fx>= code 1000) (fx<= code 1011)
         (not (fx= code 1004))
         (not (fx= code 1005))
         (not (fx= code 1006))) #t)
   ((and (fx>= code 1012) (fx<= code 1014)) #t)
   ((and (fx>= code 3000) (fx<= code 4999)) #t)
   (else #f)))

(define (run-websocket-close-handler on-close code reason)
  (handle-exceptions exn
      (e (build-error-message exn (get-call-chain) #t))
    (on-close code reason)))

;; Close a WebSocket connection.
;;
;; ### Parameters
;;   - `code`: integer - WebSocket close code (default: 1000 = normal closure)
;;   - `reason`: string - Optional human-readable close reason (default: "")
;;   - `ws`: websocket-connection - The connection to close. Defaults to
;;       `(current-websocket)`, which is the connection bound by the current
;;       handler. Pass a stored connection to force-close it from another
;;       thread (for example to kick a client from a registry).
;;
;; ### Behavior
;;   - Marks the connection closed.
;;   - Sends a WebSocket close frame to the peer (best effort).
;;   - Closes the underlying input port so the route's read loop unblocks
;;     immediately. This makes force-close from another thread predictable.
;;
;; Inside `on-text`/`on-binary`/`on-open`, calling `(close-websocket!)` is
;; the graceful way to end the conversation. The next loop iteration runs
;; `on-close` so per-connection cleanup still executes.
(define (close-websocket! #!optional (code 1000) (reason "") (ws (current-websocket)))
  (when (and ws (websocket-connection-open? ws))
    ;; Record code/reason so the loop's early-exit branch (and any other
    ;; observer) can report what was actually requested rather than a
    ;; hardcoded default.
    (websocket-connection-close-code-set! ws code)
    (websocket-connection-close-reason-set! ws reason)
    (websocket-connection-open?-set! ws #f)
    (let ((payload (string-append
                    (string (integer->char (bitwise-and (arithmetic-shift code -8) #xff))
                            (integer->char (bitwise-and code #xff)))
                    reason)))
      ;; Best-effort close frame; peer may already be gone.
      (handle-exceptions exn #f
        (write-websocket-frame ws #x8 payload)))
    ;; Tear down the input port so a route thread blocked in
    ;; `read-websocket-frame` exits promptly. This is what makes
    ;; force-close from another thread actually terminate the connection.
    (handle-exceptions exn #f
      (close-input-port (long-connection-input-port (websocket-connection-long-connection ws))))))

(define (run-websocket-error-handler on-error exn)
  (handle-exceptions error-exn
      (e (build-error-message error-exn (get-call-chain) #t))
    (on-error exn)))

(define (call-websocket-handler on-error on-close thunk)
  (handle-exceptions exn
      (begin
        ;; Let the user-provided on-error inspect the exception before we
        ;; close the connection. Failures inside on-error are logged so they
        ;; cannot mask the original error.
        (run-websocket-error-handler on-error exn)
        ;; 1011 tells the browser the server hit an unexpected condition.
        (close-websocket! 1011 "internal server error")
        (run-websocket-close-handler on-close 1011 "internal server error")
        ;; Re-raise so the router's outer exception handler (which checks
        ;; current-long-connection) absorbs and logs it cleanly. on-error and
        ;; on-close have already run, so the router must NOT treat this as a
        ;; regular 500 — that path is gated on current-long-connection being
        ;; set, which it is here.
        (raise exn))
    (thunk)))

;; Concatenate a newest-first list of u8vector fragments into a single
;; u8vector of `total-bytes`. One allocation, O(total-bytes) byte-copy
;; via a tight fx-loop. `subu8vector` is not useful here — it allocates
;; per slice; a hand-rolled copy loop is the portable choice and is
;; faster than a growable-buffer scheme for the common case of 1-5
;; fragments.
(define (assemble-fragments rev-pieces total-bytes)
  (let ((out (make-u8vector total-bytes))
        (pieces (reverse rev-pieces)))
    (let outer ((pieces pieces) (offset 0))
      (cond
       ((null? pieces) out)
       (else
        (let* ((piece (car pieces))
               (plen (u8vector-length piece)))
          (let inner ((j 0))
            (cond
             ((fx>= j plen) (outer (cdr pieces) (fx+ offset plen)))
             (else
              (u8vector-set! out (fx+ offset j) (u8vector-ref piece j))
              (inner (fx+ j 1)))))))))))

;; Pure step function for the loop's fragmentation state machine. Same
;; code path used by `run-websocket-loop`; extracted so tests can drive
;; it without ports or connections.
;;
;; `state` is a 4-vector #(frag-opcode fragments-rev frag-bytes utf8-state):
;;   - frag-opcode: #f (idle) or 1 (text) / 2 (binary) when assembling
;;   - fragments-rev: newest-first list of unmasked u8vector payloads
;;   - frag-bytes: total bytes accumulated so far
;;   - utf8-state: Höhrmann DFA state across already-seen text bytes;
;;     `utf8-accept` for idle/binary. Lets text messages fail-fast at
;;     the offending byte (RFC 6455 §5.6, Autobahn §6.4.x) instead of
;;     waiting for the full message to assemble.
;;
;; Returns (values new-state action) where action is one of:
;;   '(continue)
;;   `(deliver ,opcode ,u8vector)
;;   `(error ,code ,reason)
(define (step-fragment-state state opcode fin? payload)
  (let ((frag-opcode (vector-ref state 0))
        (fragments-rev (vector-ref state 1))
        (frag-bytes (vector-ref state 2))
        (utf8-state (vector-ref state 3))
        (payload-len (u8vector-length payload)))
    (cond
     ;; Idle: no fragmented message in flight.
     ((not frag-opcode)
      (cond
       ((fx= opcode #x0)
        (values state '(error 1002 "unexpected continuation frame")))
       ((fx= opcode #x1)
        (cond
         (fin?
          ;; Single-frame text: validate end-to-end before delivery.
          (let ((final (utf8-decode-feed utf8-accept payload)))
            (cond
             ((fx= final utf8-accept)
              (values (vector #f '() 0 utf8-accept) `(deliver ,opcode ,payload)))
             (else
              (values state '(error 1007 "invalid utf-8"))))))
         ((fx> payload-len (websocket-max-message-size))
          (values state '(error 1009 "message too large")))
         (else
          (let ((new-utf8 (utf8-decode-feed utf8-accept payload)))
            (cond
             ((fx= new-utf8 utf8-reject)
              (values state '(error 1007 "invalid utf-8")))
             (else
              (values (vector opcode (list payload) payload-len new-utf8)
                      '(continue))))))))
       ((fx= opcode #x2)
        (cond
         (fin?
          (values (vector #f '() 0 utf8-accept) `(deliver ,opcode ,payload)))
         ((fx> payload-len (websocket-max-message-size))
          (values state '(error 1009 "message too large")))
         (else
          (values (vector opcode (list payload) payload-len utf8-accept)
                  '(continue)))))
       (else
        ;; Defensive — control frames are dispatched outside this function.
        (values state '(error 1002 "invalid opcode for fragment state")))))
     ;; Active: assembling a fragmented message.
     (else
      (cond
       ((or (fx= opcode #x1) (fx= opcode #x2))
        (values state '(error 1002 "data frame during fragmented message")))
       ((fx= opcode #x0)
        (let ((new-bytes (fx+ frag-bytes payload-len)))
          (cond
           ((fx> new-bytes (websocket-max-message-size))
            (values state '(error 1009 "message too large")))
           ((fx>= (length fragments-rev) (websocket-max-fragment-count))
            (values state '(error 1009 "too many fragments")))
           (else
            (let ((new-utf8 (if (fx= frag-opcode #x1)
                                (utf8-decode-feed utf8-state payload)
                                utf8-accept)))
              (cond
               ((and (fx= frag-opcode #x1) (fx= new-utf8 utf8-reject))
                (values state '(error 1007 "invalid utf-8")))
               (fin?
                (cond
                 ((and (fx= frag-opcode #x1) (not (fx= new-utf8 utf8-accept)))
                  ;; Message ended mid-codepoint.
                  (values state '(error 1007 "invalid utf-8")))
                 (else
                  (let ((assembled (assemble-fragments
                                    (cons payload fragments-rev) new-bytes)))
                    (values (vector #f '() 0 utf8-accept)
                            `(deliver ,frag-opcode ,assembled))))))
               (else
                (values (vector frag-opcode (cons payload fragments-rev)
                                new-bytes new-utf8)
                        '(continue)))))))))
       (else
        (values state '(error 1002 "invalid opcode for fragment state"))))))))

(define (run-websocket-loop ws on-text on-binary on-close on-error)
  (let loop ((frag-opcode #f) (fragments '()) (frag-bytes 0) (utf8-state utf8-accept))
    (cond
     ((not (websocket-connection-open? ws))
      ;; The connection was closed by close-websocket! (from a handler or
      ;; another thread). Run the close callback so per-connection cleanup
      ;; still happens, using the code/reason actually passed to
      ;; close-websocket!.
      (run-websocket-close-handler on-close
                                   (websocket-connection-close-code ws)
                                   (websocket-connection-close-reason ws)))
     (else
      (let ((frame (handle-exceptions exn
                       ;; A force-close from another thread closes the input
                       ;; port underneath us. Treat that as a normal EOF.
                       #f
                     (read-websocket-frame ws))))
        (cond
         ((not frame)
          (websocket-connection-open?-set! ws #f)
          (run-websocket-close-handler on-close 1006 "connection closed"))
         ((eq? 'protocol-error (car frame))
          (close-websocket! (cadr frame) (caddr frame))
          (run-websocket-close-handler on-close (cadr frame) (caddr frame)))
         (else
          (let ((opcode (cadr frame))
                (fin? (caddr frame))
                (payload (cadddr frame)))
            (cond
             ;; Control frames are handled inline and preserve fragment
             ;; state. Pong reply uses the ping's payload — bound here,
             ;; nothing from an in-flight fragmented message leaks in.
             ((fx= opcode #x8)
              ;; Validate the peer's close frame before echoing the
              ;; code/reason back. RFC 6455 §5.5.1 says the body is
              ;; either empty or a 2-byte code optionally followed by a
              ;; UTF-8 reason. A 1-byte body, an out-of-range code, or
              ;; invalid UTF-8 in the reason all require we respond with
              ;; 1002 (or 1007 for UTF-8) rather than mirror the input.
              (let ((plen (u8vector-length payload)))
                (cond
                 ((fx= plen 0)
                  (close-websocket! 1000 "")
                  (run-websocket-close-handler on-close 1000 ""))
                 ((fx= plen 1)
                  (close-websocket! 1002 "invalid close payload")
                  (run-websocket-close-handler on-close 1002 "invalid close payload"))
                 (else
                  (let ((code (fxior (fxshl (u8vector-ref payload 0) 8)
                                     (u8vector-ref payload 1)))
                        (reason-bytes (subu8vector payload 2 plen)))
                    (cond
                     ((not (valid-close-code? code))
                      (close-websocket! 1002 "invalid close code")
                      (run-websocket-close-handler on-close 1002 "invalid close code"))
                     ((not (utf8-valid? reason-bytes))
                      (close-websocket! 1007 "invalid utf-8 in close reason")
                      (run-websocket-close-handler on-close 1007 "invalid utf-8 in close reason"))
                     (else
                      (let ((reason (blob->string (u8vector->blob/shared reason-bytes))))
                        (close-websocket! code reason)
                        (run-websocket-close-handler on-close code reason)))))))))
             ((fx= opcode #x9)
              (write-websocket-frame ws #xA payload)
              (loop frag-opcode fragments frag-bytes utf8-state))
             ((fx= opcode #xA)
              (loop frag-opcode fragments frag-bytes utf8-state))
             ;; Data / continuation frames go through the state machine.
             ;; UTF-8 is validated incrementally inside step-fragment-state,
             ;; so by the time we see a `deliver` action for a text message
             ;; every byte has already been vetted — no second pass here.
             ((or (fx= opcode #x0) (fx= opcode #x1) (fx= opcode #x2))
              (let-values (((new-state action)
                            (step-fragment-state
                             (vector frag-opcode fragments frag-bytes utf8-state)
                             opcode fin? payload)))
                (let ((nfo (vector-ref new-state 0))
                      (nfr (vector-ref new-state 1))
                      (nfb (vector-ref new-state 2))
                      (nus (vector-ref new-state 3)))
                  (case (car action)
                    ((continue) (loop nfo nfr nfb nus))
                    ((deliver)
                     (let ((msg-opcode (cadr action))
                           (msg-payload (caddr action)))
                       (cond
                        ((fx= msg-opcode #x1)
                         (call-websocket-handler on-error on-close
                           (lambda ()
                             (on-text (blob->string (u8vector->blob/shared msg-payload)))))
                         (loop nfo nfr nfb nus))
                        (else
                         (call-websocket-handler on-error on-close
                           (lambda () (on-binary msg-payload)))
                         (loop nfo nfr nfb nus)))))
                    ((error)
                     (let ((code (cadr action)) (reason (caddr action)))
                       (close-websocket! code reason)
                       (run-websocket-close-handler on-close code reason)))))))
             ;; Defensive: parser already rejects reserved opcodes.
             (else
              (close-websocket! 1003 "unsupported frame")
              (run-websocket-close-handler on-close 1003 "unsupported frame")))))))))))

(define (websocket* path on-open on-text on-binary on-close on-error)
  (get path
       (let* ((request (current-request))
              (headers (request-headers request))
              (key (header-value->string (header-value 'sec-websocket-key headers #f))))
         (unless (valid-websocket-request? request)
           (halt 'bad-request "Invalid WebSocket upgrade request"))
         (let* ((conn (start-long-connection! 'websocket))
                (ws (make-websocket-connection conn (make-mutex) #t 1000 "closed by server")))
           (parameterize ((current-websocket ws))
             (send-websocket-handshake! key)
             (call-websocket-handler on-error on-close on-open)
             (run-websocket-loop ws on-text on-binary on-close on-error))))))

(define-syntax websocket
  (syntax-rules (on-open on-text on-binary on-close on-error)
    ((_ path
        (on-open open-body ...)
        (on-text message text-body ...)
        (on-binary bytes binary-body ...)
        (on-close code reason close-body ...)
        (on-error exn error-body ...))
     (websocket* path
                 (lambda () open-body ...)
                 (lambda (message) text-body ...)
                 (lambda (bytes) binary-body ...)
                 (lambda (code reason) close-body ...)
                 (lambda (exn) error-body ...)))
    ((_ path
        (on-open open-body ...)
        (on-text message text-body ...)
        (on-binary bytes binary-body ...)
        (on-close code reason close-body ...))
     (websocket* path
                 (lambda () open-body ...)
                 (lambda (message) text-body ...)
                 (lambda (bytes) binary-body ...)
                 (lambda (code reason) close-body ...)
                 (lambda (exn) (void))))
    ((_ path
        (on-open open-body ...)
        (on-text message text-body ...)
        (on-close code reason close-body ...)
        (on-error exn error-body ...))
     (websocket* path
                 (lambda () open-body ...)
                 (lambda (message) text-body ...)
                 (lambda (bytes) (void))
                 (lambda (code reason) close-body ...)
                 (lambda (exn) error-body ...)))
    ((_ path
        (on-open open-body ...)
        (on-text message text-body ...)
        (on-close code reason close-body ...))
     (websocket* path
                 (lambda () open-body ...)
                 (lambda (message) text-body ...)
                 (lambda (bytes) (void))
                 (lambda (code reason) close-body ...)
                 (lambda (exn) (void))))
    ((_ path
        (on-text message text-body ...))
     (websocket* path
                 (lambda () (void))
                 (lambda (message) text-body ...)
                 (lambda (bytes) (void))
                 (lambda (code reason) (void))
                 (lambda (exn) (void))))))

) ;; end module schematra.ws.internal

