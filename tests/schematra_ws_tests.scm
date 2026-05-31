#!/usr/bin/env csi -s
;; Direct unit tests for schematra.ws internals.
;;
;; These tests reach the implementation through schematra.ws.internal,
;; which exposes the protocol-level routines (unmask, frame parser,
;; close-payload parser) for testing. Application code should keep
;; importing schematra.ws.

(import scheme)
(import
 chicken.base
 chicken.bitwise
 chicken.fixnum
 chicken.io
 srfi-4
 schematra.utils
 schematra.ws.internal
 test)

;; ============================================================
;; unmask-payload!
;; ============================================================
(test-group "unmask-payload!"
  (test "zero mask leaves payload unchanged"
        '(1 2 3 4 5)
        (u8vector->list
         (unmask-payload! (u8vector 1 2 3 4 5)
                          (u8vector 0 0 0 0))))

  (test "XOR is self-inverse (apply twice = identity)"
        '(72 101 108 108 111)
        (let ((mask (u8vector #xCA #xFE #xBA #xBE))
              (buf (u8vector 72 101 108 108 111)))
          (unmask-payload! buf mask)
          (unmask-payload! buf mask)
          (u8vector->list buf)))

  ;; Length 4: byte 3 XORed with m3 (=4) gives 0; if the tail had handled
  ;; it instead, k=3 hits the `else m2` branch and would yield 4^3=7.
  (test "length 4: byte 3 uses bulk's m3 (not tail's else=m2)"
        '(0 0 0 0)
        (u8vector->list
         (unmask-payload! (u8vector 1 2 3 4) (u8vector 1 2 3 4))))

  ;; Length 3: bulk = 0, tail handles bytes 0..2 with k=0,1,2 → m0,m1,m2.
  (test "length 3: tail-only dispatch (k=0,1,2 → m0,m1,m2)"
        '(0 0 0)
        (u8vector->list
         (unmask-payload! (u8vector 1 2 3) (u8vector 1 2 3 4))))

  ;; Length 7: bulk handles bytes 0..3 (m0..m3), tail handles 4..6 (m0..m2).
  ;; Index 3 = 4^4=0 (bulk's m3) AND index 6 = 3^3=0 (tail's m2 at k=2).
  (test "length 7: bulk covers [0..3] (m3 at idx 3), tail covers [4..6]"
        '(0 0 0 0 0 0 0)
        (u8vector->list
         (unmask-payload! (u8vector 1 2 3 4 1 2 3)
                          (u8vector 1 2 3 4))))

  ;; Length 8: both bulk iters use m3 at indices 3 and 7. If only the first
  ;; iter ran and the tail picked up [4..7], byte 7 (k=3) → else=m2, 4^3=7.
  (test "length 8: byte 7 uses bulk's m3 (not tail's else=m2)"
        '(0 0 0 0 0 0 0 0)
        (u8vector->list
         (unmask-payload! (u8vector 1 2 3 4 1 2 3 4)
                          (u8vector 1 2 3 4))))

  (test "empty payload"
        '()
        (u8vector->list
         (unmask-payload! (make-u8vector 0) (u8vector 1 2 3 4))))

  (test "returns the same vector (in-place)"
        #t
        (let ((v (u8vector 1 2 3 4)))
          (eq? v (unmask-payload! v (u8vector 0 0 0 0))))))

;; ============================================================
;; utf8-valid? (lives in schematra.utils)
;; ============================================================
(test-group "utf8-valid?"
  (test "empty is valid" #t (utf8-valid? (u8vector)))
  (test "pure ASCII"     #t (utf8-valid? (u8vector 72 105)))
  (test "2-byte: U+00E9 (C3 A9)"
        #t (utf8-valid? (u8vector #xC3 #xA9)))
  (test "3-byte: U+3042 (E3 81 82)"
        #t (utf8-valid? (u8vector #xE3 #x81 #x82)))
  (test "4-byte: U+1D11E (F0 9D 84 9E)"
        #t (utf8-valid? (u8vector #xF0 #x9D #x84 #x9E)))
  (test "max BMP non-surrogate: U+FFFD (EF BF BD)"
        #t (utf8-valid? (u8vector #xEF #xBF #xBD)))
  (test "max codepoint: U+10FFFF (F4 8F BF BF)"
        #t (utf8-valid? (u8vector #xF4 #x8F #xBF #xBF)))

  (test "stray continuation byte"
        #f (utf8-valid? (u8vector #x80)))
  (test "truncated 2-byte sequence"
        #f (utf8-valid? (u8vector #xC3)))
  (test "truncated 3-byte sequence"
        #f (utf8-valid? (u8vector #xE3 #x81)))
  (test "truncated 4-byte sequence"
        #f (utf8-valid? (u8vector #xF0 #x9D #x84)))
  (test "overlong NUL (C0 80)"
        #f (utf8-valid? (u8vector #xC0 #x80)))
  (test "overlong 3-byte (E0 80 80)"
        #f (utf8-valid? (u8vector #xE0 #x80 #x80)))
  (test "overlong 4-byte (F0 80 80 80)"
        #f (utf8-valid? (u8vector #xF0 #x80 #x80 #x80)))
  (test "surrogate U+D800 (ED A0 80)"
        #f (utf8-valid? (u8vector #xED #xA0 #x80)))
  (test "surrogate U+DFFF (ED BF BF)"
        #f (utf8-valid? (u8vector #xED #xBF #xBF)))
  (test "above U+10FFFF (F4 90 80 80)"
        #f (utf8-valid? (u8vector #xF4 #x90 #x80 #x80)))
  (test "invalid lead F5"
        #f (utf8-valid? (u8vector #xF5 #x80 #x80 #x80)))
  (test "invalid lead FF"
        #f (utf8-valid? (u8vector #xFF))))

;; ============================================================
;; parse-close-payload
;; ============================================================
(test-group "parse-close-payload"
  (test "empty payload -> 1000, empty reason"
        '(1000 "")
        (let-values (((c r) (parse-close-payload (u8vector))))
          (list c r)))

  (test "1-byte payload -> default 1000, empty reason"
        '(1000 "")
        (let-values (((c r) (parse-close-payload (u8vector 3))))
          (list c r)))

  (test "2-byte: 03 E8 = 1000"
        '(1000 "")
        (let-values (((c r) (parse-close-payload (u8vector #x03 #xE8))))
          (list c r)))

  (test "code + reason: 03 E9 'bye' -> 1001, 'bye'"
        '(1001 "bye")
        (let-values (((c r) (parse-close-payload
                              (u8vector #x03 #xE9 #x62 #x79 #x65))))
          (list c r)))

  (test "code with 0 reason bytes: 03 EA -> 1002, ''"
        '(1002 "")
        (let-values (((c r) (parse-close-payload (u8vector #x03 #xEA))))
          (list c r))))

;; ============================================================
;; valid-close-code? — RFC 6455 §7.4 + IANA registry
;; ============================================================
(test-group "valid-close-code?"
  ;; Defined codes that MUST be accepted.
  (test "1000 normal closure"       #t (valid-close-code? 1000))
  (test "1001 going away"           #t (valid-close-code? 1001))
  (test "1002 protocol error"       #t (valid-close-code? 1002))
  (test "1003 unsupported data"     #t (valid-close-code? 1003))
  (test "1007 invalid utf-8"        #t (valid-close-code? 1007))
  (test "1008 policy violation"     #t (valid-close-code? 1008))
  (test "1009 message too big"      #t (valid-close-code? 1009))
  (test "1010 mandatory extension"  #t (valid-close-code? 1010))
  (test "1011 server error"         #t (valid-close-code? 1011))
  ;; Post-RFC IANA additions.
  (test "1012 service restart"      #t (valid-close-code? 1012))
  (test "1013 try again later"      #t (valid-close-code? 1013))
  (test "1014 bad gateway"          #t (valid-close-code? 1014))
  ;; Application range.
  (test "3000 registered low"       #t (valid-close-code? 3000))
  (test "3999 registered high"      #t (valid-close-code? 3999))
  (test "4000 private low"          #t (valid-close-code? 4000))
  (test "4999 private high"         #t (valid-close-code? 4999))

  ;; Reserved / not-on-wire codes that MUST be rejected.
  (test "0 invalid"                 #f (valid-close-code? 0))
  (test "999 just below range"      #f (valid-close-code? 999))
  (test "1004 reserved"             #f (valid-close-code? 1004))
  (test "1005 internal only"        #f (valid-close-code? 1005))
  (test "1006 internal only"        #f (valid-close-code? 1006))
  (test "1015 internal only (TLS)"  #f (valid-close-code? 1015))
  (test "1016 reserved future"      #f (valid-close-code? 1016))
  (test "1100 reserved future"      #f (valid-close-code? 1100))
  (test "2000 reserved future"      #f (valid-close-code? 2000))
  (test "2999 reserved future"      #f (valid-close-code? 2999))
  (test "5000 above private range"  #f (valid-close-code? 5000)))

;; ============================================================
;; read-frame-from-port (end-to-end wire format)
;; ============================================================

;; Build a client->server frame on the wire. Always FIN=1, always
;; masked (clients MUST mask per RFC 6455). Length encoding picks the
;; smallest variant.
(define (build-frame opcode payload-bytes
                     #!key
                     (mask (u8vector #xCA #xFE #xBA #xBE))
                     (fin #t))
  (let ((len (length payload-bytes))
        (out (open-output-string)))
    (write-char (integer->char (fxior (if fin #x80 #x00) opcode)) out)
    (cond
     ((fx< len 126)
      (write-char (integer->char (fxior #x80 len)) out))
     ((fx< len 65536)
      (write-char (integer->char #xFE) out)
      (write-char (integer->char (fxand (fxshr len 8) #xff)) out)
      (write-char (integer->char (fxand len #xff)) out))
     (else
      (write-char (integer->char #xFF) out)
      (let loop ((shift 56))
        (when (fx>= shift 0)
          (write-char (integer->char (fxand (arithmetic-shift len (- shift)) #xff)) out)
          (loop (fx- shift 8))))))
    (for-each (lambda (b) (write-char (integer->char b) out))
              (u8vector->list mask))
    (let loop ((bs payload-bytes) (i 0))
      (when (pair? bs)
        (write-char (integer->char (fxxor (car bs) (u8vector-ref mask (fxmod i 4)))) out)
        (loop (cdr bs) (fx+ i 1))))
    (get-output-string out)))

(test-group "read-frame-from-port"
  (test "text frame 'Hi' round-trip"
        '(frame 1 #t (72 105))
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #x1 '(72 105))))))
          (list (car f) (cadr f) (caddr f) (u8vector->list (cadddr f)))))

  (test "binary frame round-trip preserves bytes 0..255"
        '(frame 2 #t (0 1 2 3 255 128 64))
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #x2 '(0 1 2 3 255 128 64))))))
          (list (car f) (cadr f) (caddr f) (u8vector->list (cadddr f)))))

  (test "empty payload"
        '(frame 1 #t ())
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #x1 '())))))
          (list (car f) (cadr f) (caddr f) (u8vector->list (cadddr f)))))

  ;; 126-byte payload exercises the 16-bit extended length path.
  (test "126-byte payload via 16-bit length encoding"
        126
        (let* ((bytes (let loop ((n 126) (acc '()))
                        (if (fx= n 0) acc (loop (fx- n 1) (cons (fxand n #xff) acc)))))
               (f (read-frame-from-port
                   (open-input-string (build-frame #x1 bytes)))))
          (u8vector-length (cadddr f))))

  ;; 65536-byte payload exercises the 64-bit extended length path.
  (test "65536-byte payload via 64-bit length encoding"
        65536
        (let* ((bytes (let loop ((n 65536) (acc '()))
                        (if (fx= n 0) acc (loop (fx- n 1) (cons 0 acc)))))
               (f (read-frame-from-port
                   (open-input-string (build-frame #x1 bytes)))))
          (u8vector-length (cadddr f))))

  (test "EOF on empty stream returns #f"
        #f
        (read-frame-from-port (open-input-string "")))

  (test "single-byte stream -> truncated header (protocol-error 1002)"
        '(protocol-error 1002)
        (let ((f (read-frame-from-port (open-input-string "\x81"))))
          (list (car f) (cadr f))))

  (test "unmasked frame -> protocol-error 1002"
        '(protocol-error 1002)
        (let ((f (read-frame-from-port (open-input-string "\x81\x02ab"))))
          (list (car f) (cadr f))))

  (test "parser accepts opcode 0 (continuation); state machine handles validity"
        '(frame 0 #t (1 2))
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #x0 '(1 2))))))
          (list (car f) (cadr f) (caddr f) (u8vector->list (cadddr f)))))

  (test "rsv bits set -> protocol-error"
        'protocol-error
        ;; b1 = 0xC1 = FIN | RSV1 | opcode 1
        (car (read-frame-from-port
              (open-input-string "\xC1\x82\xCA\xFE\xBA\xBE\x00\x00")))))

;; ============================================================
;; fragmentation — wire layer (read-frame-from-port)
;; ============================================================
(test-group "fragmentation - wire layer"
  (test "first text fragment fin=0 round-trip"
        '(frame 1 #f (72 105))
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #x1 '(72 105) fin: #f)))))
          (list (car f) (cadr f) (caddr f) (u8vector->list (cadddr f)))))

  (test "continuation fragment fin=1 round-trip"
        '(frame 0 #t (32 84 104 101 114 101))
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #x0 '(32 84 104 101 114 101))))))
          (list (car f) (cadr f) (caddr f) (u8vector->list (cadddr f)))))

  (test "3-fragment binary message via sequential reads"
        '((frame 2 #f (1 2)) (frame 0 #f (3 4)) (frame 0 #t (5 6)))
        (let* ((wire (string-append
                       (build-frame #x2 '(1 2) fin: #f)
                       (build-frame #x0 '(3 4) fin: #f)
                       (build-frame #x0 '(5 6))))
               (port (open-input-string wire))
               (f1 (read-frame-from-port port))
               (f2 (read-frame-from-port port))
               (f3 (read-frame-from-port port)))
          (list (list (car f1) (cadr f1) (caddr f1) (u8vector->list (cadddr f1)))
                (list (car f2) (cadr f2) (caddr f2) (u8vector->list (cadddr f2)))
                (list (car f3) (cadr f3) (caddr f3) (u8vector->list (cadddr f3))))))

  (test "ping frame interleaved between two data fragments"
        '((frame 1 #f (65)) (frame 9 #t (80)) (frame 0 #t (66)))
        (let* ((wire (string-append
                       (build-frame #x1 '(65) fin: #f)
                       (build-frame #x9 '(80))
                       (build-frame #x0 '(66))))
               (port (open-input-string wire))
               (f1 (read-frame-from-port port))
               (f2 (read-frame-from-port port))
               (f3 (read-frame-from-port port)))
          (list (list (car f1) (cadr f1) (caddr f1) (u8vector->list (cadddr f1)))
                (list (car f2) (cadr f2) (caddr f2) (u8vector->list (cadddr f2)))
                (list (car f3) (cadr f3) (caddr f3) (u8vector->list (cadddr f3))))))

  (test "reserved data opcode 3 -> protocol-error 1002"
        '(protocol-error 1002)
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #x3 '(0))))))
          (list (car f) (cadr f))))

  (test "reserved data opcode 5 -> protocol-error 1002"
        '(protocol-error 1002)
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #x5 '(0))))))
          (list (car f) (cadr f))))

  (test "reserved data opcode 7 -> protocol-error 1002"
        '(protocol-error 1002)
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #x7 '(0))))))
          (list (car f) (cadr f))))

  (test "reserved control opcode B -> protocol-error 1002"
        '(protocol-error 1002)
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #xB '())))))
          (list (car f) (cadr f))))

  (test "reserved control opcode D -> protocol-error 1002"
        '(protocol-error 1002)
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #xD '())))))
          (list (car f) (cadr f))))

  (test "reserved control opcode F -> protocol-error 1002"
        '(protocol-error 1002)
        (let ((f (read-frame-from-port
                  (open-input-string (build-frame #xF '())))))
          (list (car f) (cadr f)))))

;; ============================================================
;; fragmentation — state machine (step-fragment-state)
;; ============================================================
(test-group "fragmentation - step-fragment-state"
  (test "idle + text fin=1 -> deliver"
        '(deliver 1 (72 105))
        (let-values (((s a)
                      (step-fragment-state (vector #f '() 0 utf8-accept)
                                           #x1 #t (u8vector 72 105))))
          (list (car a) (cadr a) (u8vector->list (caddr a)))))

  (test "idle + text fin=1 -> state cleared"
        '#(#f () 0 0)
        (let-values (((s a)
                      (step-fragment-state (vector #f '() 0 utf8-accept)
                                           #x1 #t (u8vector 72 105))))
          s))

  (test "idle + binary fin=1 -> deliver binary"
        '(deliver 2 (1 2 3))
        (let-values (((s a)
                      (step-fragment-state (vector #f '() 0 utf8-accept)
                                           #x2 #t (u8vector 1 2 3))))
          (list (car a) (cadr a) (u8vector->list (caddr a)))))

  (test "idle + text fin=0 -> continue"
        '(continue)
        (let-values (((s a)
                      (step-fragment-state (vector #f '() 0 utf8-accept)
                                           #x1 #f (u8vector 72 105))))
          a))

  (test "idle + text fin=0 -> state holds opcode and bytes"
        '(1 2)
        (let-values (((s a)
                      (step-fragment-state (vector #f '() 0 utf8-accept)
                                           #x1 #f (u8vector 72 105))))
          (list (vector-ref s 0) (vector-ref s 2))))

  (test "idle + continuation -> error 1002"
        '(error 1002)
        (let-values (((s a)
                      (step-fragment-state (vector #f '() 0 utf8-accept)
                                           #x0 #t (u8vector 1 2))))
          (list (car a) (cadr a))))

  (test "active + text frame mid-message -> error 1002"
        '(error 1002)
        (let-values (((s a)
                      (step-fragment-state
                       (vector #x1 (list (u8vector 1)) 1 utf8-accept)
                       #x1 #t (u8vector 2))))
          (list (car a) (cadr a))))

  (test "active + binary frame mid-message -> error 1002"
        '(error 1002)
        (let-values (((s a)
                      (step-fragment-state
                       (vector #x1 (list (u8vector 1)) 1 utf8-accept)
                       #x2 #f (u8vector 2))))
          (list (car a) (cadr a))))

  (test "active + continuation fin=0 -> continue, prepended newest-first"
        '(continue 1 (3 4) (1) (2) 4)
        (let-values (((s a)
                      (step-fragment-state
                       (vector #x1 (list (u8vector 1) (u8vector 2)) 2 utf8-accept)
                       #x0 #f (u8vector 3 4))))
          (list (car a)
                (vector-ref s 0)
                (u8vector->list (car (vector-ref s 1)))
                (u8vector->list (cadr (vector-ref s 1)))
                (u8vector->list (caddr (vector-ref s 1)))
                (vector-ref s 2))))

  (test "active + continuation fin=1 -> deliver assembled (text)"
        '(deliver 1 (1 2 3 4))
        (let-values (((s a)
                      (step-fragment-state
                       (vector #x1 (list (u8vector 2) (u8vector 1)) 2 utf8-accept)
                       #x0 #t (u8vector 3 4))))
          (list (car a) (cadr a) (u8vector->list (caddr a)))))

  (test "active + continuation fin=1 (binary) preserves opcode"
        '(deliver 2 (1 2 3))
        (let-values (((s a)
                      (step-fragment-state
                       (vector #x2 (list (u8vector 1)) 1 utf8-accept)
                       #x0 #t (u8vector 2 3))))
          (list (car a) (cadr a) (u8vector->list (caddr a)))))

  (test "active + continuation fin=1 -> state cleared after deliver"
        '#(#f () 0 0)
        (let-values (((s a)
                      (step-fragment-state
                       (vector #x1 (list (u8vector 1)) 1 utf8-accept)
                       #x0 #t (u8vector 2))))
          s))

  (test "empty-payload first fragment (fin=0, len=0) tracks state"
        '(2 0)
        (let-values (((s a)
                      (step-fragment-state (vector #f '() 0 utf8-accept)
                                           #x2 #f (u8vector))))
          (list (vector-ref s 0) (vector-ref s 2))))

  (test "frag-bytes + new > max-message-size -> error 1009"
        '(error 1009)
        (parameterize ((websocket-max-message-size 4))
          (let-values (((s a)
                        (step-fragment-state
                         (vector #x2 (list (u8vector 1 2 3)) 3 utf8-accept)
                         #x0 #t (u8vector 4 5))))
            (list (car a) (cadr a)))))

  (test "idle + start fragment exceeding max-message-size -> error 1009"
        '(error 1009)
        (parameterize ((websocket-max-message-size 2))
          (let-values (((s a)
                        (step-fragment-state (vector #f '() 0 utf8-accept)
                                             #x1 #f (u8vector 1 2 3))))
            (list (car a) (cadr a)))))

  (test "fragment count >= max-fragment-count -> error 1009"
        '(error 1009)
        (parameterize ((websocket-max-fragment-count 2))
          (let-values (((s a)
                        (step-fragment-state
                         (vector #x1 (list (u8vector 1) (u8vector 2)) 2 utf8-accept)
                         #x0 #f (u8vector 3))))
            (list (car a) (cadr a)))))

  ;; End-to-end: drive several step calls in sequence to simulate a real
  ;; fragmented message arriving frame-by-frame, then check the final
  ;; assembled payload and that state is cleared.
  (test "3-fragment text message assembled end-to-end"
        '(continue continue (deliver 1 (72 101 108 108 111 32 87 111 114 108 100)) #(#f () 0 0))
        (let*-values (((s0) (vector #f '() 0 utf8-accept))
                      ((s1 a1) (step-fragment-state s0 #x1 #f (u8vector 72 101 108 108 111)))    ; "Hello"
                      ((s2 a2) (step-fragment-state s1 #x0 #f (u8vector 32 87 111)))             ; " Wo"
                      ((s3 a3) (step-fragment-state s2 #x0 #t (u8vector 114 108 100))))          ; "rld"
          (list (car a1)
                (car a2)
                (list (car a3) (cadr a3) (u8vector->list (caddr a3)))
                s3)))

  (test "4-fragment binary message assembled end-to-end"
        '(deliver 2 (1 2 3 4 5 6 7 8))
        (let*-values (((s0) (vector #f '() 0 utf8-accept))
                      ((s1 a1) (step-fragment-state s0 #x2 #f (u8vector 1 2)))
                      ((s2 a2) (step-fragment-state s1 #x0 #f (u8vector 3 4)))
                      ((s3 a3) (step-fragment-state s2 #x0 #f (u8vector 5 6)))
                      ((s4 a4) (step-fragment-state s3 #x0 #t (u8vector 7 8))))
          (list (car a4) (cadr a4) (u8vector->list (caddr a4)))))

  ;; UTF-8 fail-fast: first text fragment is already invalid, so the
  ;; bad bytes are caught on the first step call rather than waiting
  ;; for FIN.
  (test "fail-fast: invalid utf-8 in first text fragment -> error 1007"
        '(error 1007)
        (let-values (((s a)
                      (step-fragment-state (vector #f '() 0 utf8-accept)
                                           #x1 #f (u8vector #xFF #xFE))))
          (list (car a) (cadr a))))

  ;; Boundary that splits a valid 3-byte codepoint (E3 81 | 82).
  ;; First step continues with a non-accept utf8-state; second step
  ;; finishes the codepoint and delivers.
  (test "boundary splits valid codepoint: 1st call continues mid-codepoint"
        #t
        (let-values (((s a)
                      (step-fragment-state (vector #f '() 0 utf8-accept)
                                           #x1 #f (u8vector #xE3 #x81))))
          (and (eq? (car a) 'continue)
               (not (fx= (vector-ref s 3) utf8-accept))
               (not (fx= (vector-ref s 3) utf8-reject)))))

  (test "boundary splits valid codepoint: 2nd call completes and delivers"
        '(deliver 1 (#xE3 #x81 #x82))
        (let*-values (((s1 a1) (step-fragment-state (vector #f '() 0 utf8-accept)
                                                   #x1 #f (u8vector #xE3 #x81)))
                      ((s2 a2) (step-fragment-state s1 #x0 #t (u8vector #x82))))
          (list (car a2) (cadr a2) (u8vector->list (caddr a2)))))

  ;; Boundary splits a codepoint and the continuation in the 2nd
  ;; fragment is invalid (here we follow E3 81 with C0 instead of an
  ;; 0x80-0xBF continuation byte).
  (test "boundary + invalid continuation in 2nd fragment -> error 1007"
        '(error 1007)
        (let*-values (((s1 a1) (step-fragment-state (vector #f '() 0 utf8-accept)
                                                   #x1 #f (u8vector #xE3 #x81)))
                      ((s2 a2) (step-fragment-state s1 #x0 #f (u8vector #xC0))))
          (list (car a2) (cadr a2))))

  ;; FIN arrives while the DFA is still mid-codepoint: the bytes seen
  ;; are individually valid as a prefix but the message ends short.
  (test "message ends mid-codepoint (fin=1, dfa != accept) -> error 1007"
        '(error 1007)
        (let*-values (((s1 a1) (step-fragment-state (vector #f '() 0 utf8-accept)
                                                   #x1 #f (u8vector #xE3 #x81)))
                      ((s2 a2) (step-fragment-state s1 #x0 #t (u8vector))))
          (list (car a2) (cadr a2)))))

;; ============================================================
;; assemble-fragments
;; ============================================================
(test-group "assemble-fragments"
  (test "single fragment"
        '(1 2 3)
        (u8vector->list (assemble-fragments (list (u8vector 1 2 3)) 3)))

  (test "two fragments (newest-first order reversed on assembly)"
        '(1 2 3 4 5)
        (u8vector->list
         (assemble-fragments (list (u8vector 4 5) (u8vector 1 2 3)) 5)))

  (test "three fragments preserve order"
        '(10 20 30 40 50 60)
        (u8vector->list
         (assemble-fragments
          (list (u8vector 50 60) (u8vector 30 40) (u8vector 10 20)) 6)))

  (test "empty-payload fragments contribute nothing"
        '(1 2)
        (u8vector->list
         (assemble-fragments (list (u8vector) (u8vector 1 2) (u8vector)) 2)))

  (test "all-empty fragments produce empty vector"
        '()
        (u8vector->list
         (assemble-fragments (list (u8vector) (u8vector)) 0))))

(test-exit)
