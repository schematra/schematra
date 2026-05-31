;; Schematra framework-internal utilities
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

(module schematra.utils
  (hex-pair->char
   hex-string->byte-string
   header-value->string
   header-token?
   read-u8
   write-u8
   read-byte-string
   read-u8vector-exact
   read-network-integer
   utf8-valid?
   utf8-accept
   utf8-reject
   utf8-decode-step
   utf8-decode-feed)

  (import scheme)
  (import
   chicken.base
   chicken.bitwise
   chicken.blob
   chicken.fixnum
   chicken.io
   chicken.string
   srfi-4
   srfi-13
   (rename intarweb (headers intarweb:headers)))

;; Hex / byte-string helpers

(define (hex-pair->char hex index)
  (integer->char (string->number (substring hex index (+ index 2)) 16)))

(define (hex-string->byte-string hex)
  (list->string
   (let loop ((index 0)
              (bytes '()))
     (if (>= index (string-length hex))
         (reverse bytes)
         (loop (+ index 2) (cons (hex-pair->char hex index) bytes))))))

;; HTTP header helpers

(define (header-value->string value)
  (cond
   ((not value) "")
   ((string? value) value)
   ((symbol? value) (symbol->string value))
   ((list? value) (string-join (map header-value->string value) ","))
   ((pair? value) (header-value->string (car value)))
   (else (->string value))))

(define (header-token? name expected headers)
  (let ((value (string-downcase (header-value->string (header-value name headers #f)))))
    (and (not (string-null? value))
         (member (string-downcase expected)
                 (map string-trim-both (string-split value ","))))))

;; Binary I/O helpers (used by frame-based protocols)

(define (read-u8 port)
  (let ((ch (read-char port)))
    (if (eof-object? ch) ch (char->integer ch))))

(define (write-u8 n port)
  (write-char (integer->char (bitwise-and n #xff)) port))

(define (read-byte-string port length)
  (let ((data (read-string length port)))
    (if (or (eof-object? data) (< (string-length data) length))
        #f
        data)))

(define (read-u8vector-exact port length)
  (let ((data (read-string length port)))
    (if (or (eof-object? data) (< (string-length data) length))
        #f
        (blob->u8vector/shared (string->blob data)))))

(define (read-network-integer port length)
  (let loop ((remaining length)
             (value 0))
    (if (= remaining 0)
        value
        (let ((byte (read-u8 port)))
          (if (eof-object? byte)
              byte
              (loop (- remaining 1) (+ (arithmetic-shift value 8) byte)))))))

;; RFC 3629 UTF-8 validation via Björn Höhrmann's DFA
;; (https://bjoern.hoehrmann.de/utf-8/decoder/dfa/). State is a small
;; integer; 0 = ACCEPT, 12 = REJECT (sticky), other values mean we are
;; mid-codepoint. The two tables encode the full RFC 3629 rules
;; (rejects overlongs, surrogates U+D800-U+DFFF, codepoints > U+10FFFF).
;;
;; Exposing the step/feed primitives lets callers validate streaming
;; byte runs incrementally — e.g. WebSocket fragmented text messages,
;; where RFC 6455 §5.6 requires fail-fast at the offending byte rather
;; than after the full message is assembled.

(define utf8-accept 0)
(define utf8-reject 12)

(define utf8-classifier-table
  (u8vector
   ;; 0x00-0x7F: class 0 (ASCII)
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
   ;; 0x80-0x8F: class 1
   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
   ;; 0x90-0x9F: class 9
   9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9
   ;; 0xA0-0xBF: class 7
   7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
   7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
   ;; 0xC0-0xCF: 8,8 then 2x14 (0xC0,0xC1 are overlong 2-byte leads)
   8 8 2 2 2 2 2 2 2 2 2 2 2 2 2 2
   ;; 0xD0-0xDF: class 2
   2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
   ;; 0xE0-0xEF: 10 (0xE0), 3x12 (0xE1-0xEC), 4 (0xED), 3,3 (0xEE-0xEF)
   10 3 3 3 3 3 3 3 3 3 3 3 3 4 3 3
   ;; 0xF0-0xFF: 11 (0xF0), 6x3 (0xF1-0xF3), 5 (0xF4), 8x11 (0xF5-0xFF invalid)
   11 6 6 6 5 8 8 8 8 8 8 8 8 8 8 8))

(define utf8-transition-table
  (u8vector
   ;; state 0 (ACCEPT)
   0 12 24 36 60 96 84 12 12 12 48 72
   ;; state 12 (REJECT) — sticky, all transitions stay rejected
   12 12 12 12 12 12 12 12 12 12 12 12
   ;; state 24
   12  0 12 12 12 12 12  0 12  0 12 12
   ;; state 36
   12 24 12 12 12 12 12 24 12 24 12 12
   ;; state 48
   12 12 12 12 12 12 12 24 12 12 12 12
   ;; state 60
   12 24 12 12 12 12 12 12 12 24 12 12
   ;; state 72
   12 12 12 12 12 12 12 36 12 36 12 12
   ;; state 84
   12 36 12 12 12 12 12 36 12 36 12 12
   ;; state 96
   12 36 12 12 12 12 12 12 12 12 12 12))

(define (utf8-decode-step state byte)
  (let ((klass (u8vector-ref utf8-classifier-table byte)))
    (u8vector-ref utf8-transition-table (fx+ state klass))))

;; Fold the DFA over a u8vector, starting from `state`. Returns the
;; resulting state. Short-circuits as soon as REJECT is reached so a
;; long run of bytes after a bad sequence costs O(1).
(define (utf8-decode-feed state bv)
  (let ((len (u8vector-length bv)))
    (let loop ((i 0) (s state))
      (cond
       ((fx= s utf8-reject) utf8-reject)
       ((fx>= i len) s)
       (else (loop (fx+ i 1)
                   (utf8-decode-step s (u8vector-ref bv i))))))))

;; Whole-buffer validator. Used by WebSocket text frame handling
;; (RFC 6455 §8.1 mandates close 1007 on invalid UTF-8) for the
;; close-frame reason path; streaming text fragments use the
;; incremental step/feed primitives directly.
(define (utf8-valid? bv)
  (fx= utf8-accept (utf8-decode-feed utf8-accept bv)))

) ;; end module
