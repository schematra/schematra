#!/usr/bin/env csi -s
;; Schematra Utils Test Suite

(import scheme)
(import
 chicken.base
 chicken.fixnum
 chicken.io
 chicken.port
 srfi-4
 (rename intarweb (headers intarweb:headers))
 schematra.utils
 test)

;; ============================================================
;; hex-pair->char
;; ============================================================
(test-group "hex-pair->char"
  (test "low byte 00" #\nul (hex-pair->char "00" 0))
  (test "byte ff"     #\xff (hex-pair->char "ff" 0))
  (test "uppercase FF" #\xff (hex-pair->char "FF" 0))
  (test "mixed case Ab" #\xab (hex-pair->char "Ab" 0))
  (test "reads at offset" #\xcd (hex-pair->char "abCD" 2))
  (test "ascii letter from hex" #\A (hex-pair->char "41" 0)))

;; ============================================================
;; hex-string->byte-string
;; ============================================================
(test-group "hex-string->byte-string"
  (test "empty input"  "" (hex-string->byte-string ""))
  (test "single byte"  "\x00" (hex-string->byte-string "00"))
  (test "two bytes"    "AB" (hex-string->byte-string "4142"))
  (test "case-insensitive"
        (hex-string->byte-string "deadbeef")
        (hex-string->byte-string "DEADBEEF"))
  ;; SHA-1 produces 20 bytes (40 hex chars). Used by websocket-accept-key.
  (test "sha1-sized round trip length" 20
        (string-length
         (hex-string->byte-string "a94a8fe5ccb19ba61c4c0873d391e987982fbbd3"))))

;; ============================================================
;; header-value->string
;; ============================================================
(test-group "header-value->string"
  (test "#f becomes empty string" "" (header-value->string #f))
  (test "string passes through"   "websocket" (header-value->string "websocket"))
  (test "symbol -> string"         "websocket" (header-value->string 'websocket))
  (test "list joined with commas"  "a,b,c"
        (header-value->string '("a" "b" "c")))
  (test "list of symbols"          "Upgrade,keep-alive"
        (header-value->string '(Upgrade keep-alive)))
  ;; intarweb sometimes wraps token values as (token . params) pairs.
  (test "pair uses car"            "websocket"
        (header-value->string '("websocket" . #f)))
  (test "number coerced"           "42"
        (header-value->string 42)))

;; ============================================================
;; header-token?
;; ============================================================
(test-group "header-token?"
  (let ((h (intarweb:headers '((upgrade websocket)
                               (connection "Upgrade")
                               (accept-encoding "gzip, deflate, br")))))
    (test "matches simple token"          #t
          (and (header-token? 'upgrade "websocket" h) #t))
    (test "case-insensitive expected"     #t
          (and (header-token? 'upgrade "WebSocket" h) #t))
    (test "case-insensitive header value" #t
          (and (header-token? 'connection "upgrade" h) #t))
    (test "matches token in comma list"   #t
          (and (header-token? 'accept-encoding "deflate" h) #t))
    (test "trims whitespace"              #t
          (and (header-token? 'accept-encoding "gzip" h) #t))
    (test "missing header returns #f"     #f
          (header-token? 'x-custom "anything" h))
    (test "token not present returns #f"  #f
          (header-token? 'upgrade "h2c" h))))

;; ============================================================
;; read-u8 / write-u8
;; ============================================================
(test-group "read-u8 / write-u8"
  (test "round-trip single byte" 65
        (let ((out (open-output-string)))
          (write-u8 65 out)
          (read-u8 (open-input-string (get-output-string out)))))
  (test "high byte 255 round-trip" 255
        (let ((out (open-output-string)))
          (write-u8 255 out)
          (read-u8 (open-input-string (get-output-string out)))))
  (test "write-u8 truncates to low 8 bits" 1
        (let ((out (open-output-string)))
          (write-u8 257 out)
          (read-u8 (open-input-string (get-output-string out)))))
  (test "read-u8 at EOF returns eof"
        #t
        (eof-object? (read-u8 (open-input-string ""))))
  (test "sequential reads" '(1 2 3)
        (let ((p (open-input-string "\x01\x02\x03")))
          (list (read-u8 p) (read-u8 p) (read-u8 p)))))

;; ============================================================
;; read-byte-string
;; ============================================================
(test-group "read-byte-string"
  (test "exact length succeeds" "abcd"
        (read-byte-string (open-input-string "abcdef") 4))
  (test "consumes only requested bytes" "ab"
        (let ((p (open-input-string "abcdef")))
          (read-byte-string p 2)))
  (test "short read returns #f" #f
        (read-byte-string (open-input-string "ab") 4))
  (test "empty input zero length" ""
        (read-byte-string (open-input-string "") 0)))

;; ============================================================
;; read-u8vector-exact
;; ============================================================
(test-group "read-u8vector-exact"
  (test "exact length returns u8vector" #t
        (u8vector? (read-u8vector-exact (open-input-string "abcd") 4)))
  (test "exact bytes" '(97 98 99 100)
        (u8vector->list (read-u8vector-exact (open-input-string "abcd") 4)))
  (test "high byte preserved" '(255 0 128)
        (u8vector->list (read-u8vector-exact (open-input-string "\xff\x00\x80") 3)))
  (test "short read returns #f" #f
        (read-u8vector-exact (open-input-string "ab") 4))
  (test "zero length returns empty u8vector" '()
        (u8vector->list (read-u8vector-exact (open-input-string "") 0))))

;; ============================================================
;; read-network-integer
;; ============================================================
(test-group "read-network-integer"
  (test "2-byte big-endian 0x0102 = 258" 258
        (read-network-integer (open-input-string "\x01\x02") 2))
  (test "2-byte big-endian 0xffff" 65535
        (read-network-integer (open-input-string "\xff\xff") 2))
  (test "2-byte zero" 0
        (read-network-integer (open-input-string "\x00\x00") 2))
  (test "8-byte big-endian 1" 1
        (read-network-integer
         (open-input-string "\x00\x00\x00\x00\x00\x00\x00\x01") 8))
  (test "8-byte big-endian 0x0102030405060708" #x0102030405060708
        (read-network-integer
         (open-input-string "\x01\x02\x03\x04\x05\x06\x07\x08") 8))
  (test "truncated input returns eof"
        #t
        (eof-object? (read-network-integer (open-input-string "\x01") 2))))

;; ============================================================
;; utf8 dfa (utf8-decode-step / utf8-decode-feed)
;; ============================================================
(test-group "utf8 dfa"
  (test "empty feed stays at accept"
        #t (fx= utf8-accept (utf8-decode-feed utf8-accept (u8vector))))

  (test "pure ASCII run stays at accept"
        #t (fx= utf8-accept (utf8-decode-feed utf8-accept
                              (u8vector 72 101 108 108 111))))

  ;; Mid-codepoint preservation across feeds: leading byte of 2-byte
  ;; sequence leaves us in a non-accept, non-reject state; the second
  ;; byte (continuation) lands back at accept.
  (test "split 2-byte: state after lead is mid-codepoint"
        #t
        (let ((s (utf8-decode-feed utf8-accept (u8vector #xC3))))
          (and (not (fx= s utf8-accept))
               (not (fx= s utf8-reject)))))

  (test "split 2-byte: continuation byte reaches accept"
        #t
        (let* ((s1 (utf8-decode-feed utf8-accept (u8vector #xC3)))
               (s2 (utf8-decode-feed s1 (u8vector #xA9))))
          (fx= s2 utf8-accept)))

  ;; Split 3-byte sequence across two feeds (E3 81 | 82 → U+3042).
  (test "split 3-byte across two feeds reaches accept"
        #t
        (let* ((s1 (utf8-decode-feed utf8-accept (u8vector #xE3 #x81)))
               (s2 (utf8-decode-feed s1 (u8vector #x82))))
          (fx= s2 utf8-accept)))

  ;; Reject is sticky: once we hit a bad byte, valid ASCII after it
  ;; cannot recover the state.
  (test "reject is sticky across feeds"
        #t
        (let* ((s1 (utf8-decode-feed utf8-accept (u8vector #xFF)))
               (s2 (utf8-decode-feed s1 (u8vector 65 66 67))))
          (fx= s2 utf8-reject)))

  ;; Surrogate U+D800 encoded as ED A0 80. Split as (ED A0) | (80) —
  ;; the surrogate range is caught at the 2nd byte, not the 3rd.
  (test "surrogate ED A0 80 rejects at the 2nd byte (split feed)"
        #t
        (let ((s (utf8-decode-feed utf8-accept (u8vector #xED #xA0))))
          (fx= s utf8-reject)))

  (test "overlong NUL C0 80 rejects immediately"
        #t
        (fx= utf8-reject (utf8-decode-feed utf8-accept (u8vector #xC0 #x80)))))

;; ============================================================
(test-exit)
