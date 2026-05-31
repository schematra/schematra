;; Minimal WebSocket echo server for Autobahn TestSuite conformance runs.
;; Text in -> text out, binary in -> binary out, byte-for-byte. Run with:
;;
;;   csc -O2 -d0 examples/websocket-echo.scm && examples/websocket-echo
;;   # or: csi -s examples/websocket-echo.scm
;;
;; Then point Autobahn fuzzingclient at ws://localhost:9001/.

(import
 schematra
 schematra.ws)

;; Autobahn §9 sends multi-MB single frames and 1000+ fragment counts.
;; Crank the limits high enough that conformance — not our DoS knobs —
;; is what we're measuring.
(websocket-max-frame-size    (* 16 1024 1024))   ; 16 MiB per frame
(websocket-max-message-size  (* 64 1024 1024))   ; 64 MiB total
(websocket-max-fragment-count 200000)

(with-schematra-app (schematra/make-app)
  (lambda ()
    (websocket "/"
      (on-open (void))
      (on-text msg (send-text msg))
      (on-binary bytes (send-binary bytes))
      (on-close code reason (void))
      (on-error exn (void)))

    (schematra-install)
    (schematra-start port: 9001)))
