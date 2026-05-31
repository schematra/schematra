;; Schematra WebSocket support — public API.
;;
;; The implementation lives in schematra.ws.internal, which also exports
;; the protocol-level internals (frame parser, unmask, close-payload
;; parser, websocket-connection accessors) so test code can reach them.
;; Application code imports this module and sees only the curated surface.

(module schematra.ws
    (websocket
     websocket*
     send-text
     send-binary
     close-websocket!
     current-websocket
     websocket-max-frame-size
     websocket-max-message-size
     websocket-max-fragment-count)
  (import scheme chicken.module schematra.ws.internal)
  (reexport (only schematra.ws.internal
                  websocket
                  websocket*
                  send-text
                  send-binary
                  close-websocket!
                  current-websocket
                  websocket-max-frame-size
                  websocket-max-message-size
                  websocket-max-fragment-count)))
