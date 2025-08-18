#!/usr/bin/env csi -s
;; Chiccup Test Suite
;; Simple test framework and test cases for chiccup functionality

(import chiccup srfi-1 chicken.string chicken.process-context chicken.format)

;; Simple test framework
(define *test-count* 0)
(define *test-passed* 0)
(define *test-failed* 0)

(define (test-equal name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
        (set! *test-passed* (+ *test-passed* 1))
        (display (format "✓ ~a~n" name)))
      (begin
        (set! *test-failed* (+ *test-failed* 1))
        (display (format "✗ ~a~n" name))
        (display (format "  Expected: ~a~n" expected))
        (display (format "  Actual:   ~a~n" actual)))))

(define (run-tests)
  (display "\n=== Chiccup Test Suite ===\n\n")

  ;; Basic tag tests
  (test-equal "simple div tag"
              "<div></div>"
              (ccup->html `[div]))

  (test-equal "div with text content"
              "<div>hello world</div>"
              (ccup->html `[div "hello world"]))

  (test-equal "nested tags"
              "<div><p>nested</p></div>"
              (ccup->html `[div [p "nested"]]))

  ;; CSS selector tests
  (test-equal "div with class"
              "<div class=\"foo\">content</div>"
              (ccup->html `[div.foo "content"]))

  (test-equal "div with multiple classes"
              "<div class=\"foo bar\">content</div>"
              (ccup->html `[div.foo.bar "content"]))

  (test-equal "div with id"
              "<div id=\"myid\">content</div>"
              (ccup->html `[div#myid "content"]))

  (test-equal "div with class and id"
              "<div class=\"foo\" id=\"myid\">content</div>"
              (ccup->html `[div.foo#myid "content"]))

  (test-equal "class only (default div)"
              "<div class=\"container\">content</div>"
              (ccup->html `[.container "content"]))

  (test-equal "id only (default div)"
              "<div id=\"main\">content</div>"
              (ccup->html `[\#main "content"]))

  ;; Attribute tests
  (test-equal "div with attributes"
              "<div data-test=\"value\">content</div>"
              (ccup->html `[div (@ (data-test "value")) "content"]))

  (test-equal "attributes with class selector"
              "<div class=\"foo\" data-test=\"value\">content</div>"
              (ccup->html `[.foo (@ (data-test "value")) "content"]))

  (test-equal "attributes merge with class"
              "<div class=\"foo override bar\">content</div>"
              (ccup->html `[.foo (@ (class "override bar")) "content"]))

  ;; Void element tests  
  (test-equal "br void element"
              "<br>"
              (ccup->html `[br]))

  (test-equal "img void element with attributes"
              "<img src=\"test.jpg\" alt=\"test\">"
              (ccup->html `[img (@ (src "test.jpg") (alt "test"))]))

  (test-equal "input void element"
              "<input type=\"text\" name=\"username\">"
              (ccup->html `[input (@ (type "text") (name "username"))]))

  ;; Script tag closing tests
  (test-equal "empty script tag closes properly"
              "<script></script>"
              (ccup->html `[script]))

  (test-equal "script tag with src closes properly"
              "<script src=\"app.js\"></script>"
              (ccup->html `[script (@ (src "app.js"))]))

  ;; HTML escaping tests
  (test-equal "HTML characters are escaped in regular tags"
              "<p>&lt; &amp; &gt; &quot; '</p>"
              (ccup->html `[p "< & > \" '"]))

  (test-equal "HTML characters are escaped in code tags"
              "<code>&lt;script&gt;alert(&quot;xss&quot;)&lt;/script&gt;</code>"
              (ccup->html `[code "<script>alert(\"xss\")</script>"]))

  ;; Attribute value escaping tests
  (test-equal "attribute values are properly escaped"
              "<body hx-headers=\"{&quot;x-csrf-token&quot;:&quot;abc123&quot;}\"></body>"
              (ccup->html `[body (@ (hx-headers "{\"x-csrf-token\":\"abc123\"}"))]))

  ;; Boolean attribute tests
  (test-equal "boolean attributes render without values"
              "<input type=\"checkbox\" disabled checked>"
              (ccup->html `[input (@ (type "checkbox") (disabled) (checked))]))

  (test-equal "mixed boolean and value attributes"
              "<button class=\"btn\" type=\"submit\" disabled>Submit</button>"
              (ccup->html `[button (@ (type "submit") (disabled) (class "btn")) "Submit"]))

  ;; Raw content tests (script and style should not escape)
  (test-equal "script content is not escaped"
              "<script>if (x < 5 && y > 3) { console.log(\"hello\"); }</script>"
              (ccup->html `[script "if (x < 5 && y > 3) { console.log(\"hello\"); }"]))

  (test-equal "style content is not escaped"
              "<style>.class { content: \"<>&\"; }</style>"
              (ccup->html `[style ".class { content: \"<>&\"; }"]))

  ;; Raw content with (raw ...) wrapper
  (test-equal "raw content bypasses escaping"
              "<div><em>italic</em></div>"
              (ccup->html `[div (raw "<em>italic</em>")]))

  ;; Complex nested examples
  (test-equal "complex nested structure"
              "<div class=\"container\"><h1 id=\"title\">Hello</h1><p class=\"text\">World</p></div>"
              (ccup->html `[.container 
                            [h1#title "Hello"]
                            [p.text "World"]]))

  (test-equal "form with inputs"
              "<form action=\"/submit\" method=\"post\"><input type=\"text\" name=\"name\"><input type=\"submit\" value=\"Submit\"></form>"
              (ccup->html `[form (@ (action "/submit") (method "post"))
                            [input (@ (type "text") (name "name"))]
                            [input (@ (type "submit") (value "Submit"))]]))

  ;; HTML5 document structure
  (test-equal "html document with doctype"
              "<!doctype html><html><head><title>Test</title></head><body><h1>Hello</h1></body></html>"
              (ccup->html `[html
                            [head [title "Test"]]
                            [body [h1 "Hello"]]]))

  ;; Print test results
  (display "\n=== Test Results ===\n")
  (display (format "Total tests: ~a~n" *test-count*))
  (display (format "Passed: ~a~n" *test-passed*))
  (display (format "Failed: ~a~n" *test-failed*))
  (if (= *test-failed* 0)
      (begin
        (display "🎉 All tests passed!\n")
        (exit 0))
      (begin
        (display (format "💥 ~a test(s) failed!~n" *test-failed*))
        (exit 1))))

;; Run the tests
(run-tests)
