(export trace-push)
(export trace-pop)
(export trace-parser-enter)
(export trace-parser-success)
(export trace-parser-fail)
(export trace-write-line)
(export char-list->trace-string)

;; Structured tracing state.  Parser code should not mutate this directly;
;; use trace-push and trace-pop around nested parser calls.
(def *trace-depth* 0)

(def (trace-push)
  (set! *trace-depth* (1+ *trace-depth*)))

(def (trace-pop)
  (set! *trace-depth* (- *trace-depth* 1)))

(def (trace-indent)
  (def (loop n result)
    (if (equal? n 0)
      result
      (loop (- n 1) (string-append result "  "))))
  (loop *trace-depth* ""))

(def (input-consumed before after)
  (def (consume acc rest)
    (cond ((equal? rest after) (reverse acc))
          ((null? rest) (reverse acc))
          (#t (consume (cons (car rest) acc) (cdr rest)))))
  (consume '() before))

(def (char-list->trace-string chars)
  (def (escape-char char)
    (cond ((equal? char #\newline) "\\n")
          ((equal? char #\tab) "\\t")
          (else (string char))))
  (def (loop remaining result)
    (if (null? remaining)
      result
      (loop (cdr remaining) (string-append result (escape-char (car remaining))))))
  (string-append "\"" (loop chars "") "\""))

(def (trace-write-line label value)
  (display (trace-indent))
  (display label)
  (write value)
  (displayln ""))

(def (trace-parser-enter name input-stream)
  (displayln (trace-indent) "-> " name)
  (displayln (trace-indent) "   input: " (char-list->trace-string input-stream)))

(def (trace-parser-success name input-before input-after raw-tree node)
  (displayln (trace-indent) "<- " name " OK")
  (displayln (trace-indent) "   consumed: " (char-list->trace-string (input-consumed input-before input-after)))
  (displayln (trace-indent) "   remaining: " (char-list->trace-string input-after))
  (trace-write-line "   raw-tree: " raw-tree)
  (trace-write-line "   node: " node)
  (displayln ""))

(def (trace-parser-fail name input-before msg)
  (displayln (trace-indent) "<- " name " FAIL")
  (displayln (trace-indent) "   input: " (char-list->trace-string input-before))
  (displayln (trace-indent) "   reason: " msg)
  (displayln ""))
