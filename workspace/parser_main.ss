#!/usr/bin/env gxi

(import "parser_library")
(import "parse_types")

(def (main . args)
  (parse-keyword-test))
 ;; (binary-exp-parse-test))

(def (parse-keyword-test)
  (let* ((parser (parse-keyword "If"))
         (input (string->list " If  "))
        ;; (input (string->list " If ")) BUG: need to fix this case where only one instance of WS is blowing up
         (parse-tree '())
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))

(def (parse-ws-test)
  (let* ((parser (parse-ws))
         (input (string->list "  "))
         (parse-tree '())
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))

(def (if-exp-parse-test)
  (let* ((parser (parse-if-expr))
         (input (string->list "If55=22Then13Else55Fi"))
     ;;    (input (string->list "If55=22Then13ElseABCFi"))
     ;;    ABC and Fi get parsed together as one identifier.
     ;;    Need to introduce whitespace?
         (parse-tree '())
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))


(def (peek-phrase-test)
  (let* ((peeker (peek-phrase "If"))
         (input (string->list "If"))
         (parse-tree '())
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser peeker parse-stream)))


(def (binary-exp-parse-test)
  (let* ((parser (parse-expression #f))
         (input (string->list "128+a2bc+123"))
         (parse-tree '())
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))


(def (empty-parse-test)
  (let* ((parser (peek-empty))
         (input (string->list "a"))
         (parse-tree '())
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))


(def (expression-parse-test)
  (let* ((parser (parse-expression))
         (input (string->list "abc123"))
         (parse-tree '((3 2 4) 4 3))
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))

(def (identifier-parse-test)
  (let* ((parser (parse-identifier))
         (input (string->list "_412Rerfj"))
         (parse-tree '((3 2 4) 4 3))
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))

(def (integer-parse-test)
  (let* ((parser (parse-integer))
         (input (string->list "123"))
         (parse-tree '((3 2 4) 4 3))
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))

(def (string-parse-test)
  (let* ((parser (parse-string "coolness"))
         (input (string->list "\"coolness\""))
         (parse-tree '())
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))
