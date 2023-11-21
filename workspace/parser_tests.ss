

(import "parser_library")

(export binary-exp-parse-test2)
(export binary-exp-parse-test)
(export expression-parse-test)


(def (binary-exp-parse-test)
  (let* ((parser (parse-expression))
         (input (string->list "128*a2bc_"))
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
