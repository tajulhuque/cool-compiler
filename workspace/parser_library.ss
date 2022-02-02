#!/usr/bin/env gxi

;; parse "stream": tree we are building up along with frontier of unparsed characters
(defstruct parse-stream (parse-tree input-stream))
(defstruct parse-fail (msg))

;; parse-to-types
(defstruct binary-exp (left-exp op right-exp))
(defstruct if-exp (test-exp true-exp false-exp))

(def (main . args)
  (let* ((parser (parse-string "coolness"))
         (input (string->list "coolness"))
         (parse-tree '())
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))

(def (run-parser parser stream)
  (let (parse-result (parser stream))
    ;;(displayln parser)
    (match parse-result
      ((parse-stream parse-tree input-stream) (displayln "Success! Parse Tree: ") (displayln parse-tree) parse-tree)
      ((parse-fail msg) (displayln msg) '())
      (else (displayln "??") '()))))

(def (parse-string str)
  (def parser
    (let (str-chars (string->list str))
      (parser-combine (map parse-char str-chars))))
  parser)

(def (parse-char char)
  (def (parser stream)
    (displayln "stream:")
    (displayln (parse-stream-input-stream stream))
    (displayln (parse-stream-parse-tree stream))
    (displayln "now parsing:")
    (displayln char)
    (match stream
      ((parse-stream parse-tree input-stream)
       (if (equal? (car input-stream) char)
         (make-parse-stream (cons char parse-tree) (cdr input-stream))
         (make-parse-fail (string-append "PARSE FAIL:" "expected " (string char)))))
      (else (make-parse-fail ""))))
  parser)


;; combinators
;; need to make a few different variants,
;; pipeline, alternate, etc.
;; maybe put them in their own gerbil package
;; and import
;;
;; possilbe to make seperate combinator
;; library that is decoupled from parsing stuff,
;; and then use that in terms of parsing?
;; Going with the coupled version for now...

(def (parser-combine parsers)
  (def combined-parser
    (fold parser-compose (lambda (x) x) (reverse parsers)))
  combined-parser)

(def (parser-compose parser1 parser2)
  (def (parser stream)
    (let (first-result (parser1 stream))
      (match first-result
        ((parse-stream parse-tree input-stream) (parser2 first-result))
        (else first-result))))
  parser)

;; (def (parser-alternate parser1 parser2)

;; todo: need parser-alt , to succeed if one of the two parsers succeed
;; todo need parse-any, to succeed if any of a list of parsers succeed.

;; 1/7/2022
;; Need to play around about with structures, either "poor-mans" or real thing.
;; Basicall I need to return not just the input left to parse but a structure representing WHAT I Parsed

;;

;; want to return list of remaining characters to parse
;; upon successful parse, but what to return on error?
;; maybe a token symbolizing error?

;; advanced goal:
;; parse expressions, and return the cool ast_node
;; expressions, housed under a simple program root
;; and single class tree makeup object.  Start from there
;; and see if you can return a tree object like that
;; and have it print out, and be able to run it
;; on all types of COOL Expressions
;;
;; my parsing functions need to be generators
;; that return functions
;;
;;


(def (compose f g)
  (lambda args
    (g (apply f args))))
