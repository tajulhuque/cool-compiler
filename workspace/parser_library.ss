#!/usr/bin/env gxi

;; parse "stream": tree we are building up along with frontier of unparsed characters
(defstruct parse-stream (parse-tree input-stream))
(defstruct parse-fail (msg))

;; parse-to-types
(defstruct int-literal (value))
(defstruct binary-exp (left-exp op right-exp))
(defstruct if-exp (test-exp true-exp false-exp))

(def (main . args)
  (pretty-print [(make-binary-exp 9 '+ 8) (make-int-literal 8) (make-int-literal 42)]))
  ;;(repeat-test))


(def (string-parse-test)
  (let* ((parser (parse-string "coolness"))
         (input (string->list "coolness"))
         (parse-tree '())
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))

(def (alt-test)
  (let* ((parser (parser-compose-alternate
                  (parse-string "coolness")
                  (parse-string "taj")))
         (input (string->list "taj"))
         (parse-tree '())
         (parse-stream (make-parse-stream parse-tree input)))
    (run-parser parser parse-stream)))

(def (repeat-test)
  (let* ((parser (parser-repeat (parse-letter)))
         (input (string->list "mmmgdf1123a"))
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

;;(def (parse-integer)
  ;;(def (parser stream)
    ;;(let* ((digits-parser (parser-repeat (parse-digit)))
      ;;     (digits-parse-result (digits-parser stream)))
      ;;(match digits-parse-result
        ;;((parse-stream parse-tree input-stream) (make-parse-stream
          ;;                                       (cons =


(def (parse-digit)
  (parse-any-char (string->list "0123456789")))

(def (parse-letter)
  (def parser
    (parse-any-char (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnophijklmnopqrstuvwxyz")))
  parser)


(def (parse-any-char chars)
  (def parser
      (parse-any-of (map parse-char chars)))
  parser)

(def (parse-string str)
  (def parser
    (let (str-chars (string->list str))
      (parse-pipeline (map parse-char str-chars))))
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

(def (parse-pipeline parsers)
  (parser-combine parsers parser-compose-follow))

(def (parse-any-of parsers)
  (parser-combine parsers parser-compose-alternate))

;; todo: parser-combine need to make safe if passed 1 parser?
(def (parser-combine parsers composer)
  (def combined-parser
    (let (reversed-parsers (reverse parsers))
      (fold composer (car reversed-parsers) (cdr reversed-parsers))))
  combined-parser)

;; parser-repeat - need to make parser parse
;; over and over again until it finally fails,
;; basically greedy-parse as much as it can.
;; Should this be in the combinater style?
;; Takes a parser and produces another parser?
;; Probaby yes.
;; Basically, need to run the supplied parser once, if that fails, return parse-fail.
;; Otherwise, keep going as long as you can, returning until failure
;; but in the "keep going phase" you don't return failure, you only stop at failure
(def (parser-repeat parser)
  (def (repeat-parser stream)

    (def (repeat-phase repeat-phase-stream)
      (let (repeat-phase-parse-result (parser repeat-phase-stream))
        (if (parse-fail? repeat-phase-parse-result)
          repeat-phase-stream
          (let (repeat-phase-result-input (parse-stream-input-stream repeat-phase-parse-result))
            (if (null? repeat-phase-result-input)
              repeat-phase-parse-result
              (repeat-phase repeat-phase-parse-result))))))


    (let (parse-result (parser stream))
      (if (parse-stream? parse-result)
        (repeat-phase parse-result)
        (parse-result))))
  repeat-parser)

;; todo: define a more-input? function
;; which would be helpfull to cleanup the above function
;; a bit in the area where it has some extra needed piece
;; that checks if there is any more input to parse
;; (and that might be needed again)

(def (parser-compose-follow parser1 parser2)
  (def (parser stream)
    (let (first-result (parser1 stream))
      (match first-result
        ((parse-stream parse-tree input-stream) (parser2 first-result))
        (else first-result))))
  parser)

(def (parser-compose-alternate parser1 parser2)
  (def (parser stream)
    (let (first-result (parser1 stream))
      (if (parse-stream? first-result)
        first-result
        (parser2 stream))))
  parser)



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
