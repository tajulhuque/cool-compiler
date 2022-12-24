#!/usr/bin/env gxi


;; Main: Entry point of program ;;;;;;
(def (main . args)
  (parse-test-new))

;; Main structures ;;;;;;;;;;;;;

;; Parse "stream": tree we are building up along with frontier of unparsed characters.
;; It feels awkward that I have parse-tree and input-stream in the same structure.
;; Scheme does have (values) where you can return multiple results from functions,
;; but for simplicity I want to stay away from them.  What I'm thinking to
;; do is rename this as "parse-progress" -- I think that captures what
;; it is all about better.  I like the word "frontier" that is used in the literature, too; the
;; charaters that are still left to consume, so I may rename "input-stream" to char-frontier, input-frontier,
;; or something like that.  "input-stream" might be good enough though.  But I really like parse-progress
;; for the name of the structure
(defstruct parse-stream (parse-tree input-stream))


;; Structure to return when a parse fails.
;; 9/17/22: Error reporting still needs some proving out
(defstruct parse-fail (msg))

;; parse-to-types
(defstruct expression (exp))
(defstruct int-literal (value))
(defstruct string-literal (value))
(defstruct identifier (name))
(defstruct binary-exp (left-exp op right-exp))
(defstruct if-exp (test-exp true-exp false-exp))
(defstruct identifier (name))


;; Visualizers

(def (print-parse-tree-stack tree-stack)
  (displayln "----")
  (map (lambda (elem)
         (if (list? elem)
           (print-parse-tree-stack elem)
           (print-parse-tree-node elem)))
       tree-stack)
  (displayln "")
  (displayln "----"))


(def (print-parse-tree-node node)
  (match node
    ((int-literal value) (print (string-append "INT<" (number->string value) ">")))
    ((string-literal value) (print (string-append "STRING<" value ">")))
    ((identifier name) (print (string-append "ID<" name ">")))
    ((expression exp)
     (print "EXP [" )
     (print-parse-tree-node exp)
     (print "]"))
    (else (print node)))
  (print " "))

;; 09/18/2022: Starting to get to the point where I need to learn
;; Gerbil/Gambit's debugging support.  Need a stacktrace at present.

;; TEST Functions
;;
(def (binary-exp-parse-test)
  (let* ((parser (parse-binary-exp [#\+]))
         (input (string->list "123+abc$"))
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

;; End Test Methods


;; Debugging Utilities ;;;;;;;;;;;;;;;
(def (trace-msg msg stream)
  (displayln msg)
  (match stream
    ((parse-stream tree input-stream)
     (displayln "Tree: ")
     (displayln tree)
     (displayln "Input: ")
     (displayln input-stream)
     (displayln ""))))

;; Parser Runner ;;;;;;;;;
;;
(def (run-parser parser stream)
  (let (parse-result (parser stream))
    ;;(displayln parser)
    (match parse-result
      ((parse-stream parse-tree input-stream)
       (displayln "SUCCESSFUL PARSE")
       (displayln "Final Parse Progress Result: (tree, input)")
       (displayln parse-tree)
       (displayln input-stream)
       (displayln "Parse Tree: ")
       (print-parse-tree-stack parse-tree)
       parse-tree)
      ((parse-fail msg)
       (displayln "PARSE FAIL.  Error Message: ")
       (displayln msg))
      (else (displayln "??") '()))))


;; Start Of Parser "Library" ;;;;;;;;;

;; This one is the heart of it all, and will
;; of course evolve and grow as we add more pieces of it.
;; Note that COOL specific stuff probably wouldn't belong in
;; this "library" module... but abstracting out of specific
;; vs general domain will come much later... not at that state yet.
(def (parse-expression)
  (def (parser stream)
    (trace-msg ".....parse-expression" stream)
    (parse-to-tree-node
     (parse-any-of
      [(parse-identifier)
       (parse-integer)])
     (lambda (parse-result-tree)
       (make-expression parse-result-tree))
     stream))
  parser)


;; NOW with this one... the recursive definition aspect
;; starts to unfold! Expressions referring to Expressions!
;;We are getting deep now, mechanism of pushing, and popping
;;to construct node on tree is really put to the test now here.
;;We can't just do parse-tree-to-node here.
;;There are THREE Seperate results parse results to look for here.
;;LEFT EXP, OP, RIGHT EXP

(def (parse-binary-exp operators)
  (def (parser stream)
    (trace-msg "..parse-binary-exp" stream)
    (parse-to-tree-node
     (parse-pipeline
      [(parse-expression)
       (parse-any-char operators)
       (parse-expression)])
     (lambda (operands-between-operator)
       (make-expression operands-between-operator))
     stream))
  parser)


(def (parse-identifier)
  (def (parser stream)
    (trace-msg "..........parse-identifier" stream)
    (parse-to-tree-node
     (parse-pipeline
      [(parse-valid-identifier-non-digit-char)
       (parser-repeat (parse-valid-identifier-char))])
     (lambda (parse-result-tree)
       (make-identifier (list->string parse-result-tree)))
     stream))
  parser)


(def (parse-integer)
  (def (parser stream)
    (trace-msg "..........parse-integer" stream)
    (parse-to-tree-node
     (parser-repeat (parse-digit))
     (lambda (parsed-digits-tree)
       (make-int-literal (digit-list->number parsed-digits-tree)))
     stream))
  parser)


    ;;(let* ((digits-parser (parser-repeat (parse-digit)))
     ;;      (prepped-stream (push-parse-tree [] stream))
      ;;     (digits-parse-result (digits-parser prepped-stream)))
      ;;(match digits-parse-result
;        ((parse-stream tree input) (pop-build-push digits-parse-result tree
;;                                                   (lambda (parsed-digits-tree)
 ;;                                                    (make-int-literal (digit-list->number parsed-digits-tree)))))


         ;;(let* ((parsed-digits-branch (car tree))
         ;;      (parsed-int-literal (make-int-literal (digit-list->number parsed-digits-branch))))
         ;; (push-parse-tree parsed-int-literal (pop-parse-tree digits-parse-result))))
;;         (else digits-parse-result))))
;;  parser)

(def (parse-valid-identifier-non-digit-char)
  (parse-any-char (string->list "_$-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnophijklmnopqrstuvwxyz")))

(def (parse-valid-identifier-char)
  (parse-any-char (string->list "_$-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnophijklmnopqrstuvwxyz0123456789")))

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
    ;;(displayln "stream:")
   ;; (displayln (parse-stream-input-stream stream))
    ;;(displayln (parse-stream-parse-tree stream))
    ;;(displayln "now parsing:")
   ;; (displayln char)
    (match stream
      ((parse-stream parse-tree input-stream)
       (if (and (not (null? input-stream))
                (equal? (car input-stream) char))
         (make-parse-stream (append-car char parse-tree) (cdr input-stream))
         (make-parse-fail (string-append "PARSE FAIL:" "expected " (string char)))))
      (else (make-parse-fail ""))))
  parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; trying out the new stack idea
;;


;; ;; new idea - integers

;; gonna try integers now after strings,
;; then see if can parse combinations of strings and integers
;; using this new modified stack idea
;;
;;

(def (parse-test-new)
  (let* ((parser (parse-foo-int-blah))
         (input-str "foo7071882092311blah")
         (input (string->list input-str))
         (parse-tree [['x 'y]])
         (parse-stream (make-parse-stream parse-tree input)))
    (displayln "")
    (displayln "")
    (displayln (string-append "Original input: " input-str))
    (run-parser parser parse-stream)))

(def (parse-foo-int-blah)
  (parse-pipeline
   [(parse-string-new "foo")
    (parse-integer-new)
    (parse-string-new "blah")]))

(def (parse-integer-new)
  (def (parser stream)
    (let* ((digits-parser (parser-repeat (parse-digit-new)))
           (digits-parse-stream (make-parse-stream '() (parse-stream-input-stream stream)))
           (digits-parse-result (digits-parser digits-parse-stream)))
      (match digits-parse-result
        ((parse-stream parse-tree input-stream) (make-parse-stream
                                                 (cons
                                                  (make-int-literal (digit-list->number (reverse parse-tree)))
                                                  (parse-stream-parse-tree stream))
                                                 input-stream))
        (else (make-parse-fail "failed to parse integer")))))
  parser)

(def (parse-string-new str)
  (def (parser stream)
    (let* ((str-chars (string->list str))
           (characters-parser (parse-pipeline (map parse-char-new str-chars)))
           (sub-tree-stream (make-parse-stream '() (parse-stream-input-stream stream)))
           (characters-parser-result (characters-parser sub-tree-stream)))
      (match characters-parser-result
        ((parse-stream parse-tree input-stream) (make-parse-stream
                                                 (cons
                                                  (make-string-literal (list->string (reverse parse-tree)))
                                                  (parse-stream-parse-tree stream))
                                                 input-stream))
        (else (make-parse-fail (string-append "failed to parse string \"" str "\""))))))
  parser)


(def (parse-digit-new)
  (parse-any-char-new (string->list "0123456789")))


(def (parse-any-char-new chars)
  (parse-any-of (map parse-char-new chars)))


 ;;;;; new idea - strings


;;; later: hmmm need a Pop-N?

(def (parse-char-new char)
  (def (parser stream)
    ;;(displayln "stream:")
   ;; (displayln (parse-stream-input-stream stream))
    ;;(displayln (parse-stream-parse-tree stream))
    ;;(displayln "now parsing:")
   ;; (displayln char)
    (match stream
      ((parse-stream parse-tree input-stream)
       (if (and (not (null? input-stream))
                (equal? (car input-stream) char))
         (make-parse-stream (cons char parse-tree) (cdr input-stream))
         (make-parse-fail (string-append "PARSE FAIL:" "expected " (string char)))))
      (else (make-parse-fail ""))))

  parser)

;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;



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
        parse-result)))
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

;; COMPOSE CONCEPT ;;;;;;
(def (compose f g)
  (lambda args
    (g (apply f args))))


;; special parse-tree helpers ;;;;;;;;;;;;;;;



(def (parse-to-tree-node parser node-builder stream )
  "Runs the specified parser against the stream and builds the result as a new node on the parse tree"
  (let* ((prepped-stream (push-parse-tree [] stream))
         (parse-result (parser prepped-stream)))
    (match parse-result
      ((parse-stream tree input) (pop-build-push parse-result node-builder))
      (else parse-result))))

;; I think I need to explore combining with PREVIOUS LEVEL's tree
;; instead of pushing built node to the top level.
;; Because that top of stack is really to hold intermediate raw parse results,
;; and end goal is to pop that away and build onto the main tree with node
;; we built from intermediate raw result

(def (pop-build-push stream node-builder)
  "pop the parse-tree, use the popped to build node, then push that new node back"
  (let* ((popped-stream (pop-parse-tree stream))
         (popped-stream-tree (car popped-stream))
         (popped-stream-stream (car (cdr popped-stream)))
         (node (node-builder popped-stream-tree)))
    (push-parse-tree [node] popped-stream-stream)))

(def (pop-parse-tree stream)
  "pops the parse-tree stack off the parse-stream, returns as new parse-stream"
  (match stream
    ((parse-stream parse-tree input-stream)
     [(car parse-tree)
      (make-parse-stream (cdr parse-tree) input-stream)])
    (else stream)))

(def (push-parse-tree tree stream)
  "pushes specified tree onto parse-stream's parse-tree stack and returns as new parse-stream"
  (match stream
    ((parse-stream parse-tree input-stream) (make-parse-stream (cons tree parse-tree) input-stream))
    (else stream)))


;; miscelaneous helpers ;;;;;;;;;

(def (append-car x lst)
  "append x to the car of the list"
  (cons (append  (if (null? lst) [] (car lst)) (list x)) (cdr lst)))

(def (digit-list->number digit-list)
  "converts a number represented by list of digit characters to the actual number value"
  (string->number
   (list->string digit-list)))
