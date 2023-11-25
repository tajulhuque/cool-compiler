

(export parse-expression)
(export parse-binary-exp)
(export parse-integer)
(export parse-identifier)
(export parse-string)
(export run-parser)
(export parse-stream)
(export make-parse-stream)
(export parse-fail)
(export make-parse-fail)
(export parse-empty)

(import "utils")

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
(defstruct sub-expression (exp))
(defstruct int-literal (value))
(defstruct string-literal (value))
(defstruct identifier (name))
(defstruct binary-exp (left-exp op right-exp))

;; Visualizers

(def (print-parse-tree-stack tree-stack)
  (print-parse-tree-node (car tree-stack) 0 "")
  (displayln))

(def (print-node type-label value-printer)
    (display type-label)
    (display ": ")
    ;;(display "<")
    (value-printer))
    ;;(display ">"))

(def (print-simple-node type-label value)
  (print-node type-label
              (lambda ()
                (display value))))

(def (print-parse-tree-node node level tag)

  (displayln)

  (let loop ((n 0))
    (if (< n level)
      (begin
        (display "  ")
        (loop (1+ n)))))

  (when (> (string-length tag) 0)
    (display tag)
    (display ": "))

  (match node
    ((int-literal value) (print-simple-node "INT" value))
    ((string-literal value) (print-simple-node "STRING" value))
    ((identifier name) (print-simple-node "ID" name))
    ((binary-exp left-exp op right-exp)
     (print-node "BINARY-EXP"
                 (lambda ()
                   (print-parse-tree-node left-exp (+ 1 level) "L")
                   (print-parse-tree-node op (+ 1 level) "Op")
                   (print-parse-tree-node right-exp (+ 1 level) "R"))))
    ((expression exp)
     (print-node "EXP"
                 (lambda ()
                   (print-parse-tree-node exp (+ 1 level) ""))))
    ((sub-expression sub-exp) (print-simple-node "SUBEXP" sub-exp))
    (else (display node))))


;; 09/18/2022: Starting to get to the point where I need to learn
;; Gerbil/Gambit's debugging support.  Need a stacktrace at present.



;; Parser Runner ;;;;;;;;;
;;
(def (run-parser parser stream)
  (let (parse-result (parser stream))
    (displayln parse-result)
    (match parse-result
      ((parse-stream parse-tree input-stream)
       (begin
        (displayln "SUCCESSFUL PARSE")
        (displayln "Final Parse Progress Result: (tree, input)")
        (displayln parse-tree)
        (displayln input-stream)
        (displayln "Parse Tree: ")
        (print-parse-tree-stack parse-tree))
        (displayln ""))
      ((parse-fail msg)
       (displayln "PARSE FAIL.  Error Message: ")
       (displayln msg))
      (else (displayln "unknown result") "error"))))



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

;; Start Of Parser "Library" ;;;;;;;;;

;; This one is the heart of it all, and will
;; of course evolve and grow as we add more pieces of it.
;; Note that COOL specific stuff probably wouldn't belong in
;; this "library" module... but abstracting out of specific
;; vs general domain will come much later... not at that state yet.
;;
;;
(def (parse-expression (greedy #t))
  (let ((parser-builder (lambda ()
                          (parse-any-of (if greedy
                                          (list
                                           (parse-terminal)
                                           (parse-binary-exp [#\+ #\- #\*]))
                                          (list
                                           (parse-pipeline [(parse-terminal) (parse-empty)])
                                           (parse-pipeline [(parse-binary-exp [#\+ #\- #\*]) (parse-empty)]))))))
        (on-success-node-builder  (lambda (parse-result-tree)
                                    (make-expression (car parse-result-tree))))
        (on-fail-message "failed to parse expression"))
    (make-parser "parse-exp" parser-builder on-success-node-builder on-fail-message)))


(def (parse-terminal)
  (let ((parser-builder (lambda () (parse-any-of [(parse-integer) (parse-identifier)])))
        (on-success-node-builder  (lambda (terminal) terminal))
        (on-fail-message "failed to parse terminal"))
    (make-parser "parse-terminal" parser-builder on-success-node-builder on-fail-message)))

(def (parse-empty)
  (def (parser stream)
    (match stream
      ((parse-stream parse-tree input-stream) (if (null? input-stream)
                                               stream
                                               (make-parse-fail "unparsed characters remain")))
      (else stream)))
  parser)




;;(def (parse-binary-exp-alternate operator)
;;  (def (parser stream)
;;    (let-values ((left-stream right-stream) (split-stream operator))
;;      (if (and left-stream right-stream)
;;        (let* ((exp-parser (parse-expression))
;;               (left-result (exp-parser left-stream))
;;               (right-result (exp-parser right-stream)))
;;          (if (and (parse-stream? left-result) (parse-stream? right-result))
;;            (make-parse-stream
;;             (cons
;;              (make-binary-exp (parse-stream-parse-tree left-result) operator (parse-stream-parse-tree right-result))
;;              (parse-stream-parse-tree stream))
;;             '() ;; not sure what to put here for final input stream?
;;             )
;;            (make-parse-fail "could not parse binary expression")))
;;        (make-parse-fail "could not parse binary expression"))))
;;    parser)



(def (parse-binary-exp operators)
  (let* ((parser-builder (lambda ()
                           (parse-pipeline [
                                            (parse-expression)
                                            (parse-any-char operators)
                                            (parse-expression #f)])))
         (on-success-node-builder (lambda (parse-sub-tree)
                                    (let ((left-operand (car (cdr (cdr parse-sub-tree))))
                                          (operator (car (cdr parse-sub-tree)))
                                          (right-operand (car parse-sub-tree)))
                                      (make-binary-exp left-operand operator right-operand))))
         (on-fail-message "failed to parse binary expression"))
    (make-parser "parse-binary-exp" parser-builder on-success-node-builder on-fail-message)))



(def (parse-identifier)
  (let ((parser-builder (lambda ()
                          (parse-pipeline [(parse-valid-identifier-non-digit-char)
                                           (parser-repeat (parse-valid-identifier-char))])))
        (on-success-node-builder  (lambda (parse-tree)
                                    (make-identifier (list->string (reverse parse-tree)))))
        (on-fail-message "failed to parse identifier"))
    (make-parser "parse-identifier" parser-builder on-success-node-builder on-fail-message)))


 ;; (def (new-parser stream)
  ;;  (trace-msg (string-append "running " name) stream)
   ;; (display (parse-stream-input-stream stream))
    ;;(displayln)
    ;;(displayln)
    ;;(let* ((sub-tree-stream (make-parse-stream '() (parse-stream-input-stream stream)))
     ;;      (sub-tree-parse-result (parser sub-tree-stream)))
     ;; (match sub-tree-parse-result
      ;;  ((parse-stream parse-tree input-stream) (make-parse-stream
       ;;                                          (cons
        ;;                                          (on-success-node-builder parse-tree)
         ;;                                         (parse-stream-parse-tree stream))
          ;;                                       input-stream))
       ;; (else (make-parse-fail on-failure-message)))))
 ;; new-parser)

(def (make-parser name parser-builder on-success-node-builder on-failure-message)
  (def (new-parser stream)
    (trace-msg (string-append "running " name) stream)
    (display (parse-stream-input-stream stream))
    (displayln)
    (displayln)
    (let* ((sub-tree-stream (make-parse-stream '() (parse-stream-input-stream stream)))
           (parser (parser-builder))
           (sub-tree-parse-result (parser sub-tree-stream)))
      (match sub-tree-parse-result
        ((parse-stream parse-tree input-stream) (make-parse-stream
                                                 (cons
                                                  (on-success-node-builder parse-tree)
                                                  (parse-stream-parse-tree stream))
                                                 input-stream))
        (else (make-parse-fail on-failure-message)))))
  new-parser)





(def (parse-valid-identifier-non-digit-char)
  (parse-any-char (string->list "_$-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnophijklmnopqrstuvwxyz")))

(def (parse-valid-identifier-char)
  (parse-any-char (string->list "_$-ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnophijklmnopqrstuvwxyz0123456789")))

(def (parse-foo-int-blah)
  (parse-pipeline
   [(parse-string "foo")
    (parse-integer)
    (parse-string "blah")]))

(def (parse-integer)
  (def (parser stream)
    (trace-msg "running parse-integer" stream)
    (let* ((digits-parser (parser-repeat (parse-digit)))
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

;; todo: parse the string between quotes
 ;; todo: parse anything between the quotes
(def (parse-string str)
  (def (parser stream)

    (let* ((str-chars (string->list str))
           (characters-parser (parse-pipeline (map parse-char str-chars)))
           (string-parser (parse-pipeline [(parse-char #\")
                                           characters-parser
                                           (parse-char #\")]))
           (sub-tree-stream (make-parse-stream '() (parse-stream-input-stream stream)))
           (string-parser-result (string-parser sub-tree-stream)))
      (match string-parser-result
        ((parse-stream parse-tree input-stream) (make-parse-stream
                                                 (cons
                                                  (make-string-literal (list->string (reverse parse-tree)))
                                                  (parse-stream-parse-tree stream))
                                                 input-stream))
        (else (make-parse-fail (string-append "failed to parse string" str))))))
  parser)


(def (parse-digit)
  (parse-any-char (string->list "0123456789")))


(def (parse-any-char chars)
  (parse-any-of (map parse-char chars)))

 ;;;;; new idea - strings


;;; later: hmmm need a Pop-N?

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
      (foldl composer (car reversed-parsers) (cdr reversed-parsers))))
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


;;(def (split-parse-stream stream char)
 ;; (match stream
  ;;  ((parse-stream parse-tree input-stream)
   ;;  (let-values (((before after) (split-list input-stream char)))
    ;;   (if (pair? before)
     ;;    (values (make-parse-stream '() before) (make-parse-stream '() after))
      ;;   #f)
;;     (else #f)))))
