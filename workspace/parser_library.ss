
(export parse-expression)
(export parse-if-expr)
(export parse-binary-exp)
(export parse-integer)
(export parse-identifier)
(export parse-string)
(export run-parser)
(export peek-empty)
(export peek-phrase)
(export parse-ws)
(export parse-keyword)

(import "parse_types")

;; Parser Runner ;;;;;;;;;
;;
(def (run-parser parser stream)
  (displayln "initial input stream: " (parse-stream-input-stream stream))
  (let (parse-result (parser stream))
    (match parse-result
      ((parse-stream parse-tree input-stream)
       (begin
        (displayln "SUCCESSFUL PARSE")
        (displayln "input stream after parse: " input-stream)
        (displayln "parse tree result: " parse-tree)
       (displayln "")))
      ((parse-fail msg)
       (displayln "PARSE FAIL.  Error Message: ")
       (displayln msg))
      (else (displayln "unknown result") "error"))))



;; Start Of Parser "Library" ;;;;;;;;;

;; This one is the heart of it all, and will
;; of course evolve and grow as we add more pieces of it.
;; Note that COOL specific stuff probably wouldn't belong in
;; this "library" module... but abstracting out of specific
;; vs general domain will come much later... not at that state yet.
;;

(def (get-binary-operators)
  (list #\+ #\- #\* #\> #\< #\=))


(def (parse-expression (greedy #t) (followed-by (peek-empty)))
  (let ((parser-builder (lambda ()
                          (parse-any-of (if greedy
                                          (list
                                           (parse-terminal)
                                           (parse-binary-exp (get-binary-operators)))
                                          (list
                                           (parse-pipeline (list (parse-terminal) followed-by))
                                           (parse-binary-exp (get-binary-operators) #f followed-by))))))
        (on-success-node-builder  (lambda (parse-result-tree)
                                    (car parse-result-tree)))
        (on-fail-message "failed to parse expression"))
    (make-parser "parse-exp" parser-builder on-success-node-builder on-fail-message)))


(def (parse-expression-followed-by parser)
  (parse-expression #f parser))

(def (parse-if-expr)
  (let* ((parser-builder (lambda ()
                           (parse-pipeline
                            [
                             (parse-phrase "If")
                             (parse-expression-followed-by (peek-phrase "Then"))
                             (parse-phrase "Then")
                             (parse-expression-followed-by (peek-phrase "Else"))
                             (parse-phrase "Else")
                             (parse-expression-followed-by (peek-phrase "Fi"))
                             (parse-phrase "Fi")
                             ])))

         ;; Hmmm: how are we gonna do if expressions that don't have the alternate part?
         ;;
         (on-success-node-builder (lambda (parse-sub-tree)
                                    (let ((raw-if-parts (reverse parse-sub-tree)))
                                      (make-if-expr
                                       (list-ref raw-if-parts 1) ; predicate
                                       (list-ref raw-if-parts 3) ; consequent
                                       (list-ref raw-if-parts 5)) ; alternate
                                      )))

         (on-fail-message "failed to parse if-expression"))
    (make-parser "parse-if-exp" parser-builder on-success-node-builder on-fail-message)))



(def (parse-terminal)
  (let ((parser-builder (lambda () (parse-any-of [(parse-integer) (parse-identifier)])))
        (on-success-node-builder  (lambda (terminal) terminal))
        (on-fail-message "failed to parse terminal"))
    (make-parser "parse-terminal" parser-builder on-success-node-builder on-fail-message)))

;; need to rename maybe to "peek empty"... maybe a naming convention for functions
;; that just "peek" at the input frontier without actually consuming it
(def (peek-empty)
  (def (peeker stream)
    (match stream
      ((parse-stream parse-tree input-stream) (if (null? input-stream)
                                               stream
                                               (make-parse-fail "unparsed characters remain")))
      (else stream)))
  peeker)

;; general "peek" adapter function
(define (peek parser)
  (define (peeker stream)
    (let (parse-result (parser stream))
      (match parse-result
        ((parse-stream parse-tree input-stream) stream) ;; PEEK: upon "parse" success, just return the original stream, leaving the input frontier un-consumed
        (else parse-result) ;; otherwise return value result
        )))
  peeker)


(define (peek-phrase phrase)
  (let (phrase-parser (parse-phrase phrase))
    (peek phrase-parser)))

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



(def (parse-binary-exp operators (greedy #t) (followed-by (peek-empty)))
  (let* ((parser-builder (lambda ()
                           (parse-pipeline [
                                            (parse-expression)
                                            (parse-any-char operators)
                                            (parse-expression greedy followed-by)])))
         (on-success-node-builder (lambda (parse-sub-tree)
                                    (let ((left-operand (car (cdr (cdr parse-sub-tree))))
                                          (operator (car (cdr parse-sub-tree)))
                                          (right-operand (car parse-sub-tree)))
                                      (make-arithmetic-expr left-operand operator right-operand))))
         (on-fail-message "failed to parse binary expression"))
    (make-parser "parse-binary-exp" parser-builder on-success-node-builder on-fail-message)))



(def (parse-identifier)
  (let ((parser-builder (lambda ()
                          (parse-pipeline [(parse-valid-identifier-non-digit-char)
                                           (parser-repeat (parse-valid-identifier-char))])))
        (on-success-node-builder  (lambda (parse-tree)
                                    (make-identifier-expr (list->string (reverse parse-tree)))))
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
    (displayln (string-append "running " name))
    (when (equal? name "parse-exp")
      (displayln "on input: ")
      (displayln (parse-stream-input-stream stream)))
    (let* ((sub-tree-stream (make-parse-stream '() (parse-stream-input-stream stream)))
           (parser (parser-builder))
           (sub-tree-parse-result (parser sub-tree-stream)))
      (match sub-tree-parse-result
        ((parse-stream parse-tree input-stream) (begin (trace-msg (string-append name " success. Result:") sub-tree-parse-result)
                                                       (make-parse-stream
                                                        (cons
                                                         (on-success-node-builder parse-tree)
                                                         (parse-stream-parse-tree stream))
                                                        input-stream)))
        ((parse-fail msg) (make-parse-fail (string-append on-failure-message ": " msg)))
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
    (displayln (string-append "running parse-integer"))
    (let* ((digits-parser (parser-repeat (parse-digit)))
           (digits-parse-stream (make-parse-stream '() (parse-stream-input-stream stream)))
           (digits-parse-result (digits-parser digits-parse-stream)))
      (match digits-parse-result
        ((parse-stream parse-tree input-stream) (begin
                                                  (trace-msg "parse-integer success. Result:" digits-parse-result)
                                                  (make-parse-stream
                                                   (cons
                                                    (make-integer-expr (digit-list->number (reverse parse-tree)))
                                                    (parse-stream-parse-tree stream))
                                                   input-stream)))
        (else (make-parse-fail "failed to parse integer")))))
  parser)





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
                                                  (make-string-expr (list->string (reverse parse-tree)))
                                                  (parse-stream-parse-tree stream))
                                                 input-stream))
        (else (make-parse-fail (string-append "failed to parse string" str))))))
  parser)

;; Later; Need to sort out parse-string vs parse text.
;; Shouldn't "parse string reuse parse-phrase somehow -- just enclose parse-phrase's results
;; in quotes?  Note that parse-phrase (without quotes) is very useful for for just parsing
;; out simple unquoted keywords like "If, Loop" etc.


(def (parse-phrase str)
  (def (parser stream)

    (let* ((str-chars (string->list str))
           (characters-parser (parse-pipeline (map parse-char str-chars)))
           (sub-tree-stream (make-parse-stream '() (parse-stream-input-stream stream)))
           (string-parser-result (characters-parser sub-tree-stream)))
      (match string-parser-result
        ((parse-stream parse-tree input-stream) (make-parse-stream
                                                 (cons
                                                  (make-string-expr (list->string (reverse parse-tree)))
                                                  (parse-stream-parse-tree stream))
                                                 input-stream))
        ((parse-fail msg) (make-parse-fail (string-append "failed to parse phrase:" str " - Error: " msg)))
        (else (make-parse-fail (string-append "failed to parse phrase:" str))))))
  parser)

;; March, 2025:  Contemplating a "parse-keyword"
;; so that can incomporate white-space rules.  A keyword should be expected to have
;; some kind of whitespace separator between it and other syntax.
;; Start simple:  "A keyword must have some whitespace immediately before and after".
;; Then can relax later (requiring ws in front of "If" for example is kinda silly?),
;; but mabye not, we expect expressions to be embedded in some other context / expression
;; Current plan:
;;   - use parser-repeat to make a "parse-whitespace"
;;   - then make a "parse-keyword" which will sanwhich a parse-phrase
;;   - between to "parse-whitespace"s (so use parse-pipeline to combine all three)
;;   - you'll need to update "peek" functions to "peek through / past" whitespace
;;     (probably you can have --or return-- another representation of the input frontier
;;     that is all the whitespace characters stripped away? , or when peeking ahead, then strip away?)
;;
;    - start a "conessions" / "limitations" list (you can commit it to guest)
;    ("keywords must have some sourrounding whitespace", would be the first item, but might be others you can think of)
(define (parse-keyword keyword)
  (parse-pipeline
   (list (parse-ws) (parse-phrase keyword) (parse-ws))))

(define (parse-ws)
  (let ((parse-ws-char (parse-any-char (list #\space #\tab #\newline))))
    (parser-repeat parse-ws-char)))


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
         (make-parse-fail (string-append "PARSE FAIL:" "expected " (string char) ", but got " (string (car input-stream))))))
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
