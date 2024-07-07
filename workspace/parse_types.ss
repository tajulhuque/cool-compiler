
(export parse-stream)
(export parse-stream?)
(export parse-stream-input-stream)
(export parse-stream-parse-tree)
(export make-parse-stream)
(export parse-fail)
(export parse-fail?)
(export make-parse-fail)
(export make-expr)
(export expr?)
(export make-arithmetic-expr)
(export arithmetic-expr?)
(export make-identifier-expr)
(export identifier-expr?)
(export make-integer-expr)
(export integer-expr?)
(export make-string-expr)
(export string-expr?)




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

;; General expression
;;
(def (make-expr expr)
  (list 'expr expr))

(def (expr? expr)
  (eq? (car expr) 'expr))

;; If Expression

(def (make-if-expr test true-path false-path)
  (list 'if-expr test true-path false-path))

(def (if-expr? expr)
  (eq? (car expr) 'if-expr))

;; Arithmetic expression
;;
(def (make-arithmetic-expr left op right)
  (list 'arith-expr left op right))

(def (arithmetic-expr? expr)
  (eq? (car expr) 'arith))

;; Identifier xxpression

(def (make-identifier-expr expr)
  (list 'ident expr))

(def (identifier-expr? expr)
  (eq? (car expr) 'ident))

;; Integer literal expression

(def (make-integer-expr expr)
  (list 'int expr))

(def (integer-expr? expr)
  (eq? (car expr) 'int))

(def (make-string-expr expr)
  (list 'string expr))

(def (string-expr? expr)
  (eq? (car expr) 'string))



;; parse-to-types previously as structs
;; November 2023: Going with standard list structures instead
;;(defstruct expression (exp))
;;(defstruct int-literal (value))
;;(defstruct string-literal (value))
;;(defstruct identifier (name))
;;(defstruct binary-exp (left-exp op right-exp))
