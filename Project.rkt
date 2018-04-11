#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

;; Reference: http://www.cs.rpi.edu/courses/fall00/ai/scheme/reference/scheme-workshop/stacks.html
(define make-stack
  (lambda ()
    (let ((stk '()))
      (lambda (message . args)
        (case message

          ;; If the message is empty?, the stack returns #t if its
          ;; storage location contains the null object, #f if it
          ;; contains a non-empty list. 

          ((empty?) (null? stk))

          ;; The push! message should be accompanied by an extra
          ;; argument -- the value to be pushed onto the stack.  This
          ;; value is simply added to the front of the list in the
          ;; private storage location. 

          ((push!) (set! stk (cons (car args) stk)))

          ;; If the message is top, the stack returns the first
          ;; element of the list in private storage, or signals an
          ;; error if that list is empty.

          ((top) (if (null? stk)
                     (error "top: The stack is empty.")
                     (car stk)))

          ;; If the message is pop!, the stack returns the first
          ;; element of the list in private storage after removing
          ;; that element from that list.

          ((pop!) (if (null? stk)
                    (error "pop!: The stack is empty.")
                    (let ((result (car stk)))
                      (set! stk (cdr stk))
                      result)))

          ;; Comment out any of the following operations that are not
          ;; wanted in a particular application of stacks.

          ;; When it receives the size message, the stack reports the
          ;; number of elements in the list in private storage.

          ((size) (length stk))

          ;; If the message is nth, there should be an extra argument
          ;; -- the (zero-based) position of the item desired, or in
          ;; other words the number of values on the stack that
          ;; precede the one to be returned.

          ((nth) (list-ref stk (car args)))

          ;; When it receives the print message, the stack displays
          ;; the elements of the list in private storage, each
          ;; followed by a space, then starts a new line.

          ((print) (for-each (lambda (element)
                               (display element)
                               (display " "))
                             stk)
                   (newline))

          ;; It is an error to send the stack any other message.

          (else (error "stack: unrecognized message")))))))

(define-tokens value-tokens (CONST-INT CONST-FLOAT CONST-BOOL NAME SYS-CALL))
(define-empty-tokens control-tokens (IF THEN ELSE ELSEIF ENDIF))
(define-empty-tokens op-tokens (+ - / * ^ NEG = == >= <= <> > <))
(define-empty-tokens delim-tokens (EOF HASH OP CP WHITESPACE COMMENT))

(define vars (make-hash))
(define funcs (make-hash))
(define run-stack (make-stack))

(define-lex-abbrevs
  (lc-letter (:/ #\a #\z))
  (uc-letter (:/ #\A #\Z))
  (letter (:or lc-letter uc-letter))
  (digit (:/ #\0 #\9))
  (num (:>= 1 digit))
  (line-comment (:: "//" (:* (:~ #\newline))))
)

(define uofl-lexer
  (lexer
   ((:or "==" ">=" "<=" "<>") (string->symbol lexeme))
   ((:or "+" "-" "/" "*" "^" "=" ">" "<") (string->symbol lexeme))
   (#\( 'OP)
   (#\) 'CP)
   ((:or #\tab #\space #\newline) (uofl-lexer input-port)) ;; ignore whitespace
   (line-comment 'COMMENT) ;; comment
   ("true" (token-CONST-BOOL #t)) ;; true
   ("false" (token-CONST-BOOL #f)) ;; false
   ("if" 'IF)
   ("then" 'THEN)
   ("elseif" 'ELSEIF)
   ("else" 'ELSE)
   ("endif" 'ENDIF)
   ((:: #\# (:+ letter)) (token-SYS-CALL (string->symbol lexeme))) ;; system call
   ((:+ letter) (token-NAME (string->symbol lexeme))) ;; variable/function
   ((:: (:+ digit) #\. (:* digit)) (token-CONST-FLOAT (string->number lexeme))) ;; float
   ((:+ digit) (token-CONST-INT (string->number lexeme))) ;; integer
   ((eof) 'EOF)
  )
)

(define uofl-parser
  (parser
   (start s)
   (end EOF)
   (error
    (lambda (is-token token-name token-value)
      (if is-token
          (eprintf "Token '~a' has an error. It's value: ~a\n" token-name token-value) ;; Token error
          (display "Unknown parser error.") ;; Not a token error
      )
    )
   )
   (precs
    (left == >= <= <>)
    (right =)
    (left < >)
    (left - +)
    (left * /)
    (left NEG)
    (right ^)
   )
   (tokens value-tokens op-tokens delim-tokens control-tokens)
   (grammar
    (s (() #f)
       ((exp) $1))
    (exp
     ((CONST-INT) (lambda () $1))
     ((CONST-FLOAT) (lambda () $1))
     ((CONST-BOOL) (lambda () $1))
     ((NAME) (lambda () (hash-ref vars (execute $1) #f)))
     ((NAME = exp) (lambda ()
                     (hash-set! vars (execute $1) (execute $3))
                     $3
                   ))
     ((exp + exp) (lambda () (+ (execute $1) (execute $3))))
     ((exp - exp) (lambda () (- (execute $1) (execute $3))))
     ((exp * exp) (lambda () (* (execute $1) (execute $3))))
     ((exp / exp) (lambda () (/ (execute $1) (execute $3))))
     ((- exp) (prec NEG) (lambda () (- (execute $2))))
     ((exp ^ exp) (lambda () (expt (execute $1) (execute $3))))
     ((logical-op-exp) (lambda () $1))
     ((OP exp CP) (lambda () $2))
     ((SYS-CALL)
      (lambda ()
        ;; Handle the system call
        (display $1)
        ))
     ((IF cond-exp ENDIF)
      (lambda ()
        $2
        ))
     )
    (logical-op-exp
     ((exp == exp) (lambda () (eq? (execute $1) (execute $3))))
     ((exp <> exp) (lambda () (not(eq? (execute $1) (execute $3)))))
     ((exp >= exp) (lambda () (>= (execute $1) (execute $3))))
     ((exp <= exp) (lambda () (<= (execute $1) (execute $3))))
     ((exp > exp) (lambda () (> (execute $1) (execute $3))))
     ((exp < exp) (lambda () (< (execute $1) (execute $3))))
     )
    (cond-exp
     ((exp THEN cond-exp-body)
      (lambda ()
        (run-stack 'push! (execute $1))
        $3
        )
      )
     )
    (cond-exp-body
     ((exp ELSEIF cond-exp)
      (lambda ()
        (if (run-stack 'pop!)
            $1
            $3
            )
        )
      )
     ((exp ELSE exp)
      (lambda ()
        (if (run-stack 'pop!)
            $1
            $3
            )
        )
      )
     ((exp)
      (lambda ()
        (if (run-stack 'pop!)
            $1
            #f
            )
        ))
     )
    )
   )
  )

(define (lex-all-recursive ip token-list)
  (let ((tok (uofl-lexer ip)))
    (set! token-list (append token-list (list tok)))
    (if (not (eq? tok 'EOF))
        (lex-all-recursive ip token-list)
        (lambda ()
          token-list
        )
    )
  )
)

(define (parse-list token-list)
  (printf "Tokenized: ~a\n" token-list)
  (let ((parse-object (uofl-parser
                       (lambda ()
                         (let ((tok (car token-list)))
                           (set! token-list (cdr token-list))
                           tok
                           )
                         )
                       )))
    parse-object
  )
)

(define (execute parse-object)
  (if (procedure? parse-object)
      (execute (parse-object))
      parse-object
  )
)

(define (lex-parse-all ip)
  (let* ((token-list '())
        (result (lex-all-recursive ip token-list)))
    (execute (parse-list (result)))
  )
)

(define (interpret ip)
  (port-count-lines! ip)
  (let ((temp (lex-parse-all ip)))
    (printf "Result: ~a\n" temp)
  )
)

(interpret (open-input-string "#print"))

;;(interpret (open-input-string "x=0"))
;;(interpret (open-input-string "y=0"))
;;(interpret (open-input-string "if (x == 0) then y=1 endif"))
;;(interpret (open-input-string "y"))