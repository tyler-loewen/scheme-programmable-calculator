#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         racket/format)

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

(define-tokens value-tokens (CONST-INT CONST-FLOAT CONST-BOOL NAME STRING))
(define-empty-tokens control-tokens (IF THEN ELSE ELSEIF ENDIF FOR TO DO ENDFOR))
(define-empty-tokens op-tokens (+ - / * ^ NEG = == >= <= <> > <))
(define-empty-tokens delim-tokens (EOF HASH OP CP COMMENT NEWLINE COMMA STEPSIZE QUOTE))
(define-empty-tokens call-tokens (DEFINE-VAR DEFINE-FUNC EXIT CLEAR INPUT OUTPUT OUTPUT-LN))
(define-tokens type-tokens (DATA-TYPE))

(define vars (make-hash))
(define funcs (make-hash))
(define run-stack (make-stack))

(struct func (params body) #:constructor-name make-func! #:transparent)

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
   ((eof) 'EOF)
   (#\newline 'NEWLINE)
   ((:or "==" ">=" "<=" "<>") (string->symbol lexeme))
   ((:or "+" "-" "/" "*" "^" "=" ">" "<") (string->symbol lexeme))
   (#\( 'OP)
   (#\) 'CP)
   [#\" (token-STRING (list->string (string-lexer input-port)))]
   (line-comment 'COMMENT) ;; comment
   ((:or #\tab #\space #\newline) (uofl-lexer input-port)) ;; ignore whitespace
   ("true" (token-CONST-BOOL #t)) ;; true
   ("false" (token-CONST-BOOL #f)) ;; false
   ("if" 'IF)
   ("then" 'THEN)
   ("elseif" 'ELSEIF)
   ("else" 'ELSE)
   ("endif" 'ENDIF)
   ("for" 'FOR)
   ("to" 'TO)
   ("stepsize" 'STEPSIZE)
   ("do" 'DO)
   ("endfor" 'ENDFOR)
   ("#exit" 'EXIT)
   ("#clear" 'CLEAR)
   ("#definevari" 'DEFINE-VAR)
   ("#definefunc" 'DEFINE-FUNC)
   ("#" 'HASH)
   (#\, 'COMMA)
   ("input" 'INPUT)
   ("outputln" 'OUTPUT-LN)
   ("output" 'OUTPUT)
   ((:or "integer" "boolean" "float") (token-DATA-TYPE (string->symbol lexeme)))
   ((:+ letter) (token-NAME (string->symbol lexeme))) ;; variable/function
   ((:: (:+ digit) #\. (:* digit)) (token-CONST-FLOAT (string->number lexeme))) ;; float
   ((:+ digit) (token-CONST-INT (string->number lexeme))) ;; integer
   )
  )

(define string-lexer
  (lexer
   ((:~ #\" #\\) (cons (car (string->list lexeme))
                       (string-lexer input-port)))
   ((:: #\\ #\\) (cons #\\ (string-lexer input-port)))
   ((:: #\\ #\") (cons #\" (string-lexer input-port)))
   (#\" null)
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
   (tokens value-tokens op-tokens delim-tokens control-tokens call-tokens type-tokens)
   (grammar
    (s ((exp-list) (reverse $1)))
    
    (exp
     ((CONST-INT) (lambda () $1))
     ((CONST-FLOAT) (lambda () $1))
     ((CONST-BOOL) (lambda () $1))
     ((NAME) (lambda () (hash-ref vars (execute $1) (var-ref-error (execute $1)))))
     ((assign-exp) (lambda () $1))
     ((NAME OP func-call-params CP)
      (lambda ()
        (let* ((name (execute $1))
              (fn (hash-ref funcs name (func-ref-error name))))
          ;; Push the actual parameters to the stack
          ;; Left-most on the bottom, right-most on the top
          (execute (reverse $3))

          ;; Pop the actual parameters from the stack and use those
          ;; values to set variable values.
          ;; Right-most from the top, left-most from the bottom
          ;; The params are stored in order, so we reverse the order
          (execute (reverse (func-params fn)))
          
          (execute (func-body fn))
          )
        ))
     ((exp + exp) (lambda () (+ (execute $1) (execute $3))))
     ((exp - exp) (lambda () (- (execute $1) (execute $3))))
     ((exp * exp) (lambda () (* (execute $1) (execute $3))))
     ((exp / exp) (lambda () (/ (execute $1) (execute $3))))
     ((- exp) (prec NEG) (lambda () (- (execute $2))))
     ((exp ^ exp) (lambda () (expt (execute $1) (execute $3))))
     ((logical-op-exp) (lambda () $1))
     ((OP exp CP) (lambda () $2))
     ((EXIT) (lambda () (exit)))
     ((CLEAR)
      (lambda ()
        (hash-clear! vars)
        (hash-clear! funcs)
        ))
     ((IF cond-exp ENDIF)
      (lambda ()
        $2
        ))
     ((OUTPUT NAME)
      (lambda ()
        (let* ((name (execute $2))
               (val (hash-ref vars name (var-ref-error name))))
          (display val)
          )
        ))
     ((OUTPUT STRING)
      (lambda ()
        (printf "~a" (execute $2))
        ))
     ((OUTPUT-LN NAME)
      (lambda ()
        (let* ((name (execute $2))
               (val (hash-ref vars name (var-ref-error name))))
          (printf "~a\n" val)
          )
        ))
     ((OUTPUT-LN STRING)
      (lambda ()
        (printf "~a\n" (execute $2))
        ))
     ((INPUT NAME)
      (lambda ()
        (let* ((name (execute $2))
               (val (hash-ref vars (execute $2) (var-ref-error (execute $2)))))
          (begin
            (printf "Input a value for ~a: " name)
            (let ((line (read-line)))
              (interpret (open-input-string (string-append (~a name) "=" line)))
              ))
          )
        ))
     ((DEFINE-VAR NAME DATA-TYPE)
      (lambda ()
        (let ((name (execute $2)))
          (if (and (not (equal? (~a name) "I")) (not (equal? (~a name) "J")))
              (let ((type (execute $3)))
                (cond
                  ((eq? type 'integer) (hash-set! vars name 0))
                  ((eq? type 'float) (hash-set! vars name 0.0))
                  ((eq? type 'boolean) (hash-set! vars name #f))
                  )
                )
              (error "Cannot defined variable whose name is reserved for loops:" name)
              )
          )))
     ((DEFINE-FUNC NAME func-formal-params NEWLINE func-body DEFINE-FUNC)
      (lambda ()
        (hash-set! funcs (execute $2) (make-func! (reverse $3) (reverse $5)))
        ))
     ((FOR assign-loop-exp exp TO exp STEPSIZE exp DO for-body ENDFOR)
      (lambda ()
        (let ((val (execute $3)))
          (begin
            (execute-loop $2 val (execute $5) (execute $7) val (reverse $9))
            )
          )))
     ((NEWLINE) #f)
     )

    (assign-exp
     ((NAME = exp) (lambda ()
                     (let ((name (execute $1)))
                       (if (hash-has-key? vars name)
                           (let* ((val (execute $3))
                                  (cur (hash-ref vars name)))
                             ;; Make sure the types are equal first
                             (if (types-equal? val cur)
                                 (begin
                                   (hash-set! vars name val)
                                   val
                                   )
                                 (error "Type error for variable:" name)
                                 )
                             )
                           (var-ref-error name)
                           )
                       )
                     ))
     )

    (assign-loop-exp
     ((NAME =) (lambda ()
                 (let ((name (execute $1)))
                   (if (or (equal? (~a name) "I") (equal? (~a name) "J"))
                       (let* ((val (execute (run-stack 'pop!))))
                         (begin
                           (hash-set! vars name val)
                           val
                           )
                         )
                       (error "Loop variables must be named either I or J:" name)
                       )
                   )))
     )

    (stack-push-exp
     ((exp)
      (lambda ()
        (run-stack 'push! (execute $1))
        ))
     )

    (stack-pop-name-exp
     ((NAME)
      (lambda ()
        (let* ((val (execute (run-stack 'pop!)))
               (name (execute $1)))
          (if (hash-has-key? vars name)
              (let ((cur (hash-ref vars name)))
                ;; Make sure the types are equal first
                (if (types-equal? val cur)
                    (begin
                      (hash-set! vars name val)
                      val
                      )
                    (error "Type error for variable:" name)
                    )
                )
              (var-ref-error name)
              )         
          )
        ))
     )

    (func-call-params
     (() null)
     ((func-call-params2 stack-push-exp) (cons $2 $1))
     )

    (func-call-params2
     (() null)
     ((func-call-params2 stack-push-exp COMMA) (cons $2 $1))
     )

    (func-formal-params
     (() null)
     ((func-formal-params stack-pop-name-exp) (cons $2 $1))
     )

    (func-body
     (() null)
     ((func-body exp) (cons $2 $1))
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
        (lambda () $3)
        )
      )
     )

    (for-body
     (() null)
     ((for-body exp) (cons $2 $1))
     )
    
    (cond-exp-body
     ((exp-list2 ELSEIF cond-exp)
      (lambda ()
        (if (run-stack 'pop!)
            (lambda () (reverse $1))
            (lambda () $3)
            )
        )
      )
     ((exp-list2 ELSE exp-list2)
      (lambda ()
        (if (run-stack 'pop!)
            (lambda () (reverse $1))
            (lambda () (reverse $3))
            )
        )
      )
     ((exp-list2)
      (lambda ()
        (if (run-stack 'pop!)
            (lambda () (reverse $1))
            #f
            )
        ))
     )

    (exp-list
     (() null)
     ((exp-list exp) (cons $2 $1))
     )

    (exp-list2
     (() null)
     ((exp-list2 exp) (cons $2 $1))
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
  ;; (printf "Tokenized: ~a\n" token-list)
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

(define (execute-loop assign-loop-exp val-from val-to stepsize val-cur loop-body)
  (let* ((val-next (+ val-cur stepsize))
         (min-bound (min val-from val-to))
         (max-bound (max val-from val-to)))
    (if (and (>= val-cur min-bound) (<= val-cur max-bound))
        (begin
          (run-stack 'push! val-cur)
          (execute assign-loop-exp)
          (execute loop-body)
          (execute-loop assign-loop-exp val-from val-to stepsize val-next loop-body)
          )
        #f
        )
    )
  )

(define (execute parse-object)
  (if (procedure? parse-object)
      (execute (parse-object))
      (if (pair? parse-object)
          (begin
            (execute (car parse-object))
            (execute (cdr parse-object))
            )
          parse-object
          )
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
    (if (and (pair? temp) (number? (car temp)))
        null;;(display (car temp))
        null
        )
    )
  )

(define (var-ref-error name)
  (lambda () (error "Variable not defined:" name))
  )

(define (func-ref-error name)
  (lambda () (error "Function not defined:" name))
  )

(define (types-equal? x y)
  (cond
    ((exact-integer? x) (exact-integer? y)) ;; Integers
    ((and (number? x) (inexact? x)) (inexact? y)) ;; Floats
    ((boolean? x) (boolean? y))
    (else #f)
    )
  )

(define (read-fully ip str)
  (let ((str2 (read-line ip)))
    (if (and (= (string-length str2) 0) (not (char-ready? ip)))
        (string-append str str2)
        (if (= (string-length str) 0)
            (read-fully ip str2)
            (read-fully ip (string-append str (string-append "\n" str2)))
            )
        )
    )
  )

(define (uofl)
  (display "\nUofL>")
  (interpret (open-input-string (read-fully (current-input-port) "")))
  (uofl)
  )

(display "Starting interpreter.")
(uofl)