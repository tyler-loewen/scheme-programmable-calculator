#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (CONST-INT CONST-FLOAT CONST-BOOL NAME SYS-CALL))
(define-empty-tokens control-tokens (IF THEN ELSE ELSEIF ENDIF))
(define-empty-tokens op-tokens (+ - / * ^ NEG = == >= <= <> > <))
(define-empty-tokens delim-tokens (EOF HASH OP CP WHITESPACE COMMENT))

(define vars (make-hash))
(define funcs (make-hash))

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
   ("true" (token-CONST-BOOL (#t))) ;; true
   ("false" (token-CONST-BOOL (#f))) ;; false
   ("if" 'IF)
   ("then" 'THEN)
   ("elseif" 'ELSEIF)
   ("else" 'ELSE)
   ("endif" 'ENDIF)
   ((:: #\# (:+ letter)) (token-SYS-CALL (list->string lexeme))) ;; system call
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
     ((NAME) (lambda () (hash-ref vars $1 0)))
     ((NAME = exp) (lambda ()
                     (hash-set! vars $1 $3)
                     $3
                   ))
     ((exp + exp) (lambda () (+ $1 $3)))
     ((exp - exp) (lambda () (- $1 $3)))
     ((exp * exp) (lambda () (* $1 $3)))
     ((exp / exp) (lambda () (/ $1 $3)))
     ((- exp) (prec NEG) (lambda () (- $2)))
     ((exp ^ exp) (lambda () (expt $1 $3)))
     ((exp == exp) (lambda () (eq? $1 $3)))
     ((exp <> exp) (lambda () (not(eq? $1 $3))))
     ((exp >= exp) (lambda () (>= $1 $3)))
     ((exp <= exp) (lambda () (<= $1 $3)))
     ((exp > exp) (lambda () (> $1 $3)))
     ((exp < exp) (lambda () (< $1 $3)))
     ((OP exp CP) (lambda () $2))
     ((IF exp THEN exp ENDIF) (lambda()
                                (if (execute $2)
                                    (lambda () $4)
                                    (lambda () 0)
                                )))
     ))))

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

(interpret (open-input-string "if (x == 0) then x=1 endif"))
;;(interpret (open-input-string "if (x == 0) then\nx = 2\nendif"))