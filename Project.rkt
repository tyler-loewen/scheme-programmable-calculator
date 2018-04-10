#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (CONST-INT CONST-FLOAT CONST-BOOL NAME SYS-CALL))
(define-empty-tokens op-tokens (+ - / * ^ NEG = == >= <= <> > <))
(define-empty-tokens delim-tokens (NEWLINE EOF HASH OP CP WHITESPACE COMMENT))

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
   (#\newline 'NEWLINE)
   ((:or #\tab #\space) 'WHITESPACE)
   (line-comment 'COMMENT)
   ("true" (token-CONST-BOOL (#t)))
   ("false" (token-CONST-BOOL (#f)))
   ((:: #\# (:+ letter)) (token-SYS-CALL (list->string lexeme)))
   ((:+ letter) (token-NAME (string->symbol lexeme)))
   ((:: (:+ digit) #\. (:* digit)) (token-CONST-FLOAT (string->number lexeme)))
   ((:+ digit) (token-CONST-INT (string->number lexeme)))
   ((eof) 'EOF)
  )
)

(define uofl-parser
  (parser
   (start s)
   (end EOF)
   (error
    (lambda (is-token token-name token-value)
      (display "An error has occurred!")
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
   (tokens value-tokens op-tokens delim-tokens)
   (grammar
    (s (() #f)
       ((exp) $1))
    (exp
     ((CONST-INT) $1)
     ((CONST-FLOAT) $1)
     ((CONST-BOOL) $1)
     ((NAME) (hash-ref vars $1 0))
     ((NAME = exp) (begin
                     (hash-set! vars $1 $3)
                     $3
                   ))
     ((exp + exp) (+ $1 $3))
     ((exp - exp) (- $1 $3))
     ((exp * exp) (* $1 $3))
     ((exp / exp) (/ $1 $3))
     ((- exp) (prec NEG) (- $2))
     ((exp ^ exp) (expt $1 $3))
     ((exp == exp) (= $1 $3))
     ((exp <> exp) (not(eq? $1 $3)))
     ((exp >= exp) (>= $1 $3))
     ((exp <= exp) (<= $1 $3))
     ((exp > exp) (> $1 $3))
     ((exp < exp) (< $1 $3))
     ((OP exp CP) $2)
     ))))
           
;; run the calculator on the given input-port       
(define (calc ip)
  (port-count-lines! ip)
  (letrec ((one-line
	    (lambda ()
	      (let ((result (uofl-parser (lambda () (uofl-lexer ip)))))
                (cond (result) (printf "~a\n" result)(one-line)
                      (else (printf "`a\n" result) (one-line)))
                ))))
    (one-line)))

(define (uofl)
  (display "UofL>")
  (calc (open-input-string (read-line))))
;;(calc (open-input-string "x=1+2-5"))
;;(calc (open-input-string "x=7*2.0/6.0"))

(uofl)
