#lang racket

(define uofl
  (lambda (balance)
    (display "UofL>")
    (set! balance (list (read-line)))
    (display balance)
    (lambda (n)
      (display "this is lambda2")
      (set! balance (+ balance (n)))
      balance)))


(uofl 7)