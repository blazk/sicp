#!/usr/bin/guile
!#

(define (print s) (display s) (newline))
(define (square x) (* x x))


; 1.1.7  Square Roots by Newton's Method.
;
; Declarative definition of a square root function:
;   sqrt(x) is an y such that y>=0 and y^2 = x
; but it does not tell us how to find y.
; Imperative definition of a square root function:
; (can I call it that?)

(define (average x y)
  (/ (+ x y) 2))

; here we use nested definitions (block structure)
; and lexical scoping (x is a free variable
; in the nested procedures and it is not necessary
; to pass x explicitly to these nested procedures).

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(print (sqrt 2))
(print (sqrt 9))
(print (square (sqrt 1000)))
