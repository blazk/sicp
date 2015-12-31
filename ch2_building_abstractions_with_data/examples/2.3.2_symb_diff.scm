#!/usr/bin/guile
!#

;; Example 2.3.2: Symbolic differentiation.
;;
;; - Differentiation algorithm operates on
;;   abstract objects (sums, products, variables).
;;   Representation of these objects is hidden behind
;;   abstract constructors (e.g. 'make-sum') and selectors
;;   (e.g. 'a1', 'a2').
;; - We use Lisp prefix notation and quoting to represent
;;   expressions (but we could change to a different
;;   representation without affecting differentiation
;;   algorithm thanks to the data abstraction layer)
;; - First version of the program gave correct but
;;   complicated (unsimplified) answers. Simplification
;;   rules can be built into constructors (similar
;;   strategy was used in rational numbers example).


(define (deriv expr var)
  (cond ((constant? expr var) 0)
        ((same-var? expr var) 1)
        ((sum? expr)
         (make-sum (deriv (a1 expr) var)
                   (deriv (a2 expr) var)))
        ((product? expr)
         (make-sum (make-product
                     (m1 expr)
                     (deriv (m2 expr) var))
                   (make-product
                     (m2 expr)
                     (deriv (m1 expr) var))))))

(define (atom? x)
  (symbol? x))

(define (constant? expr var)
  (and (atom? expr)
       (not (eq? expr var))))

(define (same-var? expr var)
  (and (atom? expr)
       (eq? var expr)))

(define (=number? expr num)
  (and (number? expr) (= expr num)))

(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2))
         (+ a1 a2))
        ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        (else (list '+ a1 a2))))
(define (a1 sum)
  (cadr sum))
(define (a2 sum)
  (caddr sum))
(define (sum? expr)
  (and (not (atom? expr))
       (eq? (car expr) '+)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))
(define (product? expr)
  (and (not (atom? expr))
       (eq? (car expr) '*)))
(define (m1 prod)
  (cadr prod))
(define (m2 prod)
  (caddr prod))


;; Find the derivative of some function

(define foo
  '(+ (* a (* x x))
      (+ (* b x)
         c)))

(display (deriv foo 'x))    ;prints (+ (* a (+ x x)) b)
(newline)

(display (deriv foo 'a))    ;prints (* x x)
(newline)

(display (deriv foo 'b))    ;prints x
(newline)
