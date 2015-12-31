#!/usr/bin/guile
!#


(define (print x)
  (display x)
  (newline))

(define nil '())



;; Section 2.2.1: Representing sequences



;; Excercise 2.17
;; Define a procedure last-pair that returns
;; the list that contains only the last
;; element of a given nonempty list.

(define (last-pair items)
  (if (null? (cdr items))
    items
    (last-pair (cdr items))))

(print "2.17 last-pair")
(print (last-pair (list 23 72 149 34)))




;; Excercise 2.18: define reverse.

(define (reverse items)
  (define (iter items rev)
    (if (null? items)
      rev
      (iter (cdr items) (cons (car items) rev))))
  (iter items nil))

(print "2.18 reverse")
(print (reverse (list 1 2 3 4)))




;; Excercise 2.20:
;; Define same-partity procdedure which, given the arguments,
;; returns a list of arguments with the same even-odd parity
;; as the first argument, e.g.
;; (same-parity 1 2 3 4 5) -> (1 3 5)
;; (same-parity 2 3 4) -> (2 4)

; recursive implementation

(define (same-parity first . rest)
  (define desired-parity?
    (if (even? first) even? odd?))
  (define (select-from items)
    (if (null? items)
      nil
      (if (desired-parity? (car items))
        (cons (car items) (select-from (cdr items)))
        (select-from (cdr items)))))
  (cons first (select-from rest)))

(print "2.20 same-parity")
(print (same-parity 1 2 3 4 5))     ;prints (1 3 5)
(print (same-parity 2 3 4 5 6 8))   ;prints (2 4 6 8)




;; Excercise 2.21: mapping over lists.
;; Implement two different versions
;; of square-list (recursion vs map).

(define (square-list-1 items)
  (if (null? items)
    nil
    (cons (* (car items) (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

(print "2.21 square-list")
(print (square-list-1 (list 1 2 3 4)))  ;prints (1 4 9 16)
(print (square-list-2 (list 1 2 3 4)))  ;prints (1 4 9 16)




;; Excercise 2.23: Implement for-each.
;; for-each calls a procedure for its side
;; effects for each element of the list.

(define (for-each items proc)
  (cond ((not (null? items))
         (proc (car items))
         (for-each (cdr items) proc))))

(print "2.23 for-each")
(for-each (list 1 2 3 4 5 6 7 9) print)  ;prints each element




;; Section 2.2.2: Hierarchical Structures



;; Excercise 2.24: (list 1 (list 2 (list 3 4))))
;; - what interpreter would print
;; - draw box-and-pointer representation
;; - draw corresponding tree
;;
https://drive.google.com/open?id=0B2HTFyK_1qLJNHNhbndYWENYOFVhR19Ga0JVUVo5cXZNSk5J




