#!/usr/bin/guile
!#

(define (print x)
  (display x)
  (newline))

;; Section 2.2.1: Representing Sequences

;; List operations

;; return n-th item of the list

(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(print "list-ref")
(print (list-ref (list 'a 'b 'c 'd) 2))


;; return length of the list

(define (length items)
  (define (iter a count)
    (if (null? a)
      count
      (iter (cdr a) (+ count 1))))
  (iter items 0))


;; Append two lists; recursive version

(define (append list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

(print "append")
(print (append (list 1 2 3) (list 4 5 6)))
