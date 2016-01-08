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
;; https://drive.google.com/open?id=0B2HTFyK_1qLJRmpIb0NGYXlFeDdVWEMxX20xLW5lOXhOUUFN



;; Excercise 2.25: Give combinations of cars and cdrs that
;; will pick 7 from each of the following lists
;; (1 3 (5 7) 9)
;; ((7))
;; (1 (2 (3 (4 (5 (6 7))))))

(print "2.25 cars and cdrs")

(print (car
         (cdr
           (car
             (cdr
               (cdr
                 '(1 3 (5 7) 9)))))))

(print (car
         (car
           '((7)))))

(print (car
         (cdr
           (car
             (cdr
               (car
                 (cdr
                   (car
                     (cdr
                       (car
                         (cdr
                           (car
                             (cdr
                               '(1 (2 (3 (4 (5 (6 7)))))))))))))))))))




;; Excercise 2.27: Modify reverse procecure of ex.2.18 to produce
;; deep-reverse procedure that takes a list as argument and returns
;; as its value the list with its elements reversed and with all
;; sublists deep-reversed as well. e.g. ((1 2) (3 4)) -> ((4 3) (2 1))


(define (deep-reverse x)
  (define (rev-if-list x)
    (if (pair? x)
      (iter x nil)
      x))
  (define (iter src rev)
    (if (null? src)
      rev
      (iter (cdr src) (cons (rev-if-list (car src)) rev))))
  (iter x nil))


(print "2.27 deep-reverse")
(print (deep-reverse '((1 2) (3 4))))           ;prints ((4 3) (2 1))
(print (deep-reverse '((1 2) 0 (3 (5 6) 4))))   ;prints ((4 (6 5) 3) 0 (2 1))




;; Excercise 2.28: Write a procedure fringe that takes as argument
;; a tree and returns a list whose elements are all the leaves
;; of the tree arranged in left-to-right order.


(define (fringe x)
  (cond ((null? x) nil)
        ((pair? (car x)) (append (fringe (car x)) (fringe (cdr x))))
        (else (cons (car x) (fringe (cdr x))))))


(print "2.28 fringe")
(let ((x (list (list 1 2) (list 3 4))))
  (print (fringe x))                            ;prints (1 2 3 4)
  (print (fringe (list x x))))                  ;prints (1 2 3 4 1 2 3 4)





;; Excercise 2.29: mobile

(define (excercise-2-29)

;; A binary mobile consists of two branches,
;; a left branch and a right branch. Each branch is a rod of
;; a certain length, from which hangs either a weight or an-
;; other binary mobile. We can represent a binary mobile us-
;; ing compound data by constructing it from two branches
;; (for example, using list):

   (define (make-mobile left right)
     (list left right))

;; A branch is constructed from a length (which must be a number)
;; together with a structure, which may be either a number
;; (representing a simple weight) or another mobile:

   (define (make-branch length structure)
     (list length structure))

;; a) write the corresponding selectors left-branch and right-branch
;; which return the branches of a mobile, and branch-length
;; and branch-structure, which return the components of a branch.

   (define (left-branch mobile)
     (car mobile))
   (define (right-branch mobile)
     (cadr mobile))
   (define (branch-length branch)
     (car branch))
   (define (branch-structure branch)
     (cadr branch))

;; b) Using your selectors, define a procedure total-weight that
;; returns total weight of a mobile

   (define (total-weight structure)
     (if (number? structure)
       structure
       (+ (total-weight (branch-structure (left-branch structure)))
          (total-weight (branch-structure (right-branch structure))))))

;; c) Design a predicate that tests whether a mobile is balanced.
;; A mobile is balanced when the torque of the top-left arm
;; is equal to the torque of the top-right arm and any
;; sub-mobiles are also balanced.

   (define (balanced? structure)
     (define (torque branch)
       (* (branch-length branch) (total-weight (branch-structure branch))))
     (if (number? structure)
       #t
       (let ((left (left-branch structure)) (right (right-branch structure)))
         (and (eq? (torque left) (torque right))
              (balanced? (branch-structure left))
              (balanced? (branch-structure right))))))

;; d) Suppose we change the representation of mobiles
;; so that the constructors are
;;   (define (make-mobile left right) (cons left right))
;;   (define (make-branch length structure) (cons length structure))
;; How much do you need to change your program to convert to the
;; new representation?
;; A: Would need to change only constructors and selectors.

;; Testing

  ; mobiles
  (define m1 (make-mobile (make-branch 1 1) (make-branch 1 2)))
  (define m2 (make-mobile (make-branch 2 m1) (make-branch 4 1)))
  (define m3 (make-mobile (make-branch 1 m1) (make-branch 1 m2)))
  ; balanced mobiles
  (define bm1 (make-mobile (make-branch 1 2) (make-branch 2 1)))
  (define bm2 (make-mobile (make-branch 3 1) (make-branch 1 bm1)))

  (print "2.29 mobile")
  (print (total-weight m3))              ;prints 7
  (print (balanced? bm1))                ;prints #t
  (print (balanced? bm2))                ;prints #t
  (print (balanced? m3))                 ;prints #f
)
(excercise-2-29)




(define (excercise-2-30)

;; Excercise 2.30: define two versions of square-tree,
;; one without map and the other using map and recursion.

  (define (square-tree-1 x)
    (cond ((null? x) nil)
          ((not (pair? x)) (* x x))
          (else (cons (square-tree-1 (car x)) (square-tree-1 (cdr x))))))

  (define (square-tree-2 tree)
    (map (lambda (sub-tree)
      (if (pair? sub-tree)
        (square-tree-2 sub-tree)
        (* sub-tree sub-tree)))
      tree))

;; test

  (define tree (list 1 (list 2 (list 3 4) 5 (list 6 7))))
  (print "2.30 square-tree")
  (print (square-tree-1 tree))
  (print (square-tree-2 tree))
)
(excercise-2-30)




(define (excercise-2-31)

;; Excercise 2.31: Abstract your answer to 2.30 to produce a procedure
;; tree-map with a property that square-tree could be defined as
;; (define (square-tree tree) (tree-map square tree))

  (define (tree-map-1 proc tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (proc tree))
          (else (cons (tree-map-1 proc (car tree))
                      (tree-map-1 proc (cdr tree))))))

  (define (tree-map-2 proc tree)
    (map (lambda (sub-tree)
      (if (pair? sub-tree)
        (tree-map-2 proc sub-tree)
        (proc sub-tree)))
      tree))

;; test

  (define tree (list 1 (list 2 (list 3 4) 5 (list 6 7))))
  (define (square x) (* x x))
  (print "2.31 tree-map")
  (print (tree-map-1 square tree))
  (print (tree-map-2 square tree))
)
(excercise-2-31)




(define (excercise-2-32)

;; Exercise 2.32: We can represent a set as a list of distinct
;; elements, and we can represent the set of all subsets of the
;; set as a list of lists. For example, if the set is (1 2 3), then
;; the set of all subsets is (() (3) (2) (2 3) (1) (1 3)
;; (1 2) (1 2 3)). Complete the following definition of a
;; procedure that generates the set of subsets of a set and give
;; a clear explanation of why it works:

  (define (subsets s)
    (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; test

  (define s (list 1 2 3))
  (print (subsets s))
)
(excercise-2-32)






;; Exercise 2.33: Fill in the missing expressions to complete
;; the following definitions of some basic list-manipulation
;; operations as accumulations.

;; first, lets define accumulate:

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))


(define (excercise-2-33)

;; map as accumulation:

  (define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

;; append as accumulation:

  (define (append seq1 seq2)
    (accumulate cons seq2 seq1))

;; length as accumulation

  (define (length seq)
    (accumulate (lambda (x y) (+ y 1)) 0 seq))

;; test

  (print "2.33 operations as accumulation")
  (print (map (lambda (x) (* x x)) (list 1 2 3)))
  (print (append (list 1 2) (list 3 4)))
  (print (length (list 0 0 0 0 0)))
)
(excercise-2-33)






(define (excercise-2-34)

;; evaluate polynomial using Horner's rule

  (define (horner-eval x coefficients)
    (accumulate (lambda (coef sum)
                        (+ coef (* sum x)))
                0
                coefficients))

;; test

  (define c (list 1 2 3))
  (print "2.34 horner-eval")
  (print (horner-eval 1 c))
)
(excercise-2-34)






(define (excercise-2-35)

;; Exercise 2.35: Redefine count-leaves from
;; section 2.2.2 as an accumulation.

  (define (count-leaves t)
    (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

;; test

  (print "2-35 count-leaves")
  (print (count-leaves '(0 0 (0 0 ((0 0))))))     ;prints 6
)
(excercise-2-35)






(define (excercise-2-36)

;; Exercise 2.36: The procedure accumulate-n is similar to
;; accumulate except that it takes as its third argument a
;; sequence of sequences, which are all assumed to have the
;; same number of elements. It applies the designated accu-
;; mulation procedure to combine all the first elements of the
;; sequences, all the second elements of the sequences, and so
;; on, and returns a sequence of the results. For instance, if s
;; is a sequence containing four sequences, ((1 2 3) (4 5 6)
;; (7 8 9) (10 11 12)), then the value of (accumulate-n +
;; 0 s) should be the sequence (22 26 30). Fill in the missing
;; expressions in the following definition of accumulate-n:

  (define (accumulate-n op init seqs)
    (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; test

  (define x '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
  (print "2.36 accumulate-n")
  (print (accumulate-n + 0 x))            ;prints (22 26 30)
)
(excercise-2-36)
