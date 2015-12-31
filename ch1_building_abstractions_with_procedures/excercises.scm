#!/usr/bin/guile
!#

; 1.4: Compound operators.
; Procedure evaluation rules allow
; operators to be compound expressions!

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

