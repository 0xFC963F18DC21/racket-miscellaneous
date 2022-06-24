#lang lazy

;;; Challenges from 1.4
(define (id x)
  x)

(define (comp f g)
  (lambda (x) (f (g x))))
