#lang racket

;;;; This is a program walkthrough through the examples given in the book "The Little Schemer"
;;;; 4th Edition by Daniel P. Friedman and Matthias Felleisen.

;;; Guideline Function: atom?
(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

;;; Toys
(define (lat? l)
  (cond
    [(null? l)       #t]
    [(atom? (car l)) (lat? (cdr l))]
    [else            #f]))

(define (lat-? l)
  (or (null? l)
      (and (atom? (car l))
           (lat-? (cdr l)))))

(define (member? a lat)
  (cond
    [(null? lat) #f]
    [else        (or (eq? (car lat) a)
                     (member? a (cdr lat)))]))

(define (member-? a lat)
  (and (not (null? lat))
       (or (eq? (car lat) a)
           (member-? a (cdr lat)))))

;;; Cons the Magnificent
(define (rember a lat)
  (cond
    [(null? lat)       (quote ())]
    [(eq? (car lat) a) (cdr lat)]
    [else              (cons (car lat)
                             (rember a (cdr lat)))]))

(define (firsts l)
  (cond
    [(null? l) (quote ())]
    [else      (cons (caar l)
                     (firsts (cdr l)))]))

(define (firsts- l)
  (if (null? l)
      (quote ())
      (cons (caar l)
            (firsts- (cdr l)))))

(define (insertR new old lat)
  (cond
    [(null? lat)         (quote ())]
    [(eq? (car lat) old) (cons (car lat)
                               (cons new
                                     (cdr lat)))]
    [else                (cons (car lat)
                               (insertR new old (cdr lat)))]))

(define (insertL new old lat)
  (cond
    [(null? lat)         (quote ())]
    [(eq? (car lat) old) (cons new lat)]
    [else                (cons (car lat)
                               (insertL new old (cdr lat)))]))

(define (subst new old lat)
  (cond
    [(null? lat)         (quote ())]
    [(eq? (car lat) old) (cons new
                               (cdr lat))]
    [else                (cons (car lat)
                               (subst new old (cdr lat)))]))

(define (subst2 new o1 o2 lat)
  (cond
    [(null? lat)         (quote ())]
    [(or (eq? (car lat) o2)
         (eq? (car lat) o1)) (cons new (cdr lat))]
    [else                (cons (car lat)
                               (subst2 new o1 o2 (cdr lat)))]))

(define (multirember a lat)
  (cond
    [(null? lat)       (quote ())]
    [(eq? (car lat) a) (multirember a (cdr lat))]
    [else              (cons (car lat)
                             (multirember a (cdr lat)))]))
