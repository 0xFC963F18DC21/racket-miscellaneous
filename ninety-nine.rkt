#lang racket

; This set of problems comes from:
; https://www.ic.unicamp.br/~meidanis/courses/mc336/problemas-lisp/L-99_Ninety-Nine_Lisp_Problems.html

; --- Section 1: Lists ---

; P01: Last item in list.
(define (my-last lst)
  (cond
    [(empty? (cdr lst)) (car lst)]
    [else               (my-last (cdr lst))]))

; P02: Second-to-last item in list.
(define (my-but-last lst)
  (cond
    [(empty? (cddr lst)) (car lst)]
    [else                (my-but-last (cdr lst))]))

; P03: 1-indexed nth element of list.
(define (element-at lst idx)
  (cond
    [(= 1 idx) (car lst)]
    [else      (element-at (cdr lst) (- idx 1))]))

; P04: Length of list.
(define (my-length lst)
  (cond
    [(empty? lst) 0]
    [else         (+ 1 (my-length (cdr lst)))]))

; P05: Reverse of a list.
(define (my-reverse lst [acc empty])
  (cond
    [(empty? lst) acc]
    [else         (my-reverse (cdr lst) (cons (car lst) acc))]))

; P06: Is the list palindromic?
(define (palindrome? lst)
  (equal? lst (my-reverse lst)))

; P07: Flatten nested lists.
(define (my-flatten nlsts [acc empty])
  (cond
    [(empty? nlsts)      acc]
    [(list? (car nlsts)) (append acc
                                 (my-flatten (car nlsts))
                                 (my-flatten (cdr nlsts)))]
    [else                (append acc
                                 (list (car nlsts))
                                 (my-flatten (cdr nlsts)))]))

; P08: Eliminate consecutive duplicates.
(define (compress lst [last (gensym)])
  (cond
    [(empty? lst)            lst]
    [(equal? last (car lst)) (compress (cdr lst) (car lst))]
    [else                    (cons (car lst) (compress (cdr lst) (car lst)))]))

; P09: Pack consecutive duplicates into sub-lists.
(define (pack lst [i-acc empty] [l-acc empty])
  (cond
    [(empty? lst)                        (append l-acc
                                                 (list i-acc))]
    [(or (empty? i-acc)
         (equal? (car lst) (car i-acc))) (pack (cdr lst)
                                               (cons (car lst) i-acc)
                                               l-acc)]
    [else                                (pack (cdr lst)
                                               (list (car lst))
                                               (append l-acc (list i-acc)))]))

; P10: Run-length encoding of a list.
(define (encode lst)
  (map (λ (l) (cons (my-length l) (list (car l))))
       (pack lst)))

; P11: Modified run-length encoding of a list.
(define (singleton-replace lst)
  (if (= 1 (car lst))
      (cadr lst)
      lst))

(define (encode-mod lst)
  (map singleton-replace (encode lst)))

; P12: Decoding of a run-length encoded list.
(define (decode en-lst)
  (flatten (map (λ (pr) (make-list (car pr) (my-last pr))) en-lst)))

(define (decode-mod en-lst)
  (flatten (map (λ (sng/pr)
                  (if (list? sng/pr)
                      (make-list (car sng/pr) (my-last sng/pr))
                      sng/pr))
                en-lst)))

; P13: Direct run-length encoding of a list.
(define (encode-item item n)
  (cond
    [(<= n 0) empty]
    [(= n 1)  (list item)]
    [else     (list (list n item))]))

(define (encode-mod-dir lst [i-acc 0] [item (gensym)] [l-acc empty])
  (cond
    [(empty? lst)            (append l-acc (encode-item item i-acc))]
    [(equal? (car lst) item) (encode-mod-dir (cdr lst) (+ 1 i-acc) item l-acc)]
    [(= 0 i-acc)             (encode-mod-dir (cdr lst) 1 (car lst) l-acc)]
    [else                    (encode-mod-dir (cdr lst)
                                             1
                                             (car lst)
                                             (append l-acc
                                                     (encode-item item i-acc)))]))

; P14: Duplicate all items in list.
(define (duplicate lst)
  (flatten (map (λ (item) (list item item)) lst)))

; P15: Replicate all items in list n times.
(define (replicate lst n)
  (flatten (map (λ (item) (make-list n item)) lst)))

; P16: Drop the nth items of a list.
(define (drop-nth lst n [counter #f])
  (cond
    [(empty? lst)   empty]
    [(not counter)  (drop-nth lst n n)]
    [(<= counter 1) (drop-nth (cdr lst) n n)]
    [else           (cons (car lst)
                          (drop-nth (cdr lst) n (- counter 1)))]))

; P17: Split list at index.
(define (my-split-at lst n [acc empty])
  (cond
    [(<= n 0) (list (reverse acc) lst)]
    [else     (my-split-at (cdr lst) (- n 1) (cons (car lst) acc))]))

; P18: Slice a list between indices.
(define/contract (my-slice lst st en)
  (->i ([lst    list?]
        [st     number?]
        [en     (st) (and/c number? (>=/c st))])
       [result list?])
  (cond
    [(and (<= st 1) (< en 1)) empty]
    [(<= st 1)                (cons (car lst) (my-slice (cdr lst) (- st 1) (- en 1)))]
    [else                     (my-slice (cdr lst) (- st 1) (- en 1))]))

; P19: Left-rotate a list.
(define (my-rotl lst n)
  (if (= n 0)
      lst
      (match-let* ([places     (if (> n 0) n (modulo n (length lst)))]
                   [(list l r) (my-split-at lst places)])
        (append r l))))

; P20: Remove the kth element of a list.
(define/contract (remove-at lst pos)
  (-> list? (and/c number? (>/c 0)) list?)
  (if (= pos 1)
      (cdr lst)
      (cons (car lst) (remove-at (cdr lst) (- pos 1)))))

; P21: Insert an element at the kth position of a list.
(define/contract (insert-at item lst pos)
  (-> any/c list? (and/c number? (>/c 0)) list?)
  (if (= pos 1)
      (cons item lst)
      (cons (car lst) (insert-at item (cdr lst) (- pos 1)))))

; P22: Inclusive range of integers.
(define (my-range st en)
  (cond
    [(= st en) (list st)]
    [(< st en) (cons st (my-range (+ st 1) en))]
    [(> st en) (cons st (my-range (- st 1) en))]))
