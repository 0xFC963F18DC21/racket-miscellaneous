#lang racket

(define (get-guess)
  (let ([guess-str (string-trim (read-line))])
    (string->number guess-str)))

(define (check-guess correct guess)
  (cond [(or (< guess 1) (> guess 100)) (display "Out of range!\n\n") #f]
        [(< guess correct)              (display "Too low!\n\n") #f]
        [(> guess correct)              (display "Too high!\n\n") #f]
        [else                           (display "Congratulations, you win!\n\n") #t]))

(define (game [correct (random 1 101)])
  (display "Guess a number between 1 and 100:\n")
  (let ([guess (get-guess)])
    (if guess
        (unless (check-guess correct guess) (game correct))
        (begin (display "Guess was not a number!\n\n")
               (game correct)))))

(game)
