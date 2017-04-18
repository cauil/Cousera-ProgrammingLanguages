
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
  (cond [(> low high) null]
        [#t (cons low (sequence (+ low stride) high stride))])

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([foo (remainder (length xs) n)])
               (car (list-tail xs foo)))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
    null
    (let ([foo (s)])
      (cons (car foo) (stream-for-n-steps (cdr foo) (- n 1))))))

;version 1
;(define (funny-number-helper x)
;  (let ([foo (if (= (remainder x 5) 0)
;                            (- 0 x)
;                            x)])
;               (cons foo (lambda () (funny-number-helper (+ x 1))))))
;
;(define (funny-number-stream) (lambda () (funny-number-helper 1)))

;version 2
(define (stream-maker fn arg)
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (fn x arg)))))])
    (lambda () (f 1))))

(define funny-number-stream 
  (stream-maker (lambda (x y)
                  (let* ([x (if (< x 0) (- 0 x) x)]
                           [foo (+ x y)])
                          (if (= (remainder (+ x y) 5) 0)
                           (- 0 foo)
                           foo))) 1))
