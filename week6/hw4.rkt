
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
; problem 1
(define (sequence low high stride)
  (cond [(> low high) null]
        [#t (cons low (sequence (+ low stride) high stride))]))

; problem 2
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([foo (remainder n (length xs))])
               (car (list-tail xs foo)))]))

; problem 4
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

; problem 5
(define funny-number-stream 
  (stream-maker (lambda (x y)
                  (let* ([x (if (< x 0) (- 0 x) x)]
                           [foo (+ x y)])
                          (if (= (remainder (+ x y) 5) 0)
                           (- 0 foo)
                           foo))) 1))

; problem 6
(define (dan-then-dog)
  (letrec ([f (lambda (x)
                   (cons x (lambda ()
                             (f (if (string=? x "dan.jpg") "dog.jpg" x)))))])
    (f "dan.jpg")))

; problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda (s)
                (cons (cons 0 (car (s))) (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

; problem 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (i)
                (cons (cons (list-nth-mod xs i) (list-nth-mod ys i))
                      (lambda () (f (+ i 1)))))])
    (lambda () (f 0))))

; problem 9
(define (vector-assoc v vec)
  (letrec ([len (vector-length vec)]
           [f (lambda (i)
                (if (< i len)
                (letrec ([item (vector-ref vec i)])
                  (cond [(and (pair? item) (equal? (car item) v)) item]
                        [#t (f (+ i 1))]))
                #f))])
    (f 0)))

; problem 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [guard 0]
           [f (lambda (v)
                (let ([foo (vector-assoc v cache)])
                  (if (equal? foo #f)
                      (if (equal? (assoc v xs) #f)
                          #f
                          (begin (vector-set! cache guard (assoc v xs))
                                 (set! guard (remainder (+ guard 1) n))
                                 (assoc v xs)))
                      foo)))])
    (lambda (v) (f v)))) 
