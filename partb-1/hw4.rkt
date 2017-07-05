
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file
;; put your code below
;; 1  number number number

(define debug #t)

(define (sequence low high stride)
  (if (> low high)
      '()
      (cons low (sequence (+ stride low) high stride))))

;(if debug
;    (begin (sequence 3 11 2)
;           (sequence 3 8 3)
;           (sequence 3 2 1)) "")
;    

;; 2

(define (string-append-map xs suffix)
  (map (lambda (s)(string-append s suffix)) xs))


;(string-append-map (list "a" "b" "c" "d") "f")

;; 3 
(define (list-nth-mod xs n)
  (cond [(> 0 n) (error "list-nth-mod: negative number")]
        [(null? xs)  (error "list-nth-mod: empty list")]
        [#t(car
            (list-tail xs
                       (remainder
                        n (length xs))))]))
;(list-nth-mod '() 1)
;(list-nth-mod (list 1 2 3 4 5) -1 )
;(list-nth-mod (list 1 2 3 4 5) 1 )
;(list-nth-mod (list 1 2 3 4 5) 5 )
;(list-nth-mod (list 1 2 3 4 5) 6 )
(define ones (lambda () (cons 1 ones)))

(define (ziranshu-stream)
  (letrec ([f (lambda(x)(cons x (lambda () (f (+ x 1)))))])
    (f 1)))


(define (stream-for-n-steps s n)
  (let ([pair-value (s)])
    (if (> 1 n)
        '()
        (cons (car pair-value) (stream-for-n-steps (cdr pair-value) (- n 1 ))))))

;(stream-for-n-steps ones 10)
;(stream-for-n-steps ziranshu-stream 10)
  
  