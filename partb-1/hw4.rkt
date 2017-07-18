
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

; 4
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


;5
(define (funny-number-stream)
  (letrec ([f (lambda (x) (cons (if (= 0 (remainder x 5))
                                    (- x)
                                    x)
                                (lambda () (f ( + x 1)))))])
    (f 1)))

;(stream-for-n-steps funny-number-stream 20)



;6
(define (dan-then-dog)
  (letrec ([f (lambda (name)
                (cons name
                      (lambda () (f (cond [(equal? "dan.jpg" name) "dog.jpg"]
                                          [#t "dan.jpg"])))))])
    (f "dan.jpg")))


;(stream-for-n-steps dan-then-dog 20)

;7

(define (stream-add-zero s)
  (letrec ([f (lambda (x)
             (cons (cons 0 (car x))
                   (lambda () (f ((cdr x))))))])
    (f (s))))


;(stream-for-n-steps (lambda () (stream-add-zero dan-then-dog)) 20)



;8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (xs ys)
                (cons (cons (car xs) (car ys))
                      (lambda () (f (append (cdr xs) (cons (car xs) null)) (append (cdr ys) (cons (car ys) null))))))])
    (f xs ys)))


;(stream-for-n-steps (lambda () (cycle-lists (list 1 2 3) (list "a" "b" "c" "d"))) 20)
                     

;9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (= n (vector-length vec))
                    #f
                    (let ([t (vector-ref vec n)])
                      (if (pair? t)
                          (if (equal? v (car t))
                              t
                              (f (+ 1 n)))
                          (f (+ 1 n))))))])
          (f 0)))
                    
;(vector-assoc 1 (vector 0 0 0))
;(vector-assoc 1 (vector (list 1 2) 0 0))