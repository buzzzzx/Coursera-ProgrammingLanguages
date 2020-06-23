#lang racket

(provide (all-defined-out))

;; This is the solution for hw4
;; Remie Choo
;; 2020.03.08

;; P1 - sequence
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;; P2 - string-append-map
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;; P3 - list-nth-mod
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

;; P4 - stream-for-n-steps
(define (stream-for-n-steps s n)
  (letrec ([pr (s)])
    (if (= n 0)
        null
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

;; P5 - funny-number-stream
(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([y (if (= 0 (remainder x 5))
                             (- x)
                             x)])
                 (cons y (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

;; P6 - dan-then-dog
(define dan-then-dog
    (letrec ([f (lambda (x)
                (let ([y (if (odd? x) "dan.jpg" "dog.jpg")])
                 (cons y (lambda () (f (+ x 1))))))])
      (lambda () (f 1))))

;; P7 - stream-add-zero
(define (stream-add-zero s)
  (let ([pr (s)])
    (lambda () (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))

;; P8 - cycle-lists
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (let ([first (list-nth-mod xs n)]
                      [snd (list-nth-mod ys n)])
                  (cons (cons first snd) (lambda () (f (+ n 1))))))])
    (lambda () (f 0))))

;; P9 - vector-assoc
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (cond [(equal? (vector-length vec) n) #f]
                      [(not (pair? (vector-ref vec n))) (f (+ n 1))]
                      [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)]
                      [#t (f (+ n 1))]))])
    (f 0)))

;; P10 - cached-assoc
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [modified-next 0])
    (lambda (v)
      (let ([ans (vector-assoc v memo)])
        (if ans
            ans
            (let ([new-ans (assoc v xs)])
              (begin (vector-set! memo modified-next new-ans)
                     (set! modified-next (if (= modified-next (- n 1))
                                             0
                                             (+ modified-next 1)))
                     new-ans)))))))
  

;; Challenge - P11 - while-less
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([x e1]
              [f (lambda ()
                   (if (< e2 x)
                       (f)
                       #t))])
       (f))]))
              
       
  
                            
        