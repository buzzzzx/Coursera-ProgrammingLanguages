#lang racket

(provide (all-defined-out))

(define xs (list 1 2 3 4))
(define ys (list 1 2 (list 3 4 (list 5 "hi")) 6 #t 10 (list 7 8) 9))

(define (sum xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum (cdr xs)))
          (if (list? (car xs))
              (+ (sum (car xs)) (sum (cdr xs)))
              (sum (cdr xs))))))

(define (sum2 xs)
  (if (number? xs)
      xs
      (if (list? xs)
          (if (null? xs)
              0
              (if (number? (car xs))
                  (+ (car xs) (sum2 (cdr xs)))
                  (if (list? (car xs))
                      (+ (sum2 (car xs)) (sum2 (cdr xs)))
                      (sum2 (cdr xs)))))
          "Wrong Input.")))

(define (sum-cond xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum (cdr xs)))]
        [(list? (car xs)) (+ (sum (car xs)) (sum (cdr xs)))]
        [#t (sum (cdr xs))]))