#lang racket

(provide (all-defined-out))

(define (slow_add x y)
  (letrec ([slow_id (lambda (m n)
                      (if (= n 0)
                          m
                          (slow_id m (- n 1))))])
    (+ (slow_id x 50000000) y)))

(define (my_mult x y-thunk)
  (if (= x 0)
      0
      (if (= x 1)
          (y-thunk)
          (+ (y-thunk) (my_mult (- x 1) y-thunk)))))

(define (my_delay th)
  (mcons #f th))

(define (my_force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))