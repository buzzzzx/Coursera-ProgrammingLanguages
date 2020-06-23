#lang racket

(provide (all-defined-out))

(define (number-until stream tester)
  (letrec ([f (lambda (stream acc)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      acc
                      (f (cdr pr) (+ acc 1)))))])
    (f stream 1)))