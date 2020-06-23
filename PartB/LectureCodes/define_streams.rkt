#lang racket

(provide (all-defined-out))

; 1 1 1 ...
(define ones (lambda () (cons 1 ones)))

; 1 2 3 ...
(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

; 2 4 6 8 ...
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (lambda () (f 2))))

; Using streams
(define (use_stream stream tester)
  (letrec ([f (lambda (stream acc)
                (letrec ([pr (stream)])
                  (if (tester (car pr))
                      acc
                      (f (cdr pr) (+ acc 1)))))])
    (f stream 1)))

