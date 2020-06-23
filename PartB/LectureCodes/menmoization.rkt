#lang racket

(provide (all-defined-out))

(define (fibonacci1 x)
  (if (or (= x 1) (= x 2))
      1
      (+ (fibonacci1 (- x 1))
         (fibonacci1 (- x 2)))))

(define fibonacci2
  (letrec ([memo null] ; memo is list of pairs [(arg res)]
           [f (lambda (x)
                (letrec ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (letrec ([new-ans (if (or (= x 1) (= x 2))
                                            1
                                            (+ (f (- x 1))
                                               (f (- x 2))))])
                        (begin (set! memo (cons (cons x new-ans) memo))
                               new-ans)))))])
    f))