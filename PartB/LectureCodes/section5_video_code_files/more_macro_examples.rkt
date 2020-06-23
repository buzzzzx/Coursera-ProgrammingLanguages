#lang racket

(provide (all-defined-out))

(define-syntax for
  (syntax-rules (to do)
    [(for from to end do body)
     (letrec ([l from]
              [h end])
       (letrec ([loop (lambda (l)
                        (if (> l h)
                            #t
                            (begin body (loop (+ l 1)))))])
         (loop l)))]))

(define-syntax my-let*
  (syntax-rules ()
    [(my-let* () body) body]
    [(my-let* ([var0 val0] [var1 val1] ...) body)
     (let ([var0 val0])
       (my-let* ([var1 val1] ...) body))]))