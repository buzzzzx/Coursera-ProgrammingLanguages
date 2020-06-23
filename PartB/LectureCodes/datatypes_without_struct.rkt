#lang racket

(provide (all-defined-out))

;; Datatype bindings
(define (Const e) (list 'Const e))
(define (Negate e) (list 'Negate e))
(define (Add e1 e2) (list 'Add e1 e2))
(define (Multiply e1 e2 ) (list 'Multiply e1 e2))

;; Helper functions that test "what the kind of expression"
(define (const? e) (eq? 'Const (car e)))
(define (negate? e) (eq? 'Negate (car e)))
(define (add? e) (eq? 'Add (car e)))
(define (multiply? e) (eq? 'Multiply (car e)))

;; Helper functions that extract e
(define (const-int e) (car (cdr e)))
(define (negate-e e) (car (cdr e)))
(define (add-e1 e) (car (cdr e)))
(define (add-e2 e) (car (cdr (cdr e))))
(define (multiply-e1 e) (car (cdr e)))
(define (multiply-e2 e) (car (cdr (cdr e))))

;; Evaluation
(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e) (Const (- (const-int (eval-exp (negate-e e)))))]
        [(add? e) (let ([v1 (const-int (eval-exp (add-e1 e)))]
                        [v2 (const-int (eval-exp (add-e2 e)))])
                    (Const (+ v1 v2)))]
        [(multiply? e) (let ([v1 (const-int (eval-exp (multiply-e1 e)))]
                             [v2 (const-int (eval-exp (multiply-e2 e)))])
                    (Const (* v1 v2)))]))
        