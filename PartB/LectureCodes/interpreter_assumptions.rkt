#lang racket

(provide (all-defined-out))

(struct const (int) #:transparent)
(struct negate (e) #:transparent)
(struct add (e1 e2) #:transparent)
(struct multiply (e1 e2) #:transparent)
(struct bool (e) #:transparent)
(struct eq-num (e1 e2) #:transparent)
(struct if-then-else (e1 e2 e3) #:transparent)


;; Interpreter
(define (eval-exp e)
  (cond [(const? e) e]
        [(negate? e) (let ([e1 (eval-exp (negate-e e))])
                       (if (const? e1)
                           (const (- (const-int e1)))
                           (error "nagate applied to non-number.")))]
        [(add? e) (let ([e1 (eval-exp (add-e1 e))]
                        [e2 (eval-exp (add-e2 e))])
                    (if (and (const? e1) (const? e2))
                        (const (+ (const-int e1) (const-int e2)))
                        (error "add applied to non-number.")))]
        [(multiply? e) (let ([e1 (eval-exp (multiply-e1 e))]
                             [e2 (eval-exp (multiply-e2 e))])
                    (if (and (const? e1) (const? e2))
                        (const (* (const-int e1) (const-int e2)))
                        (error "multiply? applied to non-number.")))]
        [(bool? e) e]
        [(eq-num? e) (let ([e1 (eval-exp (eq-num-e1 e))]
                           [e2 (eval-exp (eq-num-e2 e))])
                       (if (and (const? e1) (const? e2))
                           (bool (= (const-int e1) (const-int e2)))
                           (error "eq-num applied to non-number.")))]
        [(if-then-else? e) (let ([b (eval-exp (if-then-else-e1 e))])
                             (if (bool? b)
                                 (let ([e2 (eval-exp (if-then-else-e2 e))]
                                       [e3 (eval-exp (if-then-else-e3 e))])
                                   (if (bool-e b) e2 e3))
                                 (error "if-then-else applied to non-boolean.")))]
        [#t (error "eval-exp expected an exp.")]                        
        ))