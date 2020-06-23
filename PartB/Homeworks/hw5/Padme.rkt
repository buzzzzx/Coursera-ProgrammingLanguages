;; Programming Languages - Padme

;; Remie Choo
;; 2020.03.10

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for Padme programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0
(struct str  (e) #:transparent) ;; a string
(struct concat (e1 e2) #:transparent) ;; concat two words

;; a closure is not in "source" programs but /is/ a Padme value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->padmelist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->padmelist (cdr xs)))))

(define (padmelist->racketlist ms)
  (if (aunit? ms)
      null
      (cons (apair-e1 ms) (padmelist->racketlist (apair-e2 ms)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of Padme expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "Padme addition applied to non-number")))]
        [(int? e) e]
        [(aunit? e) e]
        [(closure? e) e]
        [(fun? e) (closure env e)] 
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "Padme ifgreater(e1 or e2) applied to non-number")))]
        [(mlet? e) (let ([v (eval-under-env (mlet-e e) env)])
                     (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(call? e)
         (let ([cls (eval-under-env (call-funexp e) env)]
               [argu-val (eval-under-env (call-actual e) env)])
           (if (closure? cls)
               (let* ([cls-env (closure-env cls)]
                      [func (closure-fun cls)]
                      [argu-name (fun-formal func)]
                      [is-recursive (fun-nameopt func)])
                 (if is-recursive
                     (eval-under-env (fun-body func) (cons (cons is-recursive cls) (cons (cons argu-name argu-val) cls-env)))
                     (eval-under-env (fun-body func) (cons (cons argu-name argu-val) cls-env))))
               (error "Padme call applied to non-closure")))]        
        [(apair? e) (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e) (let ([re (eval-under-env (fst-e e) env)])
                    (if (apair? re)
                        (apair-e1 re)
                        (error "Padme fst applied to non-apair")))]
        [(snd? e) (let ([re (eval-under-env (snd-e e) env)])
                    (if (apair? re)
                        (apair-e2 re)
                        (error "Padme snd applied to non-apair")))]
        [(isaunit? e) (if (aunit? (eval-under-env (isaunit-e e) env))
                          (int 1)
                          (int 0))]
        [(str? e) e]
        [(concat? e) 
         (let ([v1 (eval-under-env (concat-e1 e) env)]
               [v2 (eval-under-env (concat-e2 e) env)])
           (if (and (str? v1)
                    (str? v2))
               (str (string-append (str-e v1) 
                       (str-e v2)))
               (error "Padme addition applied to non-string")))]
               
        [#t (error (format "bad Padme expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))


(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4

(define padme-map
  (fun "padme-map" "func"
       (fun #f "mlst"
            (ifaunit (var "mlst")
                     (aunit)
                     (apair (call (var "func") (fst (var "mlst"))) (call (call (var "padme-map") (var "func")) (snd (var "mlst")))))))) 

(define padme-mapAddN 
  (mlet "map" padme-map
        (fun #f "i"
             (fun #f "mlst"
                  (call (call (var "map") (fun #f "x" (add (var "x") (var "i")))) (var "mlst"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

;; Test cases
(define padme-map-force
  (mlet "map" padme-map
        (fun #f "lines"
             (fun #f "name-lst"
                  (call (call (var "map") (fun #f "x" (concat (var "x") (var "lines")))) (var "name-lst"))))))
