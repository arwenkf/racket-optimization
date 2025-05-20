#lang racket
(provide constant-fold safe? optimize)
(require "ast.rkt"
         "parse.rkt"
         "finterp.rkt"
         "fv.rkt")


;; Expr -> Bool
(define (safe? e)
    ; else optimize all the subexpressions
    (match e
    [(Lit d)            #t]
    [(Eof)              #t]
    [(Var x)            #t]
    [(Prim0 p)          (prim0-safe? p)]
    [(Prim1 p e)        (and (prim1-safe? p) (safe? e))]
    [(Prim2 p e1 e2)    (and (safe? e1) (safe? e2))]
    [(Prim3 p e1 e2 e3) (and (prim3-safe? p) (safe? e1) (safe? e2) (safe? e3))]
    [(If e1 e2 e3)      (and (safe? e1) (safe? e2) (safe? e3))]
    [(Begin e1 e2)      (and (safe? e1) (safe? e2))]
    [(Let x e1 e2)      (and (safe? e1) (safe? e2))]
    [(App e es)         #f]
    [(Lam f xs e)       #f]
    [(Match e ps es)    #t]))

(define (prim0-safe? p)
    (match p
        ['read-byte #f]
        ['peek-byte #f]
        ['collect-garbage #f]
        [_ #t]))

(define (prim1-safe? p)
    (match p
        ['write-byte #f]
        [_ #t]))

(define (prim3-safe? p)
    (match p
        ['vector-set! #f]
        [_ #t]))

;; Expr -> Expr
(define (constant-fold e t)
    ;; if theres no side effects and just one value, use that value
 (if (and (safe? e) (table-single? t e)) 
    (val2exp (table-value t e))
    ; else optimize all the subexpressions
    (match e
    [(Prim1 p e)        (optimize-prim1 p e t)]
    [(Prim2 p e1 e2)    (optimize-prim2 p e1 e2 t)]
    [(Prim3 p e1 e2 e3) (optimize-prim3 p e1 e2 e3 t)]
    [(If e1 e2 e3)      (optimize-if e1 e2 e3 t)]
    [(Let x e1 e2)      (optimize-let x e1 e2 t)]
    [(Begin e1 e2)      (optimize-begin e1 e2 t)]
    [(Lam f xs e)       (Lam f xs (constant-fold e t))]
    [(App e es) 
        (App (constant-fold e t) (map (lambda (e) (constant-fold e t)) es))]
    [(Match e ps es) 
        (Match (constant-fold e t) ps (map (lambda (e) (constant-fold e t)) es))]
    [_ e])))


;; Op1 Expr Table -> Expr
(define (optimize-prim1 p1 e t)
 (match (list p1 e)
 [_ (Prim1 p1 (constant-fold e t))]))

 ;; Op2 Expr Table -> Expr
(define (optimize-prim2 p2 e1 e2 t)
 (match (list p2 e1 e2)
 [_ (Prim2 p2 (constant-fold e1 t) (constant-fold e2 t))]))

 ;; Op3 Expr Table -> Expr
(define (optimize-prim3 p3 e1 e2 e3 t)
 (match (list p3 e1 e2 e3)
 [_ (Prim3 p3 (constant-fold e1 t) (constant-fold e2 t) (constant-fold e3 t))]))
 
 ;; Expr Expr Expr Table -> Expr
(define (optimize-if e1 e2 e3 t)
(let ([s (hash-ref t e1)])
    (cond
       [(equal? s (set '(#f #hasheq())))
        (let ([e1o (constant-fold e1 t)] [e3o (constant-fold e3 t)]) (if (safe? e1o) e3o (Begin e1o e3o)))] 
       [(set-member? s '(#f #hasheq()))
        (If (constant-fold e1 t) (constant-fold e2 t) (constant-fold e3 t))] 
       [else (let ([e1o (constant-fold e1 t)] [e2o (constant-fold e2 t)]) (if (safe? e1o) e2o (Begin e1o e2o)))])))   

  ;; Var Expr Expr Table -> Expr
(define (optimize-let x e1 e2 t)
    (let* ([e1o (constant-fold e1 t)] [e2o (constant-fold e2 t)] [used-vars (fv e2o)] )
        (if (member x used-vars) (Let x e1o e2o) (if (safe? e1o) e2o (Begin e1o e2o)))
    )
)

  ;; Expr Expr Table -> Expr
(define (optimize-begin e1 e2 t)
    (let ((e1-op (constant-fold e1 t)))
        (if (safe? e1-op)
            (constant-fold e2 t)
            (Begin e1-op (constant-fold e2 t))))
 )

; Prog -> Prog
(define (optimize p)
    (define t (analyze p))
    (match p
      [(Prog ds e)
       (Prog ds (constant-fold e t))]))

;; TODO only return true if lookup returns a literal
;; Table Expr -> Bool
(define (table-single? t e)
  (and (eq? (set-count (table-lookup t e)) 1) (let ((v (table-value t e))) (cond 
        [(eq? v #t) #t]
        [(eq? v #f) #t]
        [(integer? v) #t]
        [(eof-object? v) #t]
        [(void? v) #t]
        ;;; [(empty? v)      #b10011000]
        [(char? v) #t]
        [else #f])
  )))

;; TODO make this function return 
;; i started based on val->bits, havent done the complex ones yet
;; Value -> Expr
(define (val2exp v)
    (cond [(eq? v #t) (Lit #t)]
        [(eq? v #f) (Lit #f)]
        [(integer? v) (Lit v)]
        [(eof-object? v) (Eof)]
        [(void? v) (Prim0 'void)]
        ;;; [(empty? v)      #b10011000]
        [(char? v) (Lit v)]
        [else (error "not an immediate value" v)])
    )

;; Table Expr -> Value
(define (table-value t e)
;;TODO get the fucking value
  (match (set-first (table-lookup t e))
  [(list v s) v]
  ))
