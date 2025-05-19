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
 (Prim1 p1 (constant-fold e t)))

 ;; Op2 Expr Table -> Expr
(define (optimize-prim2 p2 e1 e2 t)
 (Prim1 p2 (constant-fold e1 t) (constant-fold e2 t)))

 ;; Op3 Expr Table -> Expr
(define (optimize-prim3 p3 e1 e2 e3 t)
 (Prim1 p3 (constant-fold e1 t) (constant-fold e2 t) (constant-fold e3 t)))
 
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
  (define (optimize-loop p)
    (define t (analyze p))
    (match p
      [(Prog ds e)
       (define p2 (Prog ds (constant-fold e t)))
       (if (equal? p p2)
           p
           (optimize-loop p2))]))
  (optimize-loop p))

; (define (var-used? e x)
;     (match e
;     [(Prim1 p e)        (var-used? e x)]
;     [(Prim2 p e1 e2)    (or (var-used? e1 x) (var-used? e2 x))]
;     [(Prim3 p e1 e2 e3) (or (var-used? e1 x) (var-used? e2 x) (var-used? e3 x))]
;     [(If e1 e2 e3)      (or (var-used? e1 x) (var-used? e2 x) (var-used? e3 x))]
;     [(Let x e1 e2)      (if (and (var-used? e1 x) )]
;     [(Begin e1 e2)      (or (var-used? e1 x) (var-used? e2 x))]

; ) 
;;; ;;; (Prog ds e)
;;; ;;;      (if (table-single? t e) 
;;; ;;;      ;; if theres only one thing in the table
;;; ;;;      (prog (Global 'entry)
;;; ;;;            (Extern 'raise_error)
;;; ;;;            (Label 'entry)
;;; ;;;            (Push rbx)    ; save callee-saved registers
;;; ;;;            (Push rbp)
;;; ;;;            (Push r15)
;;; ;;;            (Mov rbp rsp) ; save stack base pointer
;;; ;;;            (Mov rbx rdi) ; recv heap pointer
;;; ;;;            (compile-value (table-value t e))
;;; ;;;            (Pop r15)     ; restore callee-save registers
;;; ;;;            (Pop rbp)
;;; ;;;            (Pop rbx)
;;; ;;;            (Ret)
;;; ;;;            (Label 'err)
;;; ;;;            pad-stack
;;; ;;;            (Call 'raise_error))


;; TODO only return true if lookup returns a literal
;; Table Expr -> Bool
(define (table-single? t e)
;;TODO make sure its not abstract
  (eq? (set-count (table-lookup t e)) 1))

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

;;;     (define (value->bits v)
;;;   (cond [(eq? v #t) #b00011000]
;;;         [(eq? v #f) #b00111000]
;;;         [(integer? v) (arithmetic-shift v int-shift)]
;;;         [(eof-object? v) #b01011000]
;;;         [(void? v) #b01111000]
;;;         [(empty? v)      #b10011000]
;;;         [(char? v)
;;;          (bitwise-ior type-char
;;;                       (arithmetic-shift (char->integer v) char-shift))]
;;;         [else (error "not an immediate value" v)]))

;; Table Expr -> Value
(define (table-value t e)
;;TODO get the fucking value
  (match (set-first (table-lookup t e))
  [(list v s) v]
  ))


;;;   ;;; (let [soa (set->list (table-lookup t e))] ( ;; soa is set of answers
;;;   ;;;                                (andmap (λ (v) (eq? (car soa) v)
;;;   ;;;                                 soa))
;;;   ;;;                                ))


;;;   ;; type CEnv = (Listof [Maybe Id])
;;; ;; Expr CEnv Boolean Table -> Asm
;;; ;;; (define (optimize-e e c t? t)
;;; ;;;   (match e
;;; ;;;     [(Lit d) (optimize-value d)]
;;; ;;;     [(Eof) (optimize-value eof)]
;;; ;;;     [(Var x) (optimize-variable x c)]
;;; ;;;     [(Prim0 p) (optimize-prim0 p)]
;;; ;;;     [(Prim1 p e) (optimize-prim1 p e c t)]
;;; ;;;     [(Prim2 p e1 e2) (optimize-prim2 p e1 e2 c t)]
;;; ;;;     [(Prim3 p e1 e2 e3) (optimize-prim3 p e1 e2 e3 c t)]
;;; ;;;     [(If e1 e2 e3)
;;; ;;;      (optimize-if e1 e2 e3 c t? t)]
;;; ;;;     [(Begin e1 e2)
;;; ;;;      (optimize-begin e1 e2 c t? t)]
;;; ;;;     [(Let x e1 e2)
;;; ;;;      (optimize-let x e1 e2 c t? t)]
;;; ;;;     [(App e es)
;;; ;;;      (optimize-app e es c t? t)]
;;; ;;;     [(Lam f xs e)
;;; ;;;      (optimize-lam f xs e c)]
;;; ;;;     [(Match e ps es) (optimize-match e ps es c t?)]))

;;; ;;; ;; Value -> Asm
;;; ;;; (define (optimize-value v)
;;; ;;;   (cond [(string? v) (optimize-string v)]
;;; ;;;         [else        (Mov rax (value->bits v))]))

;;; ;;; ;; Id CEnv -> Asm
;;; ;;; (define (optimize-variable x c)
;;; ;;;   (let ((i (lookup x c)))
;;; ;;;     (seq (Mov rax (Offset rsp i)))))

;;; ;;; ;; String -> Asm
;;; ;;; (define (optimize-string s)
;;; ;;;   (let ((len (string-length s)))
;;; ;;;     (if (zero? len)
;;; ;;;         (seq (Mov rax type-str))
;;; ;;;         (seq (Mov rax len)
;;; ;;;              (Mov (Offset rbx 0) rax)
;;; ;;;              (optimize-string-chars (string->list s) 8)
;;; ;;;              (Mov rax rbx)
;;; ;;;              (Xor rax type-str)
;;; ;;;              (Add rbx
;;; ;;;                   (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

;;; ;;; ;; [Listof Char] Integer -> Asm
;;; ;;; (define (optimize-string-chars cs i)
;;; ;;;   (match cs
;;; ;;;     ['() (seq)]
;;; ;;;     [(cons c cs)
;;; ;;;      (seq (Mov rax (char->integer c))
;;; ;;;           (Mov (Offset rbx i) 'eax)
;;; ;;;           (optimize-string-chars cs (+ 4 i)))]))

;;; ;;; ;; Op0 -> Asm
;;; ;;; (define (optimize-prim0 p)
;;; ;;;   (optimize-op0 p))

;;; ;;; ;; Op1 Expr CEnv Table -> Asm
;;; ;;; (define (optimize-prim1 p e c t)
;;; ;;;   (seq (optimize-e e c #f t)
;;; ;;;        (optimize-op1 p (hash-ref t e))))

;;; ;;; ;; Op2 Expr Expr CEnv Table -> Asm
;;; ;;; (define (optimize-prim2 p e1 e2 c t)
;;; ;;;   (seq (optimize-e e1 c #f t)
;;; ;;;        (Push rax)
;;; ;;;        (optimize-e e2 (cons #f c) #f t)
;;; ;;;        (optimize-op2 p)))

;;; ;;; ;; Op3 Expr Expr Expr CEnv Table -> Asm
;;; ;;; (define (optimize-prim3 p e1 e2 e3 c t)
;;; ;;;   (seq (optimize-e e1 c #f t)
;;; ;;;        (Push rax)
;;; ;;;        (optimize-e e2 (cons #f c) #f t)
;;; ;;;        (Push rax)
;;; ;;;        (optimize-e e3 (cons #f (cons #f c)) #f t)
;;; ;;;        (optimize-op3 p)))

;;; ;;; ;; Expr Expr Expr CEnv Boolean Table -> Asm
;;; ;;; (define (optimize-if e1 e2 e3 c t? t)
;;; ;;;   (let ((l1 (gensym 'if))
;;; ;;;         (l2 (gensym 'if)))
;;; ;;;     (seq (optimize-e e1 c #f t)
;;; ;;;          (Cmp rax (value->bits #f))
;;; ;;;          (Je l1)
;;; ;;;          (optimize-e e2 c t? t)
;;; ;;;          (Jmp l2)
;;; ;;;          (Label l1)
;;; ;;;          (optimize-e e3 c t? t)
;;; ;;;          (Label l2))))

;;; ;;; ;; Expr Expr CEnv Boolean Table -> Asm

;;; ;;; ;; our attempts with begin
;;; ;;; ;;; (define (optimize-begin e1 e2 c t? t)
;;; ;;; ;;;   (begin 

;;; ;;; ;;;     ;;; (match minitable  
;;; ;;; ;;;     ;;;   []
;;; ;;; ;;;     ;;;   []
;;; ;;; ;;;     ;;;   [])
;;; ;;; ;;;     (print e1)
;;; ;;; ;;;   (if (ret-void? (hash-ref t e1)) ;; Idea: check if e1 returns void. if it does, keep it. if it doesn't, nuke it 
;;; ;;; ;;;   ;; bc only things that do side effects are relevant ...?
;;; ;;; ;;;   (seq (optimize-e e1 c #f t)
;;; ;;; ;;;        (optimize-e e2 c t? t)) ;; if it returned void, perform both actions
;;; ;;; ;;;   (optimize-e e2 c t? t))
;;; ;;; ;;;   )
;;; ;;; ;;;   ) ;; otherwise just do the second one 

;;; ;;; ;; Expr Expr CEnv Boolean Table -> Asm
;;; ;;;  (define (optimize-begin e1 e2 c t? t)
;;; ;;;    (seq (optimize-e e1 c #f t)
;;; ;;;    (optimize-e e2 c t? t)))


;;; ;;; ;; Id Expr Expr CEnv Boolean Table -> Asm
;;; ;;; (define (optimize-let x e1 e2 c t? t)
;;; ;;;   (seq (optimize-e e1 c #f t)
;;; ;;;        (Push rax)
;;; ;;;        (optimize-e e2 (cons x c) t? t)
;;; ;;;        (Add rsp 8)))

;;; ;;; ;; Id [Listof Expr] CEnv Table -> Asm
;;; ;;; ;; The return address is placed above the arguments, so callee pops
;;; ;;; ;; arguments and return address is next frame
;;; ;;; ;; Expr [Listof Expr] CEnv Boolean -> Asm
;;; ;;; (define (optimize-app e es c t? t)
;;; ;;;   (if t?
;;; ;;;       (optimize-app-tail e es c t)
;;; ;;;       (optimize-app-nontail e es c t)))

;;; ;;; ;; Expr [Listof Expr] CEnv Table -> Asm
;;; ;;; (define (optimize-app-tail e es c t)
;;; ;;;   (seq (optimize-es (cons e es) c t)
;;; ;;;        (move-args (add1 (length es)) (length c))
;;; ;;;        (Add rsp (* 8 (length c)))
;;; ;;;        (Mov rax (Offset rsp (* 8 (length es))))
;;; ;;;        (if (type-proc? (hash-ref t e))
;;; ;;;            (% "Skipped proc type check")
;;; ;;;            (assert-proc rax))

;;; ;;;        (% "Pass arity info")
;;; ;;;        (Mov r15 (length es))
       
;;; ;;;        (let ((l (known-lambda? (hash-ref t e))))
;;; ;;;          (if l
;;; ;;;              (seq (%% "Known lambda")
;;; ;;;                   (Jmp (symbol->label l)))
;;; ;;;              (seq (Mov rax (Offset rax (- type-proc)))
;;; ;;;                   (Jmp rax))))))

;;; ;;; ;; Integer Integer -> Asm
;;; ;;; (define (move-args i off)
;;; ;;;   (cond [(zero? off) (seq)]
;;; ;;;         [(zero? i)   (seq)]
;;; ;;;         [else
;;; ;;;          (seq (Mov r8 (Offset rsp (* 8 (sub1 i))))
;;; ;;;               (Mov (Offset rsp (* 8 (+ off (sub1 i)))) r8)
;;; ;;;               (move-args (sub1 i) off))]))

;;; ;;; ;; Expr [Listof Expr] CEnv Table -> Asm
;;; ;;; ;; The return address is placed above the arguments, so callee pops
;;; ;;; ;; arguments and return address is next frame
;;; ;;; (define (optimize-app-nontail e es c t)
;;; ;;;   (let ((r (gensym 'ret))
;;; ;;;         (i (* 8 (length es))))
;;; ;;;     (seq (Lea rax r)
;;; ;;;          (Push rax)
;;; ;;;          (optimize-es (cons e es) (cons #f c) t)
;;; ;;;          (Mov rax (Offset rsp i))
;;; ;;;          (if (type-proc? (hash-ref t e))
;;; ;;;              (%% "Skipped proc type check")
;;; ;;;              (assert-proc rax))

;;; ;;;          (% "Pass arity info")
;;; ;;;          (Mov r15 (length es))
       
;;; ;;;          (let ((l (known-lambda? (hash-ref t e))))
;;; ;;;            (if l
;;; ;;;                (seq (%% "Known lambda")
;;; ;;;                     (Jmp (symbol->label l)))
;;; ;;;                (seq (Mov rax (Offset rax (- type-proc)))
;;; ;;;                     (Jmp rax))))
         
;;; ;;;          (Label r))))

;;; ;;; ;; [Listof Lam] -> Asm
;;; ;;; ;; optimize the closures for ds and push them on the stack
;;; ;;; (define (optimize-defines-values ds)
;;; ;;;   (seq (alloc-defines ds 0)
;;; ;;;        (init-defines ds (reverse (define-ids ds)) 8)
;;; ;;;        (add-rbx-defines ds 0)))

;;; ;;; ;; [Listof Lam] Int -> Asm
;;; ;;; ;; Allocate closures for ds at given offset, but don't write environment yet
;;; ;;; (define (alloc-defines ds off)
;;; ;;;   (match ds
;;; ;;;     ['() (seq)]
;;; ;;;     [(cons (and (Lam f xs e) l) ds)
;;; ;;;      (let ((fvs (fv l)))
;;; ;;;        (seq (Lea rax (symbol->label f))
;;; ;;;             (Mov (Offset rbx off) rax)
;;; ;;;             (Mov rax rbx)
;;; ;;;             (Add rax off)
;;; ;;;             (Xor rax type-proc)
;;; ;;;             (Push rax)
;;; ;;;             (alloc-defines ds (+ off (* 8 (add1 (length fvs)))))))]))

;;; ;;; ;; [Listof Lam] CEnv Int -> Asm
;;; ;;; ;; Initialize the environment for each closure for ds at given offset
;;; ;;; (define (init-defines ds c off)
;;; ;;;   (match ds
;;; ;;;     ['() (seq)]
;;; ;;;     [(cons l ds)
;;; ;;;      (let ((fvs (fv l)))
;;; ;;;        (seq (free-vars-to-heap fvs c off)
;;; ;;;             (init-defines ds c (+ off (* 8 (add1 (length fvs)))))))]))

;;; ;;; ;; [Listof Lam] Int -> Asm
;;; ;;; ;; Compute adjustment to rbx for allocation of all ds
;;; ;;; (define (add-rbx-defines ds n)
;;; ;;;   (match ds
;;; ;;;     ['() (seq (Add rbx (* n 8)))]
;;; ;;;     [(cons l ds)
;;; ;;;      (add-rbx-defines ds (+ n (add1 (length (fv l)))))]))

;;; ;;; ;; Id [Listof Id] Expr CEnv -> Asm
;;; ;;; (define (optimize-lam f xs e c)
;;; ;;;   (let ((fvs (fv (Lam f xs e))))
;;; ;;;     (seq (Lea rax (symbol->label f))
;;; ;;;          (Mov (Offset rbx 0) rax)
;;; ;;;          (free-vars-to-heap fvs c 8)
;;; ;;;          (Mov rax rbx) ; return value
;;; ;;;          (Xor rax type-proc)
;;; ;;;          (Add rbx (* 8 (add1 (length fvs)))))))

;;; ;;; ;; [Listof Id] CEnv Int -> Asm
;;; ;;; ;; Copy the values of given free variables into the heap at given offset
;;; ;;; (define (free-vars-to-heap fvs c off)
;;; ;;;   (match fvs
;;; ;;;     ['() (seq)]
;;; ;;;     [(cons x fvs)
;;; ;;;      (seq (Mov r8 (Offset rsp (lookup x c)))
;;; ;;;           (Mov (Offset rbx off) r8)
;;; ;;;           (free-vars-to-heap fvs c (+ off 8)))]))

;;; ;;; ;; [Listof Expr] CEnv Table -> Asm
;;; ;;; (define (optimize-es es c t)
;;; ;;;   (match es
;;; ;;;     ['() '()]
;;; ;;;     [(cons e es)
;;; ;;;      (seq (optimize-e e c #f t)
;;; ;;;           (Push rax)
;;; ;;;           (optimize-es es (cons #f c) t))]))

;;; ;;; ;; Expr [Listof Pat] [Listof Expr] CEnv Bool -> Asm
;;; ;;; (define (optimize-match e ps es c t?)
;;; ;;;   (let ((done (gensym)))
;;; ;;;     (seq (optimize-e e c #f)
;;; ;;;          (Push rax) ; save away to be restored by each clause
;;; ;;;          (optimize-match-clauses ps es (cons #f c) done t?)
;;; ;;;          (Jmp 'err)
;;; ;;;          (Label done)
;;; ;;;          (Add rsp 8)))) ; pop the saved value being matched

;;; ;;; ;; [Listof Pat] [Listof Expr] CEnv Symbol Bool -> Asm
;;; ;;; (define (optimize-match-clauses ps es c done t?)
;;; ;;;   (match* (ps es)
;;; ;;;     [('() '()) (seq)]
;;; ;;;     [((cons p ps) (cons e es))
;;; ;;;      (seq (optimize-match-clause p e c done t?)
;;; ;;;           (optimize-match-clauses ps es c done t?))]))

;;; ;;; ;; Pat Expr CEnv Symbol Bool -> Asm
;;; ;;; (define (optimize-match-clause p e c done t?)
;;; ;;;   (let ((next (gensym)))
;;; ;;;     (match (optimize-pattern p '() next)
;;; ;;;       [(list i cm)
;;; ;;;        (seq (Mov rax (Offset rsp 0)) ; restore value being matched
;;; ;;;             i
;;; ;;;             (optimize-e e (append cm c) t?)
;;; ;;;             (Add rsp (* 8 (length cm)))
;;; ;;;             (Jmp done)
;;; ;;;             (Label next))])))

;;; ;;; ;; Pat CEnv Symbol -> (list Asm CEnv)
;;; ;;; (define (optimize-pattern p cm next)
;;; ;;;   (match p
;;; ;;;     [(Var '_)
;;; ;;;      (list (seq) cm)]
;;; ;;;     [(Var x)
;;; ;;;      (list (seq (Push rax)) (cons x cm))]
;;; ;;;     [(Lit l)
;;; ;;;      (let ((ok (gensym)))
;;; ;;;        (list (seq (Mov r8 rax)
;;; ;;;                   (optimize-value l)
;;; ;;;                   (Cmp rax r8)
;;; ;;;                   (Je ok)
;;; ;;;                   (Add rsp (* 8 (length cm)))
;;; ;;;                   (Jmp next)
;;; ;;;                   (Label ok))
;;; ;;;              cm))]
;;; ;;;     [(Conj p1 p2)
;;; ;;;      (match (optimize-pattern p1 (cons #f cm) next)
;;; ;;;        [(list i1 cm1)
;;; ;;;         (match (optimize-pattern p2 cm1 next)
;;; ;;;           [(list i2 cm2)
;;; ;;;            (list
;;; ;;;             (seq (Push rax)
;;; ;;;                  i1
;;; ;;;                  (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
;;; ;;;                  i2)
;;; ;;;             cm2)])])]
;;; ;;;     [(Box p)
;;; ;;;      (match (optimize-pattern p cm next)
;;; ;;;        [(list i1 cm1)
;;; ;;;         (let ((ok (gensym)))
;;; ;;;           (list
;;; ;;;            (seq (Mov r8 rax)
;;; ;;;                 (And r8 ptr-mask)
;;; ;;;                 (Cmp r8 type-box)
;;; ;;;                 (Je ok)
;;; ;;;                 (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
;;; ;;;                 (Jmp next)
;;; ;;;                 (Label ok)
;;; ;;;                 (Mov rax (Offset rax (- type-box)))
;;; ;;;                 i1)
;;; ;;;            cm1))])]
;;; ;;;     [(Cons p1 p2)
;;; ;;;      (match (optimize-pattern p1 (cons #f cm) next)
;;; ;;;        [(list i1 cm1)
;;; ;;;         (match (optimize-pattern p2 cm1 next)
;;; ;;;           [(list i2 cm2)
;;; ;;;            (let ((ok (gensym)))
;;; ;;;              (list
;;; ;;;               (seq (Mov r8 rax)
;;; ;;;                    (And r8 ptr-mask)
;;; ;;;                    (Cmp r8 type-cons)
;;; ;;;                    (Je ok)
;;; ;;;                    (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
;;; ;;;                    (Jmp next)
;;; ;;;                    (Label ok)
;;; ;;;                    (Xor rax type-cons)
;;; ;;;                    (Mov r8 (Offset rax 0))
;;; ;;;                    (Push r8)                ; push cdr
;;; ;;;                    (Mov rax (Offset rax 8)) ; mov rax car
;;; ;;;                    i1
;;; ;;;                    (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
;;; ;;;                    i2)
;;; ;;;               cm2))])])]))

;;; ;;; ;; Id CEnv -> Integer
;;; ;;; (define (lookup x cenv)
;;; ;;;   (match cenv
;;; ;;;     ['() (error "undefined variable:" x)]
;;; ;;;     [(cons y rest)
;;; ;;;      (match (eq? x y)
;;; ;;;        [#t 0]
;;; ;;;        [#f (+ 8 (lookup x rest))])]))



    