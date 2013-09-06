;
;This project is done by Arun Baskar(u0811338) and Arvind Haran(u0809850).
;
;

#lang racket

(define (unop? op)
   (match op
   [(or 
   `bitwise-not
   `+
   `-
   (? number?)
   (? string?)
   `tuple?
   `dict?
   `py-list?
   `set?
   `assert1
   `py-print
   `not
   ) #t]
   [else #f]
   )
)

(define (binop? op)
    (match op
    [(or
    `<
    `>
    `equal?
    `>=
    `<=
    `not-equal?
    `in?
    `not-in?
    `eq?
    `not-eq?
    `<<
    `>>
    `+
    `-
    `*
    `/
    `quotient
    `modulo
    `expt
    `assert2
    `bitwise-and
    `bitwise-or
    `bitwise-xor
    `py-list-ref
    `py-list-remove!
    `tuple-ref
    `tuple-set!
    `dict-ref
    `dict-remove!
    `py-list-set!
    `dict-set!
    ) #t]
    [else #f]
    )
)

(define (lib-fun? op)
    (match op
    [`for-set #t]
    [`for-py-list #t]
    [`for-tuple #t]
    [`for-dict #t]
    [else #f]
    )
)

(define (translate-binop op)
    (match op
       [`for-set `for-set-k]
       [`for-py-list `for-py-list-k]
       [`for-tuple `for-tuple-k]
       [`for-dict `for-dict-k]
       [else op]
    )
)

(define prims (apply set '( + - / *  = )))

(define (aexpr? expr)
  (match expr
    [(or `(lambda (,_ ...) ,_)
         (? symbol?)
         (? number?)
         (? string?)
         (? boolean?)
         '(void)
         '$halt
         'Ellipsis
         ;`(set ,_ ...)
         ;`(tuple ,_ ...)
         ;`(py-list* ,_ ...)
         `(dict (,_ ,_) ...))
     ; =>
     #t]
    
    [else #f]))

(define (prim? term)
  (set-member? prims term))
         

(define (T-k expr k)
  (match expr
    [ (? aexpr?)   (k (M expr))]

    [`(begin ,expr)
      (T-k expr k)]
    
    [`(begin ,expr ,exprs ...)
      (T-k expr (lambda (_)
                  (T-k `(begin ,@exprs) k)))]
   
    [`(if ,exprc ,exprt ,exprf)
      (define rv (gensym 'rv))
      (define cont `(lambda (,rv) ,(k rv)))
      (T-k exprc (lambda (aexp)
           `(if ,aexp 
                ,(T-c exprt cont)
                ,(T-c exprf cont))))]
 
    [`(set! ,var ,expr)
      (T-k expr (lambda (aexp)
                  `(set-then! ,var ,aexp
                              ,(k '(void)))))]

    [`(,_ ,_ ...)
      ; =>
      (define rv (gensym 'rv))
      (define cont `(lambda (,rv) ,(k rv)))
      (T-c expr cont)]))


(define (T-c expr c)
  (match expr
    [ (? aexpr?)         `(,c ,(M expr))]
    
    [`(begin ,expr)      (T-c expr c)]
    
    [`(begin ,expr ,exprs ...)
      (T-k expr (lambda (_)
                  (T-c `(begin ,@exprs) c)))]
   
    [`(if ,exprc ,exprt ,exprf)
      (define k (gensym 'k))
      `((lambda (,k)
          ,(T-k exprc (λ (aexp)
                        `(if ,aexp 
                             ,(T-c exprt k)
                             ,(T-c exprf k)))))
        ,c)]
    
    [`(set! ,var ,expr)
      (T-k expr (lambda (aexp)
                  `(set-then! ,var ,aexp
                              (,c (void)))))]

    [`(,(and p (? lib-fun?)) ,es ...)
      ; =>
     (T*-k es (lambda (es)
                `(,(translate-binop p) ,@es ,c)))]
   
    [`(,(and p (? binop?)) ,es ...)
      ; =>
     (T*-k es (lambda (es)
                `((cps ,p) ,@es ,c)))]
 
    [`(,(and p (? unop?)) ,es ...)
      ; =>
     (T*-k es (lambda (es)
                `((cps ,p) ,@es ,c)))]


    [`(set ,es ...)
      ; =>
      (T-k `set (lambda (f)
             (T*-k es (lambda (es)
                      `(,c (set ,@es))))))]

    [`(py-list* ,es ...)
      ; =>
      (T-k `py-list* (lambda (f)
             (T*-k es (lambda (es)
                      `(,c (py-list* ,@es))))))]

    [`(tuple ,es ...)
      ; =>
      (T-k `tuple (lambda (f)
             (T*-k es (lambda (es)
                      `(,c (tuple ,@es))))))]

    [`(dict ,es ...)
      ; =>
      (T-k `dict (lambda (f)
             (T*-k es (lambda (es)
                      `(,c (dict ,@es))))))]

    [`(,f ,es ...)    
      ; =>
      (T-k f (lambda (f)
             (T*-k es (lambda (es)
                      `(,f ,@es ,c)))))]))

(define (T*-k exprs k)
  (cond
    [(null? exprs)   (k '())]
    [(pair? exprs)   (T-k (car exprs) (lambda (hd)
                       (T*-k (cdr exprs) (lambda (tl)
                         (k (cons hd tl))))))]))
     
(define (M aexpr)
  (match aexpr
    [`(lambda (,vars ...) ,body)
      ; =>
      (define k (gensym 'k))
     `(lambda (,@vars ,k) 
        ,(T-c body k))]

    [(or 'call/ec 'call/cc)
     '(lambda (f cc) (f (lambda (x k) (cc x)) cc))]
    
    [(? aexpr?) aexpr]
    
    [else 
     (error "Not an aexpr!")]))

(define (process_define define_stmt) define_stmt)

(define (cps-transform-program program)
 (match program
   [`(program ,define_stmt ... ,aexp) `(program ,@(process_define define_stmt) ,(T-c aexp '$halt))]
 )
)

(pretty-write (cps-transform-program (read)))

;
;This project is done by Arun Baskar(u0811338) and Arvind Haran(u0809850).
;
;
