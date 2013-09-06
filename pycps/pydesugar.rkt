;
;This project is done by Arun Baskar(u0811338) and Arvind Haran(u0809850).
;
;


#lang racket

;; Helpers.

; partition-k : ('a -> boolean) 'a list ('a list 'a list -> 'a list 'a list)
(define (partition-k pred lst k)
  (if (not (pair? lst))
      (k '() '())
      (partition-k pred (cdr lst) (λ (in out)
        (if (pred (car lst))
            (k (cons (car lst) in) out)
            (k in (cons (car lst) out)))))))

; define? : term -> boolean
(define (define? sx)
  (match sx
    [`(define . ,_) #t]
    [else           #f]))

; not-define? : term -> boolean
(define (not-define? sx)
  (not (define? sx)))

; atomic? : term -> boolean
(define (atomic? exp)
  (match exp
    [`(,(or 'lambda 'λ) . ,_)     #t]
    [(? number?)   #t]
    [(? string?)   #t]
    [(? boolean?)  #t]
    [`(quote . ,_) #t]
    ['(void)       #t]
    ['None         #t]
    ['Ellipsis     #t]
    [else          #f]))

; atomic-define? : term -> boolean
(define (atomic-define? def)
  (match def
    [`(define ,v ,exp)  (atomic? exp)]
    [else               #f]))

; global-name : symbol -> symbol
(define (global-name name)
  (string->symbol (string-append "g$" (symbol->string name))))

; atomize-tops : top list -> top list
(define (atomize-tops tops)
  (match tops
    ['()  #;=>  '()]
    
    [(cons (and head (? atomic-define?)) tail)
     (cons head (atomize-tops tail))]
    
    [(cons `(define ,v ,exp) tail)
     `((define ,v (void))
       (set! ,v ,exp)
       ,@(atomize-tops tail))]
    
    [(cons head tail)
     (cons head (atomize-tops tail))]))

;; Desugaring.

; desugar-top : top -> top
(define (desugar-top top)
  (match top
    [`(define ,v ,exp)
     `(define ,(global-name v) ,(desugar-exp exp))]
    
    [exp
     (desugar-exp exp)]))

     
; desugar : program -> program
(define (desugar-program program)
  
  (define prog (match program [`(program . ,stmts) stmts]))
  
  (set! prog (atomize-tops prog))
  
  (set! prog (map desugar-top prog))
  
  (set! prog (append (list
                      '(define break (void))
                      '(define return (void))
                      '(define continue (void))
                      '(define $current-handler (void)))
                     prog))
  
  (set! prog
    (partition-k 
     atomic-define?
     prog
     (λ (atomic complex)
       (append atomic `((begin ,@complex))))))
  
  `(program ,@prog))

;desugar-body
(define (desugar-body body)
  (match body
    [`(,exp)
     (cons (desugar-exp exp) '())]
    
    [`(,(and (? not-define?) exps) ...)
     (map desugar-exp exps)]
    
    [`(,tops ... ,exp)
     (define defs (tops-to-defs tops))
     (desugar-exp (match defs
                    [`((define ,vs ,es) ...)
                     `(letrec ,(map list vs es) ,exp)]))]))

; tops-to-defs : top list -> def list
(define (tops-to-defs tops)
  
  (define (top-to-def top)
    (match top
      [`(define (,f ,params ...) . ,body) 
       `(define ,f (λ ,params . ,body))]
    
      [`(define ,v ,exp)
       `(define ,v ,exp)]
    
      [exp
       `(define ,(gensym '_) ,exp)]))
  
  (map top-to-def tops))


; desugar-exp : exp -> exp
(define (desugar-exp exp)
  (match exp
    ['() '()]
    [(? symbol?)      exp]
    [`(quote ,_)      (error "quotes not allowed in hir")]

    [`(letrec ((,vs ,es) ...) . ,body)
      (desugar-exp
      `(let ,(for/list ([v vs])
               (list v '(void)))
         ,@(map (λ (v e)
                  `(set! ,v ,e))
                vs es)
         ,@body))]

    [`(let ((,vs ,es) ...) . ,body)
     `((lambda ,vs (begin ,@(desugar-body body))) 
       ,@(map desugar-exp es))]
   
    [`(let* () ,body)
     (error "haven't handled let*")]
    
    [`(let* ((,v ,e) . ,rest) ,body)
     (error "haven't handled let*")]
    
    [`(,(or 'lambda 'λ) ,params ,body)
     `(lambda ,params ,(desugar-exp body))]

    [`(call/ec ,exp)
     `(call/ec ,(desugar-exp exp))]
    
    [`(cond)
     (void)]
    
    [`(cond (else ,exp))
     (desugar-exp exp)]
    
    [`(cond (,test ,exp))
     `(if ,(desugar-exp test) ,(desugar-exp exp) (void))]
     
    [`(cond (,test ,exp) ,rest ...)
     `(if ,(desugar-exp test) ,(desugar-exp exp) ,(desugar-exp `(cond . ,rest)))]
     
    [`(and)   #t]
    [`(or)    #f]
    
    [`(or ,exp)
     (desugar-exp exp)]
    
    [`(and ,exp)
     (desugar-exp exp)]
    
    [`(or ,exp . ,rest)
     (define $t (gensym 't))
     (desugar-exp `(let ((,$t ,exp))
             (if ,$t ,$t (or . ,rest))))]
     
    [`(and ,exp . ,rest)
     `(if ,(desugar-exp exp) ,(desugar-exp `(and . ,rest)) #f)]
     
    [`(if ,test ,exp)
      `(if ,(desugar-exp test) ,(desugar-exp exp) (void))]
    
    [`(if ,test ,exp1 ,exp2)
     `(if ,(desugar-exp test) 
          ,(desugar-exp exp1) 
          ,(desugar-exp exp2))]
    
    [`(set! ,v ,exp)
      `(set! ,(desugar-exp v) ,(desugar-exp exp))]

    [`(assert ,test)
      `(assert1 ,(desugar-exp `(lambda () ,test)))]
    
    [`(assert ,test ,kind)
     `(assert2 ,(desugar-exp `(lambda () ,test)) ,(desugar-exp `(lambda () ,kind)))]
    
    [`(get-global ,var)
      (global-name (desugar-exp var))]
    
    [`(set-global! ,var ,exp)
      `(set! ,(global-name var) ,(desugar-exp exp))]
    
    [`(begin . ,exps)
      `(begin ,@(map desugar-exp exps))]
    
    ['(return)
      (desugar-body (list 'return '(void)))]
    
    ['(break)
      (desugar-body (list 'break '(void)))]
    
    ['(continue)
      (desugar-body (list 'continue '(void)))]

    [`(while ,cond ,body)
      `(call/ec
         (lambda (break)
            ((lambda (loop)
               (begin
                  (set! loop
                     (lambda ()
                     (if ,(desugar-exp cond)
                         (begin
                            (call/ec
                              (lambda (continue) ,(desugar-exp body)))
                   (loop))
                 (void))))
           (loop)
           (void)))
       (void))))]
    
    [`(while ,cond ,body ,else)
      `(call/ec
         (lambda (break)
            ((lambda (loop)
               (begin
                  (set! loop
                     (lambda ()
                     (if ,(desugar-exp cond)
                         (begin
                            (call/ec
                              (lambda (continue) ,(desugar-exp body)))
                   (loop))
                 (void))))
           (loop)
           ,(desugar-exp else)))
       (void))))]
     
    [`(for-each ,var ,seq ,body ,else)
        `(call/ec
          (lambda (break)
            ((lambda ($seq16 $loop17)
              (begin
                (begin
                  (if (set? $seq16)
                    (for-set $seq16 $loop17)
                      (if (tuple? $seq16)
                         (for-tuple $seq16 $loop17)
                            (if (py-list? $seq16)
                               (for-py-list $seq16 $loop17)
                                  (if (dict? $seq16) (for-dict $seq16 $loop17) (void)))))
             ,(desugar-exp else))))
             ,(desugar-exp seq)
               (lambda (,var)
                 (call/ec
                   (lambda (continue)
             ,(desugar-exp body)))))))]
    
    [`(for-each ,var ,seq ,body)
        `(call/ec
          (lambda (break)
            ((lambda ($seq16 $loop17)
              (begin
                (begin
                  (if (set? $seq16)
                    (for-set $seq16 $loop17)
                      (if (tuple? $seq16)
                         (for-tuple $seq16 $loop17)
                            (if (py-list? $seq16)
                               (for-py-list $seq16 $loop17)
                                  (if (dict? $seq16) (for-dict $seq16 $loop17) (void)))))
             (void))))
             ,(desugar-exp seq)
               (lambda (,var)
                 (call/ec
                   (lambda (continue)
             ,(desugar-exp body)))))))]
    
    [`(dict (,keys ,values) ...)
      `(dict ,@(map (lambda (a b) (list (desugar-exp a)  (desugar-exp b))) keys values))]
    
    [`(set . ,values)
     `(set ,@(map desugar-exp values))]
    
    [`(tuple . ,values)
     `(tuple ,@(map desugar-exp values))]
    
    [`(py-list* . ,values)
     `(py-list* ,@(map desugar-exp values))]
    
    [`(try ,body ,handler)
     `((lambda ($old-handler)
      (begin
        ((lambda ($old-return)
           (begin
             ((lambda ($old-continue)
                (begin
                  ((lambda ($old-break)
                     (begin
                       ((lambda (return)
                          (begin
                            ((lambda (continue)
                               (begin
                                 ((lambda (break)
                                    (begin
                                      (call/ec
                                       (lambda ($ec17)
                                         (begin
                                           (set! $current-handler
                                             (lambda ($ex16)
                                               (begin
                                                 (set! $current-handler
                                                   $old-handler)
                                                 ($ec17
                                                  (,(desugar-exp handler)
                                                   $ex16)))))
                                           ((lambda (rv)
                                              (begin
                                                (begin
                                                  (set! $current-handler
                                                    $old-handler)
                                                  rv)))
                                            ,(desugar-exp body)))))))
                                  (lambda ()
                                    (begin
                                      (set! $current-handler $old-handler)
                                      ($old-break (void)))))))
                             (lambda ()
                               (begin
                                 (set! $current-handler $old-handler)
                                 ($old-continue (void)))))))
                        (lambda (rv)
                          (begin
                            (set! $current-handler $old-handler)
                            (return rv))))))
                   break)))
              continue)))
         return)))
    $current-handler)]
     
    [`(throw ,exp)
     `($current-handler ,(desugar-exp exp))]
    
    [(? atomic?)      
     `(,@exp)]

    [`(,f . ,args)  
     `(,(desugar-exp f) ,@(map desugar-exp args))]
            
    [else 
     (error (format "desugar fail: ~s~n" exp))]))




(pretty-write (desugar-program (read)))


;
;This project is done by Arun Baskar(u0811338) and Arvind Haran(u0809850).
;
; 
