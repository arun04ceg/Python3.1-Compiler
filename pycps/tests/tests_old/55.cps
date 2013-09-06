(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (set-then!
  g$b
  (lambda (k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17)
       ((lambda (k18) ((cps py-print) "Passing" (lambda (rv19) (k18 (void)))))
        k17))
     k16))
  (g$b $halt)))
