(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (set-then!
  g$f
  (lambda (x k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17) ((lambda (k18) (return (void) k18)) k17))
     k16))
  (g$f 30 (lambda (rv19) (g$f 40 (lambda (rv20) ((cps py-print) 1 $halt)))))))
