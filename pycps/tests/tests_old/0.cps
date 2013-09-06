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
     (lambda (return k17) (return x k17))
     k16))
  (g$f 3 (lambda (rv18) ((cps py-print) rv18 $halt)))))
