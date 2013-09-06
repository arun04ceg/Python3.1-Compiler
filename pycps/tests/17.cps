(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (set-then!
  g$f
  (lambda (x y k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17) ((cps +) x y (lambda (rv18) (return rv18 k17))))
     k16))
  (g$f 10 20 (lambda (rv19) ((cps py-print) rv19 $halt)))))
