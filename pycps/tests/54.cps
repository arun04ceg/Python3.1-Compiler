(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$y (void))
 (set-then!
  g$y
  100
  (set-then!
   g$f
   (lambda (x k16)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k17)
        ((lambda (y k18) (set-then! y 10 (k18 (void)))) (void) k17))
      k16))
   (g$f 300 (lambda (rv19) ((cps py-print) g$y $halt))))))
