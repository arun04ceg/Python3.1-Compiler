(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$g (void))
 (define g$x (void))
 (set-then!
  g$x
  10
  (set-then!
   g$g
   (lambda (y k16)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k17) ((lambda (k18) (return g$x k18)) k17))
      k16))
   (g$g 20 (lambda (rv19) ((cps py-print) rv19 $halt))))))
