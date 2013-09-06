(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$h (void))
 (define g$g (void))
 (define g$z (void))
 (set-then!
  g$g
  (lambda (x k16) (k16 (lambda (y k17) ((cps +) x y k17))))
  (g$g
   20
   (lambda (rv18)
     (set-then!
      g$h
      rv18
      (g$h
       30
       (lambda (rv19) (set-then! g$z rv19 ((cps py-print) g$z $halt)))))))))
