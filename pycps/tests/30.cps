(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (define g$y (void))
 (define g$z (void))
 (set-then!
  g$x
  10
  (set-then!
   g$y
   20
   (set-then!
    g$z
    30
    ((cps bitwise-or)
     g$x
     g$y
     g$z
     (lambda (rv16) ((cps py-print) rv16 $halt)))))))
