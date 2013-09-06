(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  200
  ((cps +)
   g$x
   20
   (lambda (rv16) (set-then! g$x rv16 ((cps py-print) g$x $halt))))))
