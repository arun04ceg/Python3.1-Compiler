(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  #f
  (if g$x
    ((lambda (rv16) ((cps py-print) rv16 $halt)) 10)
    ((lambda (rv16) ((cps py-print) rv16 $halt)) 20))))
