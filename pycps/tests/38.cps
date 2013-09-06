(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((lambda (x y k17) (k17 x))
  20
  30
  (lambda (rv16) ((cps py-print) rv16 $halt))))
