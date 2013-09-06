(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (set-then!
  g$f
  (lambda (x k16) (k16 x))
  (g$f 200 (lambda (rv17) ((cps py-print) rv17 $halt)))))
