(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$y (void))
 (define g$x (void))
 (set-then!
  g$x
  3
  (set-then!
   g$y
   10
   ((cps bitwise-xor) g$x g$y (lambda (rv16) ((cps py-print) rv16 $halt))))))
