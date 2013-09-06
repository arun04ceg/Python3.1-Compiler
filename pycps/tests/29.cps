(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$c (void))
 (define g$a (void))
 (set-then!
  g$a
  1
  (set-then!
   g$b
   2
   (set-then!
    g$c
    3
    ((cps bitwise-and)
     g$a
     g$b
     g$c
     (lambda (rv16) ((cps py-print) rv16 $halt)))))))
