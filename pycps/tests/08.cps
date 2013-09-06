(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (define g$x (void))
 (define g$y (void))
 (set-then!
  g$a
  10
  (set-then!
   g$x
   42
   (set-then!
    g$y
    30
    ((cps py-print)
     g$a
     (lambda (rv16)
       ((cps py-print) g$x (lambda (rv17) ((cps py-print) g$y $halt)))))))))
