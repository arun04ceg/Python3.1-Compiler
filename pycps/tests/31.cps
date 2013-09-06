(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$y (void))
 (define g$x (void))
 (set-then!
  g$x
  10
  (set-then!
   g$y
   30
   ((cps <)
    g$x
    g$y
    (lambda (rv17)
      (if rv17
        ((lambda (rv16) ((cps py-print) rv16 $halt)) 1)
        ((lambda (rv16) ((cps py-print) rv16 $halt)) 20)))))))
