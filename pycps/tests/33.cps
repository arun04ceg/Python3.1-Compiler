(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$y (void))
 (define g$a (void))
 (set-then!
  g$a
  30
  (set-then!
   g$y
   40
   (if g$a
     ((lambda (rv16) ((cps py-print) rv16 $halt)) g$y)
     ((lambda (rv16) ((cps py-print) rv16 $halt)) #f)))))
