(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$a (void))
 (set-then!
  g$a
  20
  (set-then!
   g$b
   40
   ((lambda (t16 k17) ((lambda (k18) (if t16 (k18 t16) (k18 g$b))) k17))
    g$a
    (lambda (rv16) ((cps py-print) rv16 $halt))))))
