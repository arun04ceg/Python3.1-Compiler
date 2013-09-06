(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((cps *)
  3
  8
  (lambda (rv17)
    ((cps +) 5 rv17 (lambda (rv16) ((cps py-print) rv16 $halt))))))
