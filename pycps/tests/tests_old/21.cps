(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((cps >>)
  3
  2
  (lambda (rv18)
    ((cps py-print)
     rv18
     (lambda (rv16)
       ((cps <<) 1 2 (lambda (rv17) ((cps py-print) rv17 $halt))))))))
