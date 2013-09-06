(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((cps expt) 3 4 (lambda (rv16) ((cps py-print) rv16 $halt))))
