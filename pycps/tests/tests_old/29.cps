(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((cps +)
  4
  2
  (lambda (rv19)
    ((cps expt)
     rv19
     2
     (lambda (rv18)
       ((cps *)
        2
        rv18
        (lambda (rv17)
          ((cps +) 3 rv17 (lambda (rv16) ((cps py-print) rv16 $halt))))))))))
