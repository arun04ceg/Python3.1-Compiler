(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  #t
  ((lambda (k16)
     ((cps not)
      g$x
      (lambda (rv17)
        (if rv17
          ((lambda (k18)
             ((cps not) g$x (lambda (rv19) ((cps py-print) rv19 k18))))
           k16)
          ((lambda (k20) ((cps py-print) 20 k20)) k16)))))
   $halt)))
