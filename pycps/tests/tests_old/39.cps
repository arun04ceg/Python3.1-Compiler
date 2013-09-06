(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (if #f
   ((lambda (k21) (k21 (void)))
    (lambda (rv16)
      ((cps py-print)
       g$x
       (lambda (rv17)
         (if #t
           ((lambda (k19) (k19 (void)))
            (lambda (rv18) ((cps py-print) g$x $halt)))
           ((lambda (k20) (set-then! g$x 10 (k20 (void))))
            (lambda (rv18) ((cps py-print) g$x $halt))))))))
   ((lambda (k22) (set-then! g$x 3 (k22 (void))))
    (lambda (rv16)
      ((cps py-print)
       g$x
       (lambda (rv17)
         (if #t
           ((lambda (k19) (k19 (void)))
            (lambda (rv18) ((cps py-print) g$x $halt)))
           ((lambda (k20) (set-then! g$x 10 (k20 (void))))
            (lambda (rv18) ((cps py-print) g$x $halt))))))))))
