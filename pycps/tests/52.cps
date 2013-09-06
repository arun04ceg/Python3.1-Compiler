(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  3
  ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
   (lambda (break k17)
     ((lambda (loop k18)
        (set-then!
         loop
         (lambda (k19)
           ((lambda (k20)
              (if g$x
                ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                 (lambda (continue k22)
                   ((lambda (k23)
                      ((cps -)
                       g$x
                       1
                       (lambda (rv24)
                         (set-then!
                          g$x
                          rv24
                          ((cps py-print)
                           g$x
                           (lambda (rv25) (break (void) k23)))))))
                    k22))
                 (lambda (rv21) (loop k20)))
                (k20 (void))))
            k19))
         (loop (lambda (rv26) (k18 (void))))))
      (void)
      k17))
   (lambda (rv16) ((cps py-print) 42 $halt)))))
