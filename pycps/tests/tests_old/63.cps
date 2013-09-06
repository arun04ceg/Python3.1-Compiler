(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$i (void))
 (set-then!
  g$i
  10
  ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
   (lambda (break k16)
     ((lambda (loop k17)
        (set-then!
         loop
         (lambda (k18)
           ((lambda (k19)
              ((cps >)
               g$i
               0
               (lambda (rv20)
                 (if rv20
                   ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                    (lambda (continue k22)
                      ((lambda (k23)
                         ((cps py-print)
                          g$i
                          (lambda (rv24)
                            ((cps -)
                             g$i
                             1
                             (lambda (rv25)
                               (set-then! g$i rv25 (k23 (void))))))))
                       k22))
                    (lambda (rv21) (loop k19)))
                   (k19 (void))))))
            k18))
         (loop (lambda (rv26) (k17 (void))))))
      (void)
      k16))
   $halt)))
