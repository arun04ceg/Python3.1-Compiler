(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  20
  ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
   (lambda (break k16)
     ((lambda (loop k17)
        (set-then!
         loop
         (lambda (k18)
           ((lambda (k19)
              ((cps >)
               g$x
               0
               (lambda (rv20)
                 (if rv20
                   ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                    (lambda (continue k22)
                      ((lambda (k23)
                         ((cps -)
                          g$x
                          1
                          (lambda (rv24)
                            (set-then! g$x rv24 ((cps py-print) g$x k23)))))
                       k22))
                    (lambda (rv21) (loop k19)))
                   (k19 (void))))))
            k18))
         (loop
          (lambda (rv25)
            ((lambda (k26) ((cps py-print) "didn't run\n" k26)) k17)))))
      (void)
      k16))
   $halt)))
