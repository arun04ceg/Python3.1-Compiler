(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 ((lambda (rv16)
    (set-then!
     g$a
     rv16
     ((lambda (b17 k18)
        ((lambda (i16 k19)
           ((lambda (k20)
              ((cps tuple?)
               b17
               (lambda (rv21)
                 (if rv21
                   ((cps tuple-set!) b17 i16 30 k20)
                   ((lambda (k22)
                      ((cps py-list?)
                       b17
                       (lambda (rv23)
                         (if rv23
                           ((cps py-list-set!) b17 i16 30 k22)
                           ((lambda (k24)
                              ((cps dict?)
                               b17
                               (lambda (rv25)
                                 (if rv25
                                   ((cps dict-set!) b17 i16 30 k24)
                                   (k24 (void))))))
                            k22)))))
                    k20)))))
            k19))
         1
         k18))
      g$a
      (lambda (rv17) ((cps py-print) g$a $halt)))))
  (py-list* 0 1 2 3)))
