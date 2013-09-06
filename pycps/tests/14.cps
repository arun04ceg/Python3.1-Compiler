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
     ((lambda (e17 k18)
        ((lambda (i16 k19)
           ((lambda (k20)
              ((cps py-list?)
               e17
               (lambda (rv21)
                 (if rv21
                   ((cps py-list-ref) e17 i16 k20)
                   ((lambda (k22)
                      ((cps tuple?)
                       e17
                       (lambda (rv23)
                         (if rv23
                           ((cps tuple-ref) e17 i16 k22)
                           ((lambda (k24)
                              ((cps dict?)
                               e17
                               (lambda (rv25)
                                 (if rv25
                                   ((cps dict-ref) e17 i16 k24)
                                   (error "cannot index object" k24)))))
                            k22)))))
                    k20)))))
            k19))
         1
         k18))
      g$a
      (lambda (rv17) ((cps py-print) rv17 $halt)))))
  (py-list* 5 4 3)))
