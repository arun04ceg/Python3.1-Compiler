(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (set-then!
  g$a
  (dict (1 2) (3 4))
  ((lambda (e17 k27)
     ((lambda (i16 k28)
        ((lambda (k29)
           ((cps py-list?)
            e17
            (lambda (rv30)
              (if rv30
                ((cps py-list-ref) e17 i16 k29)
                ((lambda (k31)
                   ((cps tuple?)
                    e17
                    (lambda (rv32)
                      (if rv32
                        ((cps tuple-ref) e17 i16 k31)
                        ((lambda (k33)
                           ((cps dict?)
                            e17
                            (lambda (rv34)
                              (if rv34
                                ((cps dict-ref) e17 i16 k33)
                                (error "cannot index object" k33)))))
                         k31)))))
                 k29)))))
         k28))
      1
      k27))
   g$a
   (lambda (rv26)
     ((cps py-print)
      rv26
      (lambda (rv16)
        ((lambda (e19 k18)
           ((lambda (i18 k19)
              ((lambda (k20)
                 ((cps py-list?)
                  e19
                  (lambda (rv21)
                    (if rv21
                      ((cps py-list-ref) e19 i18 k20)
                      ((lambda (k22)
                         ((cps tuple?)
                          e19
                          (lambda (rv23)
                            (if rv23
                              ((cps tuple-ref) e19 i18 k22)
                              ((lambda (k24)
                                 ((cps dict?)
                                  e19
                                  (lambda (rv25)
                                    (if rv25
                                      ((cps dict-ref) e19 i18 k24)
                                      (error "cannot index object" k24)))))
                               k22)))))
                       k20)))))
               k19))
            3
            k18))
         g$a
         (lambda (rv17) ((cps py-print) rv17 $halt)))))))))
