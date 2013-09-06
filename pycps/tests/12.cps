(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$a (void))
 (set-then!
  g$a
  (dict (1 2) (3 4))
  ((lambda (rv16)
     (set-then!
      g$b
      rv16
      ((lambda (b17 k28)
         ((lambda (i16 k29)
            ((lambda (k30)
               ((cps tuple?)
                b17
                (lambda (rv31)
                  (if rv31
                    (error "Cannot delete from tuples!" k30)
                    ((lambda (k32)
                       ((cps py-list?)
                        b17
                        (lambda (rv33)
                          (if rv33
                            ((cps py-list-remove!) b17 i16 k32)
                            ((lambda (k34)
                               ((cps dict?)
                                b17
                                (lambda (rv35)
                                  (if rv35
                                    ((cps dict-remove!) b17 i16 k34)
                                    (k34 (void))))))
                             k32)))))
                     k30)))))
             k29))
          1
          k28))
       g$a
       (lambda (rv17)
         ((lambda (b19 k20)
            ((lambda (i18 k21)
               ((lambda (k22)
                  ((cps tuple?)
                   b19
                   (lambda (rv23)
                     (if rv23
                       (error "Cannot delete from tuples!" k22)
                       ((lambda (k24)
                          ((cps py-list?)
                           b19
                           (lambda (rv25)
                             (if rv25
                               ((cps py-list-remove!) b19 i18 k24)
                               ((lambda (k26)
                                  ((cps dict?)
                                   b19
                                   (lambda (rv27)
                                     (if rv27
                                       ((cps dict-remove!) b19 i18 k26)
                                       (k26 (void))))))
                                k24)))))
                        k22)))))
                k21))
             2
             k20))
          g$b
          (lambda (rv18)
            ((cps py-print)
             g$a
             (lambda (rv19) ((cps py-print) g$b $halt)))))))))
   (py-list* 0 1 2))))
