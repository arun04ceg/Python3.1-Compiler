(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 ((lambda (rv45)
    ((lambda (rv44)
       ((lambda (rv16)
          (set-then!
           g$x
           rv16
           ((lambda (e17 k36)
              ((lambda (i16 k37)
                 ((lambda (k38)
                    ((cps py-list?)
                     e17
                     (lambda (rv39)
                       (if rv39
                         ((cps py-list-ref) e17 i16 k38)
                         ((lambda (k40)
                            ((cps tuple?)
                             e17
                             (lambda (rv41)
                               (if rv41
                                 ((cps tuple-ref) e17 i16 k40)
                                 ((lambda (k42)
                                    ((cps dict?)
                                     e17
                                     (lambda (rv43)
                                       (if rv43
                                         ((cps dict-ref) e17 i16 k42)
                                         (error "cannot index object" k42)))))
                                  k40)))))
                          k38)))))
                  k37))
               0
               k36))
            g$x
            (lambda (rv35)
              ((lambda (e19 k27)
                 ((lambda (i18 k28)
                    ((lambda (k29)
                       ((cps py-list?)
                        e19
                        (lambda (rv30)
                          (if rv30
                            ((cps py-list-ref) e19 i18 k29)
                            ((lambda (k31)
                               ((cps tuple?)
                                e19
                                (lambda (rv32)
                                  (if rv32
                                    ((cps tuple-ref) e19 i18 k31)
                                    ((lambda (k33)
                                       ((cps dict?)
                                        e19
                                        (lambda (rv34)
                                          (if rv34
                                            ((cps dict-ref) e19 i18 k33)
                                            (error
                                             "cannot index object"
                                             k33)))))
                                     k31)))))
                             k29)))))
                     k28))
                  0
                  k27))
               rv35
               (lambda (rv26)
                 ((lambda (e21 k18)
                    ((lambda (i20 k19)
                       ((lambda (k20)
                          ((cps py-list?)
                           e21
                           (lambda (rv21)
                             (if rv21
                               ((cps py-list-ref) e21 i20 k20)
                               ((lambda (k22)
                                  ((cps tuple?)
                                   e21
                                   (lambda (rv23)
                                     (if rv23
                                       ((cps tuple-ref) e21 i20 k22)
                                       ((lambda (k24)
                                          ((cps dict?)
                                           e21
                                           (lambda (rv25)
                                             (if rv25
                                               ((cps dict-ref) e21 i20 k24)
                                               (error
                                                "cannot index object"
                                                k24)))))
                                        k22)))))
                                k20)))))
                        k19))
                     0
                     k18))
                  rv26
                  (lambda (rv17) ((cps py-print) rv17 $halt)))))))))
        (py-list* rv44)))
     (py-list* rv45)))
  (py-list* 5)))
