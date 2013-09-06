(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$a (void))
 (define g$i (void))
 (define g$j (void))
 ((lambda (rv16)
    (set-then!
     g$a
     rv16
     ((lambda (rv17)
        (set-then!
         g$b
         rv17
         (set-then!
          g$i
          0
          (set-then!
           g$j
           1
           ((lambda (b17 k20)
              ((lambda (i16 k21)
                 ((lambda (k22)
                    ((cps tuple?)
                     b17
                     (lambda (rv23)
                       (if rv23
                         ((lambda (e19 k25)
                            ((lambda (i18 k26)
                               ((lambda (k27)
                                  ((cps py-list?)
                                   e19
                                   (lambda (rv28)
                                     (if rv28
                                       ((cps py-list-ref) e19 i18 k27)
                                       ((lambda (k29)
                                          ((cps tuple?)
                                           e19
                                           (lambda (rv30)
                                             (if rv30
                                               ((cps tuple-ref) e19 i18 k29)
                                               ((lambda (k31)
                                                  ((cps dict?)
                                                   e19
                                                   (lambda (rv32)
                                                     (if rv32
                                                       ((cps dict-ref)
                                                        e19
                                                        i18
                                                        k31)
                                                       (error
                                                        "cannot index object"
                                                        k31)))))
                                                k29)))))
                                        k27)))))
                                k26))
                             g$j
                             k25))
                          g$b
                          (lambda (rv24) ((cps tuple-set!) b17 i16 rv24 k22)))
                         ((lambda (k33)
                            ((cps py-list?)
                             b17
                             (lambda (rv34)
                               (if rv34
                                 ((lambda (e21 k36)
                                    ((lambda (i20 k37)
                                       ((lambda (k38)
                                          ((cps py-list?)
                                           e21
                                           (lambda (rv39)
                                             (if rv39
                                               ((cps py-list-ref) e21 i20 k38)
                                               ((lambda (k40)
                                                  ((cps tuple?)
                                                   e21
                                                   (lambda (rv41)
                                                     (if rv41
                                                       ((cps tuple-ref)
                                                        e21
                                                        i20
                                                        k40)
                                                       ((lambda (k42)
                                                          ((cps dict?)
                                                           e21
                                                           (lambda (rv43)
                                                             (if rv43
                                                               ((cps dict-ref)
                                                                e21
                                                                i20
                                                                k42)
                                                               (error
                                                                "cannot index object"
                                                                k42)))))
                                                        k40)))))
                                                k38)))))
                                        k37))
                                     g$j
                                     k36))
                                  g$b
                                  (lambda (rv35)
                                    ((cps py-list-set!) b17 i16 rv35 k33)))
                                 ((lambda (k44)
                                    ((cps dict?)
                                     b17
                                     (lambda (rv45)
                                       (if rv45
                                         ((lambda (e23 k47)
                                            ((lambda (i22 k48)
                                               ((lambda (k49)
                                                  ((cps py-list?)
                                                   e23
                                                   (lambda (rv50)
                                                     (if rv50
                                                       ((cps py-list-ref)
                                                        e23
                                                        i22
                                                        k49)
                                                       ((lambda (k51)
                                                          ((cps tuple?)
                                                           e23
                                                           (lambda (rv52)
                                                             (if rv52
                                                               ((cps tuple-ref)
                                                                e23
                                                                i22
                                                                k51)
                                                               ((lambda (k53)
                                                                  ((cps dict?)
                                                                   e23
                                                                   (lambda (rv54)
                                                                     (if rv54
                                                                       ((cps
                                                                         dict-ref)
                                                                        e23
                                                                        i22
                                                                        k53)
                                                                       (error
                                                                        "cannot index object"
                                                                        k53)))))
                                                                k51)))))
                                                        k49)))))
                                                k48))
                                             g$j
                                             k47))
                                          g$b
                                          (lambda (rv46)
                                            ((cps dict-set!)
                                             b17
                                             i16
                                             rv46
                                             k44)))
                                         (k44 (void))))))
                                  k33)))))
                          k22)))))
                  k21))
               g$i
               k20))
            g$a
            (lambda (rv18)
              ((cps py-print)
               g$a
               (lambda (rv19) ((cps py-print) g$b $halt)))))))))
      (py-list* 3 4))))
  (py-list* 1 2)))
