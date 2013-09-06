(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fib (void))
 (define g$cache (void))
 (set-then!
  g$cache
  (dict (0 0) (1 1))
  (set-then!
   g$fib
   (lambda (n k16)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k17)
        ((lambda (k18)
           ((cps in?)
            n
            g$cache
            (lambda (rv53)
              (if rv53
                ((lambda (k54)
                   ((lambda (e17 k56)
                      ((lambda (i16 k57)
                         ((lambda (k58)
                            ((cps py-list?)
                             e17
                             (lambda (rv59)
                               (if rv59
                                 ((cps py-list-ref) e17 i16 k58)
                                 ((lambda (k60)
                                    ((cps tuple?)
                                     e17
                                     (lambda (rv61)
                                       (if rv61
                                         ((cps tuple-ref) e17 i16 k60)
                                         ((lambda (k62)
                                            ((cps dict?)
                                             e17
                                             (lambda (rv63)
                                               (if rv63
                                                 ((cps dict-ref) e17 i16 k62)
                                                 (error
                                                  "cannot index object"
                                                  k62)))))
                                          k60)))))
                                  k58)))))
                          k57))
                       n
                       k56))
                    g$cache
                    (lambda (rv55) (return rv55 k54))))
                 (lambda (rv19)
                   ((lambda (b19 k30)
                      ((lambda (i18 k31)
                         ((lambda (k32)
                            ((cps tuple?)
                             b19
                             (lambda (rv33)
                               (if rv33
                                 ((cps -)
                                  n
                                  1
                                  (lambda (rv38)
                                    (g$fib
                                     rv38
                                     (lambda (rv35)
                                       ((cps -)
                                        n
                                        2
                                        (lambda (rv37)
                                          (g$fib
                                           rv37
                                           (lambda (rv36)
                                             ((cps +)
                                              rv35
                                              rv36
                                              (lambda (rv34)
                                                ((cps tuple-set!)
                                                 b19
                                                 i18
                                                 rv34
                                                 k32)))))))))))
                                 ((lambda (k39)
                                    ((cps py-list?)
                                     b19
                                     (lambda (rv40)
                                       (if rv40
                                         ((cps -)
                                          n
                                          1
                                          (lambda (rv45)
                                            (g$fib
                                             rv45
                                             (lambda (rv42)
                                               ((cps -)
                                                n
                                                2
                                                (lambda (rv44)
                                                  (g$fib
                                                   rv44
                                                   (lambda (rv43)
                                                     ((cps +)
                                                      rv42
                                                      rv43
                                                      (lambda (rv41)
                                                        ((cps py-list-set!)
                                                         b19
                                                         i18
                                                         rv41
                                                         k39)))))))))))
                                         ((lambda (k46)
                                            ((cps dict?)
                                             b19
                                             (lambda (rv47)
                                               (if rv47
                                                 ((cps -)
                                                  n
                                                  1
                                                  (lambda (rv52)
                                                    (g$fib
                                                     rv52
                                                     (lambda (rv49)
                                                       ((cps -)
                                                        n
                                                        2
                                                        (lambda (rv51)
                                                          (g$fib
                                                           rv51
                                                           (lambda (rv50)
                                                             ((cps +)
                                                              rv49
                                                              rv50
                                                              (lambda (rv48)
                                                                ((cps
                                                                  dict-set!)
                                                                 b19
                                                                 i18
                                                                 rv48
                                                                 k46)))))))))))
                                                 (k46 (void))))))
                                          k39)))))
                                  k32)))))
                          k31))
                       n
                       k30))
                    g$cache
                    (lambda (rv20)
                      ((lambda (e21 k22)
                         ((lambda (i20 k23)
                            ((lambda (k24)
                               ((cps py-list?)
                                e21
                                (lambda (rv25)
                                  (if rv25
                                    ((cps py-list-ref) e21 i20 k24)
                                    ((lambda (k26)
                                       ((cps tuple?)
                                        e21
                                        (lambda (rv27)
                                          (if rv27
                                            ((cps tuple-ref) e21 i20 k26)
                                            ((lambda (k28)
                                               ((cps dict?)
                                                e21
                                                (lambda (rv29)
                                                  (if rv29
                                                    ((cps dict-ref)
                                                     e21
                                                     i20
                                                     k28)
                                                    (error
                                                     "cannot index object"
                                                     k28)))))
                                             k26)))))
                                     k24)))))
                             k23))
                          n
                          k22))
                       g$cache
                       (lambda (rv21) (return rv21 k18)))))))
                ((lambda (rv19)
                   ((lambda (b19 k30)
                      ((lambda (i18 k31)
                         ((lambda (k32)
                            ((cps tuple?)
                             b19
                             (lambda (rv33)
                               (if rv33
                                 ((cps -)
                                  n
                                  1
                                  (lambda (rv38)
                                    (g$fib
                                     rv38
                                     (lambda (rv35)
                                       ((cps -)
                                        n
                                        2
                                        (lambda (rv37)
                                          (g$fib
                                           rv37
                                           (lambda (rv36)
                                             ((cps +)
                                              rv35
                                              rv36
                                              (lambda (rv34)
                                                ((cps tuple-set!)
                                                 b19
                                                 i18
                                                 rv34
                                                 k32)))))))))))
                                 ((lambda (k39)
                                    ((cps py-list?)
                                     b19
                                     (lambda (rv40)
                                       (if rv40
                                         ((cps -)
                                          n
                                          1
                                          (lambda (rv45)
                                            (g$fib
                                             rv45
                                             (lambda (rv42)
                                               ((cps -)
                                                n
                                                2
                                                (lambda (rv44)
                                                  (g$fib
                                                   rv44
                                                   (lambda (rv43)
                                                     ((cps +)
                                                      rv42
                                                      rv43
                                                      (lambda (rv41)
                                                        ((cps py-list-set!)
                                                         b19
                                                         i18
                                                         rv41
                                                         k39)))))))))))
                                         ((lambda (k46)
                                            ((cps dict?)
                                             b19
                                             (lambda (rv47)
                                               (if rv47
                                                 ((cps -)
                                                  n
                                                  1
                                                  (lambda (rv52)
                                                    (g$fib
                                                     rv52
                                                     (lambda (rv49)
                                                       ((cps -)
                                                        n
                                                        2
                                                        (lambda (rv51)
                                                          (g$fib
                                                           rv51
                                                           (lambda (rv50)
                                                             ((cps +)
                                                              rv49
                                                              rv50
                                                              (lambda (rv48)
                                                                ((cps
                                                                  dict-set!)
                                                                 b19
                                                                 i18
                                                                 rv48
                                                                 k46)))))))))))
                                                 (k46 (void))))))
                                          k39)))))
                                  k32)))))
                          k31))
                       n
                       k30))
                    g$cache
                    (lambda (rv20)
                      ((lambda (e21 k22)
                         ((lambda (i20 k23)
                            ((lambda (k24)
                               ((cps py-list?)
                                e21
                                (lambda (rv25)
                                  (if rv25
                                    ((cps py-list-ref) e21 i20 k24)
                                    ((lambda (k26)
                                       ((cps tuple?)
                                        e21
                                        (lambda (rv27)
                                          (if rv27
                                            ((cps tuple-ref) e21 i20 k26)
                                            ((lambda (k28)
                                               ((cps dict?)
                                                e21
                                                (lambda (rv29)
                                                  (if rv29
                                                    ((cps dict-ref)
                                                     e21
                                                     i20
                                                     k28)
                                                    (error
                                                     "cannot index object"
                                                     k28)))))
                                             k26)))))
                                     k24)))))
                             k23))
                          n
                          k22))
                       g$cache
                       (lambda (rv21) (return rv21 k18))))))
                 (void))))))
         k17))
      k16))
   (g$fib 25 (lambda (rv64) ((cps py-print) rv64 $halt))))))
