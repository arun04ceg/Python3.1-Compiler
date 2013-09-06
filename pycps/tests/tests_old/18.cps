(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$a (void))
 ((lambda (rv37)
    ((lambda (t16 k18)
       ((lambda (e18 k29)
          ((lambda (i17 k30)
             ((lambda (k31)
                ((cps py-list?)
                 e18
                 (lambda (rv32)
                   (if rv32
                     ((cps py-list-ref) e18 i17 k31)
                     ((lambda (k33)
                        ((cps tuple?)
                         e18
                         (lambda (rv34)
                           (if rv34
                             ((cps tuple-ref) e18 i17 k33)
                             ((lambda (k35)
                                ((cps dict?)
                                 e18
                                 (lambda (rv36)
                                   (if rv36
                                     ((cps dict-ref) e18 i17 k35)
                                     (error "cannot index object" k35)))))
                              k33)))))
                      k31)))))
              k30))
           0
           k29))
        t16
        (lambda (rv19)
          (set-then!
           g$a
           rv19
           ((lambda (e20 k21)
              ((lambda (i19 k22)
                 ((lambda (k23)
                    ((cps py-list?)
                     e20
                     (lambda (rv24)
                       (if rv24
                         ((cps py-list-ref) e20 i19 k23)
                         ((lambda (k25)
                            ((cps tuple?)
                             e20
                             (lambda (rv26)
                               (if rv26
                                 ((cps tuple-ref) e20 i19 k25)
                                 ((lambda (k27)
                                    ((cps dict?)
                                     e20
                                     (lambda (rv28)
                                       (if rv28
                                         ((cps dict-ref) e20 i19 k27)
                                         (error "cannot index object" k27)))))
                                  k25)))))
                          k23)))))
                  k22))
               1
               k21))
            t16
            (lambda (rv20) (set-then! g$b rv20 (k18 (void)))))))))
     rv37
     (lambda (rv16)
       ((cps py-print) g$a (lambda (rv17) ((cps py-print) g$b $halt))))))
  (tuple 1 2)))
