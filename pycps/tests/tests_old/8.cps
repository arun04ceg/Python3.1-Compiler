(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (define g$y (void))
 (define g$x (void))
 (set-then!
  g$a
  10
  ((lambda (rv38)
     ((lambda (t16 k19)
        ((lambda (e18 k30)
           ((lambda (i17 k31)
              ((lambda (k32)
                 ((cps py-list?)
                  e18
                  (lambda (rv33)
                    (if rv33
                      ((cps py-list-ref) e18 i17 k32)
                      ((lambda (k34)
                         ((cps tuple?)
                          e18
                          (lambda (rv35)
                            (if rv35
                              ((cps tuple-ref) e18 i17 k34)
                              ((lambda (k36)
                                 ((cps dict?)
                                  e18
                                  (lambda (rv37)
                                    (if rv37
                                      ((cps dict-ref) e18 i17 k36)
                                      (error "cannot index object" k36)))))
                               k34)))))
                       k32)))))
               k31))
            0
            k30))
         t16
         (lambda (rv20)
           (set-then!
            g$x
            rv20
            ((lambda (e20 k22)
               ((lambda (i19 k23)
                  ((lambda (k24)
                     ((cps py-list?)
                      e20
                      (lambda (rv25)
                        (if rv25
                          ((cps py-list-ref) e20 i19 k24)
                          ((lambda (k26)
                             ((cps tuple?)
                              e20
                              (lambda (rv27)
                                (if rv27
                                  ((cps tuple-ref) e20 i19 k26)
                                  ((lambda (k28)
                                     ((cps dict?)
                                      e20
                                      (lambda (rv29)
                                        (if rv29
                                          ((cps dict-ref) e20 i19 k28)
                                          (error "cannot index object" k28)))))
                                   k26)))))
                           k24)))))
                   k23))
                1
                k22))
             t16
             (lambda (rv21) (set-then! g$y rv21 (k19 (void)))))))))
      rv38
      (lambda (rv16)
        ((cps py-print)
         g$a
         (lambda (rv17)
           ((cps py-print) g$x (lambda (rv18) ((cps py-print) g$y $halt))))))))
   (tuple 42 1701))))
