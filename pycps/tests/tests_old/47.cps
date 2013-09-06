(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$d (void))
 (define g$b (void))
 (define g$c (void))
 (define g$a (void))
 ((lambda (rv57)
    ((lambda (t16 k20)
       ((lambda (e18 k49)
          ((lambda (i17 k50)
             ((lambda (k51)
                ((cps py-list?)
                 e18
                 (lambda (rv52)
                   (if rv52
                     ((cps py-list-ref) e18 i17 k51)
                     ((lambda (k53)
                        ((cps tuple?)
                         e18
                         (lambda (rv54)
                           (if rv54
                             ((cps tuple-ref) e18 i17 k53)
                             ((lambda (k55)
                                ((cps dict?)
                                 e18
                                 (lambda (rv56)
                                   (if rv56
                                     ((cps dict-ref) e18 i17 k55)
                                     (error "cannot index object" k55)))))
                              k53)))))
                      k51)))))
              k50))
           0
           k49))
        t16
        (lambda (rv21)
          (set-then!
           g$a
           rv21
           ((lambda (e20 k41)
              ((lambda (i19 k42)
                 ((lambda (k43)
                    ((cps py-list?)
                     e20
                     (lambda (rv44)
                       (if rv44
                         ((cps py-list-ref) e20 i19 k43)
                         ((lambda (k45)
                            ((cps tuple?)
                             e20
                             (lambda (rv46)
                               (if rv46
                                 ((cps tuple-ref) e20 i19 k45)
                                 ((lambda (k47)
                                    ((cps dict?)
                                     e20
                                     (lambda (rv48)
                                       (if rv48
                                         ((cps dict-ref) e20 i19 k47)
                                         (error "cannot index object" k47)))))
                                  k45)))))
                          k43)))))
                  k42))
               1
               k41))
            t16
            (lambda (rv22)
              (set-then!
               g$b
               rv22
               ((lambda (e22 k33)
                  ((lambda (i21 k34)
                     ((lambda (k35)
                        ((cps py-list?)
                         e22
                         (lambda (rv36)
                           (if rv36
                             ((cps py-list-ref) e22 i21 k35)
                             ((lambda (k37)
                                ((cps tuple?)
                                 e22
                                 (lambda (rv38)
                                   (if rv38
                                     ((cps tuple-ref) e22 i21 k37)
                                     ((lambda (k39)
                                        ((cps dict?)
                                         e22
                                         (lambda (rv40)
                                           (if rv40
                                             ((cps dict-ref) e22 i21 k39)
                                             (error
                                              "cannot index object"
                                              k39)))))
                                      k37)))))
                              k35)))))
                      k34))
                   2
                   k33))
                t16
                (lambda (rv23)
                  (set-then!
                   g$c
                   rv23
                   ((lambda (e24 k25)
                      ((lambda (i23 k26)
                         ((lambda (k27)
                            ((cps py-list?)
                             e24
                             (lambda (rv28)
                               (if rv28
                                 ((cps py-list-ref) e24 i23 k27)
                                 ((lambda (k29)
                                    ((cps tuple?)
                                     e24
                                     (lambda (rv30)
                                       (if rv30
                                         ((cps tuple-ref) e24 i23 k29)
                                         ((lambda (k31)
                                            ((cps dict?)
                                             e24
                                             (lambda (rv32)
                                               (if rv32
                                                 ((cps dict-ref) e24 i23 k31)
                                                 (error
                                                  "cannot index object"
                                                  k31)))))
                                          k29)))))
                                  k27)))))
                          k26))
                       3
                       k25))
                    t16
                    (lambda (rv24)
                      (set-then! g$d rv24 (k20 (void)))))))))))))))
     rv57
     (lambda (rv16)
       ((cps py-print)
        g$a
        (lambda (rv17)
          ((cps py-print)
           g$b
           (lambda (rv18)
             ((cps py-print)
              g$c
              (lambda (rv19) ((cps py-print) g$d $halt))))))))))
  (tuple 1 2 3 4)))
