(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$c (void))
 (define g$a (void))
 ((lambda (rv48)
    ((lambda (rv47)
       ((lambda (t16 k19)
          ((lambda (e18 k39)
             ((lambda (i17 k40)
                ((lambda (k41)
                   ((cps py-list?)
                    e18
                    (lambda (rv42)
                      (if rv42
                        ((cps py-list-ref) e18 i17 k41)
                        ((lambda (k43)
                           ((cps tuple?)
                            e18
                            (lambda (rv44)
                              (if rv44
                                ((cps tuple-ref) e18 i17 k43)
                                ((lambda (k45)
                                   ((cps dict?)
                                    e18
                                    (lambda (rv46)
                                      (if rv46
                                        ((cps dict-ref) e18 i17 k45)
                                        (error "cannot index object" k45)))))
                                 k43)))))
                         k41)))))
                 k40))
              0
              k39))
           t16
           (lambda (rv20)
             (set-then!
              g$a
              rv20
              ((lambda (e20 k31)
                 ((lambda (i19 k32)
                    ((lambda (k33)
                       ((cps py-list?)
                        e20
                        (lambda (rv34)
                          (if rv34
                            ((cps py-list-ref) e20 i19 k33)
                            ((lambda (k35)
                               ((cps tuple?)
                                e20
                                (lambda (rv36)
                                  (if rv36
                                    ((cps tuple-ref) e20 i19 k35)
                                    ((lambda (k37)
                                       ((cps dict?)
                                        e20
                                        (lambda (rv38)
                                          (if rv38
                                            ((cps dict-ref) e20 i19 k37)
                                            (error
                                             "cannot index object"
                                             k37)))))
                                     k35)))))
                             k33)))))
                     k32))
                  1
                  k31))
               t16
               (lambda (rv21)
                 (set-then!
                  g$b
                  rv21
                  ((lambda (e22 k23)
                     ((lambda (i21 k24)
                        ((lambda (k25)
                           ((cps py-list?)
                            e22
                            (lambda (rv26)
                              (if rv26
                                ((cps py-list-ref) e22 i21 k25)
                                ((lambda (k27)
                                   ((cps tuple?)
                                    e22
                                    (lambda (rv28)
                                      (if rv28
                                        ((cps tuple-ref) e22 i21 k27)
                                        ((lambda (k29)
                                           ((cps dict?)
                                            e22
                                            (lambda (rv30)
                                              (if rv30
                                                ((cps dict-ref) e22 i21 k29)
                                                (error
                                                 "cannot index object"
                                                 k29)))))
                                         k27)))))
                                 k25)))))
                         k24))
                      2
                      k23))
                   t16
                   (lambda (rv22) (set-then! g$c rv22 (k19 (void))))))))))))
        rv47
        (lambda (rv16)
          ((cps py-print)
           g$a
           (lambda (rv17)
             ((cps py-print)
              g$b
              (lambda (rv18) ((cps py-print) g$c $halt))))))))
     (py-list* 1 2 rv48)))
  (py-list* 3 4)))
