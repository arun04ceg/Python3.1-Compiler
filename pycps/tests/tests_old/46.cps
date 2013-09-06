(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$d (void))
 (define g$b (void))
 (define g$c (void))
 (define g$a (void))
 (define g$w (void))
 ((lambda (rv80)
    ((lambda (t16 k34)
       ((lambda (e18 k72)
          ((lambda (i17 k73)
             ((lambda (k74)
                ((cps py-list?)
                 e18
                 (lambda (rv75)
                   (if rv75
                     ((cps py-list-ref) e18 i17 k74)
                     ((lambda (k76)
                        ((cps tuple?)
                         e18
                         (lambda (rv77)
                           (if rv77
                             ((cps tuple-ref) e18 i17 k76)
                             ((lambda (k78)
                                ((cps dict?)
                                 e18
                                 (lambda (rv79)
                                   (if rv79
                                     ((cps dict-ref) e18 i17 k78)
                                     (error "cannot index object" k78)))))
                              k76)))))
                      k74)))))
              k73))
           0
           k72))
        t16
        (lambda (rv35)
          (set-then!
           g$a
           rv35
           ((lambda (e20 k64)
              ((lambda (i19 k65)
                 ((lambda (k66)
                    ((cps py-list?)
                     e20
                     (lambda (rv67)
                       (if rv67
                         ((cps py-list-ref) e20 i19 k66)
                         ((lambda (k68)
                            ((cps tuple?)
                             e20
                             (lambda (rv69)
                               (if rv69
                                 ((cps tuple-ref) e20 i19 k68)
                                 ((lambda (k70)
                                    ((cps dict?)
                                     e20
                                     (lambda (rv71)
                                       (if rv71
                                         ((cps dict-ref) e20 i19 k70)
                                         (error "cannot index object" k70)))))
                                  k68)))))
                          k66)))))
                  k65))
               1
               k64))
            t16
            (lambda (rv36)
              (set-then!
               g$b
               rv36
               ((lambda (e22 k56)
                  ((lambda (i21 k57)
                     ((lambda (k58)
                        ((cps py-list?)
                         e22
                         (lambda (rv59)
                           (if rv59
                             ((cps py-list-ref) e22 i21 k58)
                             ((lambda (k60)
                                ((cps tuple?)
                                 e22
                                 (lambda (rv61)
                                   (if rv61
                                     ((cps tuple-ref) e22 i21 k60)
                                     ((lambda (k62)
                                        ((cps dict?)
                                         e22
                                         (lambda (rv63)
                                           (if rv63
                                             ((cps dict-ref) e22 i21 k62)
                                             (error
                                              "cannot index object"
                                              k62)))))
                                      k60)))))
                              k58)))))
                      k57))
                   2
                   k56))
                t16
                (lambda (rv37)
                  (set-then!
                   g$c
                   rv37
                   ((lambda (e24 k48)
                      ((lambda (i23 k49)
                         ((lambda (k50)
                            ((cps py-list?)
                             e24
                             (lambda (rv51)
                               (if rv51
                                 ((cps py-list-ref) e24 i23 k50)
                                 ((lambda (k52)
                                    ((cps tuple?)
                                     e24
                                     (lambda (rv53)
                                       (if rv53
                                         ((cps tuple-ref) e24 i23 k52)
                                         ((lambda (k54)
                                            ((cps dict?)
                                             e24
                                             (lambda (rv55)
                                               (if rv55
                                                 ((cps dict-ref) e24 i23 k54)
                                                 (error
                                                  "cannot index object"
                                                  k54)))))
                                          k52)))))
                                  k50)))))
                          k49))
                       3
                       k48))
                    t16
                    (lambda (rv38)
                      (set-then!
                       g$d
                       rv38
                       ((lambda (e26 k40)
                          ((lambda (i25 k41)
                             ((lambda (k42)
                                ((cps py-list?)
                                 e26
                                 (lambda (rv43)
                                   (if rv43
                                     ((cps py-list-ref) e26 i25 k42)
                                     ((lambda (k44)
                                        ((cps tuple?)
                                         e26
                                         (lambda (rv45)
                                           (if rv45
                                             ((cps tuple-ref) e26 i25 k44)
                                             ((lambda (k46)
                                                ((cps dict?)
                                                 e26
                                                 (lambda (rv47)
                                                   (if rv47
                                                     ((cps dict-ref)
                                                      e26
                                                      i25
                                                      k46)
                                                     (error
                                                      "cannot index object"
                                                      k46)))))
                                              k44)))))
                                      k42)))))
                              k41))
                           4
                           k40))
                        t16
                        (lambda (rv39)
                          (set-then! g$w rv39 (k34 (void))))))))))))))))))
     rv80
     (lambda (rv16)
       (if g$a
         ((lambda (rv33)
            ((lambda (t16 k18)
               ((lambda (k19)
                  (if t16
                    (k19 t16)
                    ((lambda (t17 k20)
                       ((lambda (k21)
                          (if t17
                            (k21 t17)
                            ((lambda (k22)
                               ((cps not)
                                g$b
                                (lambda (rv23)
                                  (if rv23
                                    ((lambda (t18 k24)
                                       ((lambda (k25)
                                          (if t18
                                            (k25 t18)
                                            ((lambda (t19 k26)
                                               ((lambda (k27)
                                                  (if t19
                                                    (k27 t19)
                                                    ((lambda (k28)
                                                       (if g$b
                                                         ((lambda (k29)
                                                            ((lambda (t20 k31)
                                                               ((lambda (k32)
                                                                  (if t20
                                                                    (k32 t20)
                                                                    (k32 g$d)))
                                                                k31))
                                                             g$c
                                                             (lambda (rv30)
                                                               (if rv30
                                                                 (k29 g$w)
                                                                 (k29 #f)))))
                                                          k28)
                                                         (k28 #f)))
                                                     k27)))
                                                k26))
                                             g$c
                                             k25)))
                                        k24))
                                     g$d
                                     k22)
                                    (k22 #f)))))
                             k21)))
                        k20))
                     3
                     k19)))
                k18))
             rv33
             (lambda (rv17) ((cps py-print) rv17 $halt))))
          g$b)
         ((lambda (rv33)
            ((lambda (t16 k18)
               ((lambda (k19)
                  (if t16
                    (k19 t16)
                    ((lambda (t17 k20)
                       ((lambda (k21)
                          (if t17
                            (k21 t17)
                            ((lambda (k22)
                               ((cps not)
                                g$b
                                (lambda (rv23)
                                  (if rv23
                                    ((lambda (t18 k24)
                                       ((lambda (k25)
                                          (if t18
                                            (k25 t18)
                                            ((lambda (t19 k26)
                                               ((lambda (k27)
                                                  (if t19
                                                    (k27 t19)
                                                    ((lambda (k28)
                                                       (if g$b
                                                         ((lambda (k29)
                                                            ((lambda (t20 k31)
                                                               ((lambda (k32)
                                                                  (if t20
                                                                    (k32 t20)
                                                                    (k32 g$d)))
                                                                k31))
                                                             g$c
                                                             (lambda (rv30)
                                                               (if rv30
                                                                 (k29 g$w)
                                                                 (k29 #f)))))
                                                          k28)
                                                         (k28 #f)))
                                                     k27)))
                                                k26))
                                             g$c
                                             k25)))
                                        k24))
                                     g$d
                                     k22)
                                    (k22 #f)))))
                             k21)))
                        k20))
                     3
                     k19)))
                k18))
             rv33
             (lambda (rv17) ((cps py-print) rv17 $halt))))
          #f)))))
  (tuple 1 2 3 4 5)))
