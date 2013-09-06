(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 ((lambda (rv142)
    ((lambda (rv141)
       ((lambda (rv16)
          (set-then!
           g$x
           rv16
           ((lambda (e17 k133)
              ((lambda (i16 k134)
                 ((lambda (k135)
                    ((cps py-list?)
                     e17
                     (lambda (rv136)
                       (if rv136
                         ((cps py-list-ref) e17 i16 k135)
                         ((lambda (k137)
                            ((cps tuple?)
                             e17
                             (lambda (rv138)
                               (if rv138
                                 ((cps tuple-ref) e17 i16 k137)
                                 ((lambda (k139)
                                    ((cps dict?)
                                     e17
                                     (lambda (rv140)
                                       (if rv140
                                         ((cps dict-ref) e17 i16 k139)
                                         (error "cannot index object" k139)))))
                                  k137)))))
                          k135)))))
                  k134))
               0
               k133))
            g$x
            (lambda (rv132)
              ((lambda (e19 k124)
                 ((lambda (i18 k125)
                    ((lambda (k126)
                       ((cps py-list?)
                        e19
                        (lambda (rv127)
                          (if rv127
                            ((cps py-list-ref) e19 i18 k126)
                            ((lambda (k128)
                               ((cps tuple?)
                                e19
                                (lambda (rv129)
                                  (if rv129
                                    ((cps tuple-ref) e19 i18 k128)
                                    ((lambda (k130)
                                       ((cps dict?)
                                        e19
                                        (lambda (rv131)
                                          (if rv131
                                            ((cps dict-ref) e19 i18 k130)
                                            (error
                                             "cannot index object"
                                             k130)))))
                                     k128)))))
                             k126)))))
                     k125))
                  0
                  k124))
               rv132
               (lambda (rv123)
                 ((lambda (b21 k18)
                    ((lambda (i20 k19)
                       ((lambda (e24 k115)
                          ((lambda (i23 k116)
                             ((lambda (k117)
                                ((cps py-list?)
                                 e24
                                 (lambda (rv118)
                                   (if rv118
                                     ((cps py-list-ref) e24 i23 k117)
                                     ((lambda (k119)
                                        ((cps tuple?)
                                         e24
                                         (lambda (rv120)
                                           (if rv120
                                             ((cps tuple-ref) e24 i23 k119)
                                             ((lambda (k121)
                                                ((cps dict?)
                                                 e24
                                                 (lambda (rv122)
                                                   (if rv122
                                                     ((cps dict-ref)
                                                      e24
                                                      i23
                                                      k121)
                                                     (error
                                                      "cannot index object"
                                                      k121)))))
                                              k119)))))
                                      k117)))))
                              k116))
                           i20
                           k115))
                        b21
                        (lambda (rv114)
                          ((lambda (v22 k20)
                             ((lambda (k21)
                                ((cps tuple?)
                                 b21
                                 (lambda (rv22)
                                   (if rv22
                                     ((lambda (e26 k44)
                                        ((lambda (i25 k45)
                                           ((lambda (k46)
                                              ((cps py-list?)
                                               e26
                                               (lambda (rv47)
                                                 (if rv47
                                                   ((cps py-list-ref)
                                                    e26
                                                    i25
                                                    k46)
                                                   ((lambda (k48)
                                                      ((cps tuple?)
                                                       e26
                                                       (lambda (rv49)
                                                         (if rv49
                                                           ((cps tuple-ref)
                                                            e26
                                                            i25
                                                            k48)
                                                           ((lambda (k50)
                                                              ((cps dict?)
                                                               e26
                                                               (lambda (rv51)
                                                                 (if rv51
                                                                   ((cps
                                                                     dict-ref)
                                                                    e26
                                                                    i25
                                                                    k50)
                                                                   (error
                                                                    "cannot index object"
                                                                    k50)))))
                                                            k48)))))
                                                    k46)))))
                                            k45))
                                         0
                                         k44))
                                      g$x
                                      (lambda (rv43)
                                        ((lambda (e28 k35)
                                           ((lambda (i27 k36)
                                              ((lambda (k37)
                                                 ((cps py-list?)
                                                  e28
                                                  (lambda (rv38)
                                                    (if rv38
                                                      ((cps py-list-ref)
                                                       e28
                                                       i27
                                                       k37)
                                                      ((lambda (k39)
                                                         ((cps tuple?)
                                                          e28
                                                          (lambda (rv40)
                                                            (if rv40
                                                              ((cps tuple-ref)
                                                               e28
                                                               i27
                                                               k39)
                                                              ((lambda (k41)
                                                                 ((cps dict?)
                                                                  e28
                                                                  (lambda (rv42)
                                                                    (if rv42
                                                                      ((cps
                                                                        dict-ref)
                                                                       e28
                                                                       i27
                                                                       k41)
                                                                      (error
                                                                       "cannot index object"
                                                                       k41)))))
                                                               k39)))))
                                                       k37)))))
                                               k36))
                                            0
                                            k35))
                                         rv43
                                         (lambda (rv34)
                                           ((lambda (e30 k26)
                                              ((lambda (i29 k27)
                                                 ((lambda (k28)
                                                    ((cps py-list?)
                                                     e30
                                                     (lambda (rv29)
                                                       (if rv29
                                                         ((cps py-list-ref)
                                                          e30
                                                          i29
                                                          k28)
                                                         ((lambda (k30)
                                                            ((cps tuple?)
                                                             e30
                                                             (lambda (rv31)
                                                               (if rv31
                                                                 ((cps
                                                                   tuple-ref)
                                                                  e30
                                                                  i29
                                                                  k30)
                                                                 ((lambda (k32)
                                                                    ((cps
                                                                      dict?)
                                                                     e30
                                                                     (lambda (rv33)
                                                                       (if rv33
                                                                         ((cps
                                                                           dict-ref)
                                                                          e30
                                                                          i29
                                                                          k32)
                                                                         (error
                                                                          "cannot index object"
                                                                          k32)))))
                                                                  k30)))))
                                                          k28)))))
                                                  k27))
                                               0
                                               k26))
                                            rv34
                                            (lambda (rv25)
                                              ((cps *)
                                               100
                                               rv25
                                               (lambda (rv24)
                                                 ((cps +)
                                                  v22
                                                  rv24
                                                  (lambda (rv23)
                                                    ((cps tuple-set!)
                                                     b21
                                                     i20
                                                     rv23
                                                     k21)))))))))))
                                     ((lambda (k52)
                                        ((cps py-list?)
                                         b21
                                         (lambda (rv53)
                                           (if rv53
                                             ((lambda (e32 k75)
                                                ((lambda (i31 k76)
                                                   ((lambda (k77)
                                                      ((cps py-list?)
                                                       e32
                                                       (lambda (rv78)
                                                         (if rv78
                                                           ((cps py-list-ref)
                                                            e32
                                                            i31
                                                            k77)
                                                           ((lambda (k79)
                                                              ((cps tuple?)
                                                               e32
                                                               (lambda (rv80)
                                                                 (if rv80
                                                                   ((cps
                                                                     tuple-ref)
                                                                    e32
                                                                    i31
                                                                    k79)
                                                                   ((lambda (k81)
                                                                      ((cps
                                                                        dict?)
                                                                       e32
                                                                       (lambda (rv82)
                                                                         (if rv82
                                                                           ((cps
                                                                             dict-ref)
                                                                            e32
                                                                            i31
                                                                            k81)
                                                                           (error
                                                                            "cannot index object"
                                                                            k81)))))
                                                                    k79)))))
                                                            k77)))))
                                                    k76))
                                                 0
                                                 k75))
                                              g$x
                                              (lambda (rv74)
                                                ((lambda (e34 k66)
                                                   ((lambda (i33 k67)
                                                      ((lambda (k68)
                                                         ((cps py-list?)
                                                          e34
                                                          (lambda (rv69)
                                                            (if rv69
                                                              ((cps
                                                                py-list-ref)
                                                               e34
                                                               i33
                                                               k68)
                                                              ((lambda (k70)
                                                                 ((cps tuple?)
                                                                  e34
                                                                  (lambda (rv71)
                                                                    (if rv71
                                                                      ((cps
                                                                        tuple-ref)
                                                                       e34
                                                                       i33
                                                                       k70)
                                                                      ((lambda (k72)
                                                                         ((cps
                                                                           dict?)
                                                                          e34
                                                                          (lambda (rv73)
                                                                            (if rv73
                                                                              ((cps
                                                                                dict-ref)
                                                                               e34
                                                                               i33
                                                                               k72)
                                                                              (error
                                                                               "cannot index object"
                                                                               k72)))))
                                                                       k70)))))
                                                               k68)))))
                                                       k67))
                                                    0
                                                    k66))
                                                 rv74
                                                 (lambda (rv65)
                                                   ((lambda (e36 k57)
                                                      ((lambda (i35 k58)
                                                         ((lambda (k59)
                                                            ((cps py-list?)
                                                             e36
                                                             (lambda (rv60)
                                                               (if rv60
                                                                 ((cps
                                                                   py-list-ref)
                                                                  e36
                                                                  i35
                                                                  k59)
                                                                 ((lambda (k61)
                                                                    ((cps
                                                                      tuple?)
                                                                     e36
                                                                     (lambda (rv62)
                                                                       (if rv62
                                                                         ((cps
                                                                           tuple-ref)
                                                                          e36
                                                                          i35
                                                                          k61)
                                                                         ((lambda (k63)
                                                                            ((cps
                                                                              dict?)
                                                                             e36
                                                                             (lambda (rv64)
                                                                               (if rv64
                                                                                 ((cps
                                                                                   dict-ref)
                                                                                  e36
                                                                                  i35
                                                                                  k63)
                                                                                 (error
                                                                                  "cannot index object"
                                                                                  k63)))))
                                                                          k61)))))
                                                                  k59)))))
                                                          k58))
                                                       0
                                                       k57))
                                                    rv65
                                                    (lambda (rv56)
                                                      ((cps *)
                                                       100
                                                       rv56
                                                       (lambda (rv55)
                                                         ((cps +)
                                                          v22
                                                          rv55
                                                          (lambda (rv54)
                                                            ((cps py-list-set!)
                                                             b21
                                                             i20
                                                             rv54
                                                             k52)))))))))))
                                             ((lambda (k83)
                                                ((cps dict?)
                                                 b21
                                                 (lambda (rv84)
                                                   (if rv84
                                                     ((lambda (e38 k106)
                                                        ((lambda (i37 k107)
                                                           ((lambda (k108)
                                                              ((cps py-list?)
                                                               e38
                                                               (lambda (rv109)
                                                                 (if rv109
                                                                   ((cps
                                                                     py-list-ref)
                                                                    e38
                                                                    i37
                                                                    k108)
                                                                   ((lambda (k110)
                                                                      ((cps
                                                                        tuple?)
                                                                       e38
                                                                       (lambda (rv111)
                                                                         (if rv111
                                                                           ((cps
                                                                             tuple-ref)
                                                                            e38
                                                                            i37
                                                                            k110)
                                                                           ((lambda (k112)
                                                                              ((cps
                                                                                dict?)
                                                                               e38
                                                                               (lambda (rv113)
                                                                                 (if rv113
                                                                                   ((cps
                                                                                     dict-ref)
                                                                                    e38
                                                                                    i37
                                                                                    k112)
                                                                                   (error
                                                                                    "cannot index object"
                                                                                    k112)))))
                                                                            k110)))))
                                                                    k108)))))
                                                            k107))
                                                         0
                                                         k106))
                                                      g$x
                                                      (lambda (rv105)
                                                        ((lambda (e40 k97)
                                                           ((lambda (i39 k98)
                                                              ((lambda (k99)
                                                                 ((cps
                                                                   py-list?)
                                                                  e40
                                                                  (lambda (rv100)
                                                                    (if rv100
                                                                      ((cps
                                                                        py-list-ref)
                                                                       e40
                                                                       i39
                                                                       k99)
                                                                      ((lambda (k101)
                                                                         ((cps
                                                                           tuple?)
                                                                          e40
                                                                          (lambda (rv102)
                                                                            (if rv102
                                                                              ((cps
                                                                                tuple-ref)
                                                                               e40
                                                                               i39
                                                                               k101)
                                                                              ((lambda (k103)
                                                                                 ((cps
                                                                                   dict?)
                                                                                  e40
                                                                                  (lambda (rv104)
                                                                                    (if rv104
                                                                                      ((cps
                                                                                        dict-ref)
                                                                                       e40
                                                                                       i39
                                                                                       k103)
                                                                                      (error
                                                                                       "cannot index object"
                                                                                       k103)))))
                                                                               k101)))))
                                                                       k99)))))
                                                               k98))
                                                            0
                                                            k97))
                                                         rv105
                                                         (lambda (rv96)
                                                           ((lambda (e42 k88)
                                                              ((lambda (i41
                                                                        k89)
                                                                 ((lambda (k90)
                                                                    ((cps
                                                                      py-list?)
                                                                     e42
                                                                     (lambda (rv91)
                                                                       (if rv91
                                                                         ((cps
                                                                           py-list-ref)
                                                                          e42
                                                                          i41
                                                                          k90)
                                                                         ((lambda (k92)
                                                                            ((cps
                                                                              tuple?)
                                                                             e42
                                                                             (lambda (rv93)
                                                                               (if rv93
                                                                                 ((cps
                                                                                   tuple-ref)
                                                                                  e42
                                                                                  i41
                                                                                  k92)
                                                                                 ((lambda (k94)
                                                                                    ((cps
                                                                                      dict?)
                                                                                     e42
                                                                                     (lambda (rv95)
                                                                                       (if rv95
                                                                                         ((cps
                                                                                           dict-ref)
                                                                                          e42
                                                                                          i41
                                                                                          k94)
                                                                                         (error
                                                                                          "cannot index object"
                                                                                          k94)))))
                                                                                  k92)))))
                                                                          k90)))))
                                                                  k89))
                                                               0
                                                               k88))
                                                            rv96
                                                            (lambda (rv87)
                                                              ((cps *)
                                                               100
                                                               rv87
                                                               (lambda (rv86)
                                                                 ((cps +)
                                                                  v22
                                                                  rv86
                                                                  (lambda (rv85)
                                                                    ((cps
                                                                      dict-set!)
                                                                     b21
                                                                     i20
                                                                     rv85
                                                                     k83)))))))))))
                                                     (k83 (void))))))
                                              k52)))))
                                      k21)))))
                              k20))
                           rv114
                           k19))))
                     0
                     k18))
                  rv123
                  (lambda (rv17) ((cps py-print) g$x $halt)))))))))
        (py-list* rv141)))
     (py-list* rv142)))
  (py-list* 5)))
