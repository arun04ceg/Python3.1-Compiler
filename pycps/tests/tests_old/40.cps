(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (define g$y (void))
 (define g$a (void))
 ((lambda (rv16)
    (set-then!
     g$x
     rv16
     ((lambda (rv17)
        (set-then!
         g$y
         rv17
         (set-then!
          g$a
          (lambda (k18)
            ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
             (lambda (return k19)
               ((lambda (i j k20)
                  ((lambda (k21)
                     ((cps equal?)
                      5
                      3
                      (lambda (rv22)
                        (if rv22
                          ((lambda (k23) (k23 (void))) k21)
                          ((lambda (k24)
                             ((cps modulo)
                              99
                              50
                              (lambda (rv102)
                                ((cps <)
                                 5
                                 rv102
                                 (lambda (rv25)
                                   (if rv25
                                     ((lambda (k26)
                                        ((lambda (f cc)
                                           (f (lambda (x k) (cc x)) cc))
                                         (lambda (break k74)
                                           ((lambda ($seq16 $loop17 k75)
                                              ((cps set?)
                                               $seq16
                                               (lambda (rv77)
                                                 (if rv77
                                                   (for-set-k
                                                    $seq16
                                                    $loop17
                                                    (lambda (rv76)
                                                      (k75 (void))))
                                                   ((lambda (k78)
                                                      ((cps tuple?)
                                                       $seq16
                                                       (lambda (rv79)
                                                         (if rv79
                                                           (for-tuple-k
                                                            $seq16
                                                            $loop17
                                                            k78)
                                                           ((lambda (k80)
                                                              ((cps py-list?)
                                                               $seq16
                                                               (lambda (rv81)
                                                                 (if rv81
                                                                   (for-py-list-k
                                                                    $seq16
                                                                    $loop17
                                                                    k80)
                                                                   ((lambda (k82)
                                                                      ((cps
                                                                        dict?)
                                                                       $seq16
                                                                       (lambda (rv83)
                                                                         (if rv83
                                                                           (for-dict-k
                                                                            $seq16
                                                                            $loop17
                                                                            k82)
                                                                           (k82
                                                                            (void))))))
                                                                    k80)))))
                                                            k78)))))
                                                    (lambda (rv76)
                                                      (k75 (void))))))))
                                            g$x
                                            (lambda (i16 k84)
                                              ((lambda (f cc)
                                                 (f (lambda (x k) (cc x)) cc))
                                               (lambda (continue k85)
                                                 (set-then!
                                                  i
                                                  i16
                                                  ((lambda (k86)
                                                     ((cps py-print)
                                                      i
                                                      (lambda (rv87)
                                                        ((lambda (k88)
                                                           ((cps modulo)
                                                            i
                                                            2
                                                            (lambda (rv101)
                                                              ((cps equal?)
                                                               rv101
                                                               0
                                                               (lambda (rv89)
                                                                 (if rv89
                                                                   ((lambda (k90)
                                                                      ((lambda (e18
                                                                                k92)
                                                                         ((cps
                                                                           modulo)
                                                                          i
                                                                          2
                                                                          (lambda (rv100)
                                                                            ((lambda (i17
                                                                                      k93)
                                                                               ((lambda (k94)
                                                                                  ((cps
                                                                                    py-list?)
                                                                                   e18
                                                                                   (lambda (rv95)
                                                                                     (if rv95
                                                                                       ((cps
                                                                                         py-list-ref)
                                                                                        e18
                                                                                        i17
                                                                                        k94)
                                                                                       ((lambda (k96)
                                                                                          ((cps
                                                                                            tuple?)
                                                                                           e18
                                                                                           (lambda (rv97)
                                                                                             (if rv97
                                                                                               ((cps
                                                                                                 tuple-ref)
                                                                                                e18
                                                                                                i17
                                                                                                k96)
                                                                                               ((lambda (k98)
                                                                                                  ((cps
                                                                                                    dict?)
                                                                                                   e18
                                                                                                   (lambda (rv99)
                                                                                                     (if rv99
                                                                                                       ((cps
                                                                                                         dict-ref)
                                                                                                        e18
                                                                                                        i17
                                                                                                        k98)
                                                                                                       (error
                                                                                                        "cannot index object"
                                                                                                        k98)))))
                                                                                                k96)))))
                                                                                        k94)))))
                                                                                k93))
                                                                             rv100
                                                                             k92))))
                                                                       g$y
                                                                       (lambda (rv91)
                                                                         ((cps
                                                                           py-print)
                                                                          rv91
                                                                          k90))))
                                                                    k88)
                                                                   (k88
                                                                    (void))))))))
                                                         k86))))
                                                   k85)))
                                               k84))
                                            k74))
                                         (lambda (rv27)
                                           (set-then!
                                            i
                                            0
                                            ((lambda (f cc)
                                               (f (lambda (x k) (cc x)) cc))
                                             (lambda (break k28)
                                               ((lambda ($seq16 $loop17 k29)
                                                  ((cps set?)
                                                   $seq16
                                                   (lambda (rv31)
                                                     (if rv31
                                                       (for-set-k
                                                        $seq16
                                                        $loop17
                                                        (lambda (rv30)
                                                          (k29 (void))))
                                                       ((lambda (k32)
                                                          ((cps tuple?)
                                                           $seq16
                                                           (lambda (rv33)
                                                             (if rv33
                                                               (for-tuple-k
                                                                $seq16
                                                                $loop17
                                                                k32)
                                                               ((lambda (k34)
                                                                  ((cps
                                                                    py-list?)
                                                                   $seq16
                                                                   (lambda (rv35)
                                                                     (if rv35
                                                                       (for-py-list-k
                                                                        $seq16
                                                                        $loop17
                                                                        k34)
                                                                       ((lambda (k36)
                                                                          ((cps
                                                                            dict?)
                                                                           $seq16
                                                                           (lambda (rv37)
                                                                             (if rv37
                                                                               (for-dict-k
                                                                                $seq16
                                                                                $loop17
                                                                                k36)
                                                                               (k36
                                                                                (void))))))
                                                                        k34)))))
                                                                k32)))))
                                                        (lambda (rv30)
                                                          (k29 (void))))))))
                                                g$x
                                                (lambda (i19 k38)
                                                  ((lambda (f cc)
                                                     (f
                                                      (lambda (x k) (cc x))
                                                      cc))
                                                   (lambda (continue k39)
                                                     (set-then!
                                                      j
                                                      i19
                                                      ((lambda (k40)
                                                         ((lambda (e21 k66)
                                                            ((lambda (i20 k67)
                                                               ((lambda (k68)
                                                                  ((cps
                                                                    py-list?)
                                                                   e21
                                                                   (lambda (rv69)
                                                                     (if rv69
                                                                       ((cps
                                                                         py-list-ref)
                                                                        e21
                                                                        i20
                                                                        k68)
                                                                       ((lambda (k70)
                                                                          ((cps
                                                                            tuple?)
                                                                           e21
                                                                           (lambda (rv71)
                                                                             (if rv71
                                                                               ((cps
                                                                                 tuple-ref)
                                                                                e21
                                                                                i20
                                                                                k70)
                                                                               ((lambda (k72)
                                                                                  ((cps
                                                                                    dict?)
                                                                                   e21
                                                                                   (lambda (rv73)
                                                                                     (if rv73
                                                                                       ((cps
                                                                                         dict-ref)
                                                                                        e21
                                                                                        i20
                                                                                        k72)
                                                                                       (error
                                                                                        "cannot index object"
                                                                                        k72)))))
                                                                                k70)))))
                                                                        k68)))))
                                                                k67))
                                                             i
                                                             k66))
                                                          g$x
                                                          (lambda (rv65)
                                                            ((cps modulo)
                                                             rv65
                                                             2
                                                             (lambda (rv64)
                                                               ((cps equal?)
                                                                rv64
                                                                0
                                                                (lambda (rv43)
                                                                  (if rv43
                                                                    ((lambda (k44)
                                                                       ((lambda (e23
                                                                                 k46)
                                                                          ((lambda (e25
                                                                                    k56)
                                                                             ((lambda (i24
                                                                                       k57)
                                                                                ((lambda (k58)
                                                                                   ((cps
                                                                                     py-list?)
                                                                                    e25
                                                                                    (lambda (rv59)
                                                                                      (if rv59
                                                                                        ((cps
                                                                                          py-list-ref)
                                                                                         e25
                                                                                         i24
                                                                                         k58)
                                                                                        ((lambda (k60)
                                                                                           ((cps
                                                                                             tuple?)
                                                                                            e25
                                                                                            (lambda (rv61)
                                                                                              (if rv61
                                                                                                ((cps
                                                                                                  tuple-ref)
                                                                                                 e25
                                                                                                 i24
                                                                                                 k60)
                                                                                                ((lambda (k62)
                                                                                                   ((cps
                                                                                                     dict?)
                                                                                                    e25
                                                                                                    (lambda (rv63)
                                                                                                      (if rv63
                                                                                                        ((cps
                                                                                                          dict-ref)
                                                                                                         e25
                                                                                                         i24
                                                                                                         k62)
                                                                                                        (error
                                                                                                         "cannot index object"
                                                                                                         k62)))))
                                                                                                 k60)))))
                                                                                         k58)))))
                                                                                 k57))
                                                                              i
                                                                              k56))
                                                                           g$x
                                                                           (lambda (rv55)
                                                                             ((cps
                                                                               modulo)
                                                                              rv55
                                                                              2
                                                                              (lambda (rv54)
                                                                                ((lambda (i22
                                                                                          k47)
                                                                                   ((lambda (k48)
                                                                                      ((cps
                                                                                        py-list?)
                                                                                       e23
                                                                                       (lambda (rv49)
                                                                                         (if rv49
                                                                                           ((cps
                                                                                             py-list-ref)
                                                                                            e23
                                                                                            i22
                                                                                            k48)
                                                                                           ((lambda (k50)
                                                                                              ((cps
                                                                                                tuple?)
                                                                                               e23
                                                                                               (lambda (rv51)
                                                                                                 (if rv51
                                                                                                   ((cps
                                                                                                     tuple-ref)
                                                                                                    e23
                                                                                                    i22
                                                                                                    k50)
                                                                                                   ((lambda (k52)
                                                                                                      ((cps
                                                                                                        dict?)
                                                                                                       e23
                                                                                                       (lambda (rv53)
                                                                                                         (if rv53
                                                                                                           ((cps
                                                                                                             dict-ref)
                                                                                                            e23
                                                                                                            i22
                                                                                                            k52)
                                                                                                           (error
                                                                                                            "cannot index object"
                                                                                                            k52)))))
                                                                                                    k50)))))
                                                                                            k48)))))
                                                                                    k47))
                                                                                 rv54
                                                                                 k46))))))
                                                                        g$y
                                                                        (lambda (rv45)
                                                                          ((cps
                                                                            py-print)
                                                                           rv45
                                                                           k44))))
                                                                     (lambda (rv41)
                                                                       ((cps +)
                                                                        i
                                                                        1
                                                                        (lambda (rv42)
                                                                          (set-then!
                                                                           i
                                                                           rv42
                                                                           (k40
                                                                            (void)))))))
                                                                    ((lambda (rv41)
                                                                       ((cps +)
                                                                        i
                                                                        1
                                                                        (lambda (rv42)
                                                                          (set-then!
                                                                           i
                                                                           rv42
                                                                           (k40
                                                                            (void))))))
                                                                     (void))))))))))
                                                       k39)))
                                                   k38))
                                                k28))
                                             k26)))))
                                      k24)
                                     (k24 (void))))))))
                           k21)))))
                   k20))
                (void)
                (void)
                k19))
             k18))
          (g$a $halt))))
      (py-list* "bob" "bar" "billy"))))
  (py-list* 1 5 1 43 2 6 2 95 10 0 5 4)))
