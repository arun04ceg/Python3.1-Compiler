(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (define g$y (void))
 (define g$z (void))
 ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
  (lambda (break k16)
    ((lambda (rv26)
       ((lambda ($seq16 $loop17 k17)
          ((cps set?)
           $seq16
           (lambda (rv19)
             (if rv19
               (for-set-k $seq16 $loop17 (lambda (rv18) (k17 (void))))
               ((lambda (k20)
                  ((cps tuple?)
                   $seq16
                   (lambda (rv21)
                     (if rv21
                       (for-tuple-k $seq16 $loop17 k20)
                       ((lambda (k22)
                          ((cps py-list?)
                           $seq16
                           (lambda (rv23)
                             (if rv23
                               (for-py-list-k $seq16 $loop17 k22)
                               ((lambda (k24)
                                  ((cps dict?)
                                   $seq16
                                   (lambda (rv25)
                                     (if rv25
                                       (for-dict-k $seq16 $loop17 k24)
                                       (k24 (void))))))
                                k22)))))
                        k20)))))
                (lambda (rv18) (k17 (void))))))))
        rv26
        (lambda (i16 k27)
          ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
           (lambda (continue k28)
             (set-then!
              g$x
              i16
              ((lambda (k29)
                 ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                  (lambda (break k30)
                    ((lambda (rv40)
                       ((lambda ($seq16 $loop17 k31)
                          ((cps set?)
                           $seq16
                           (lambda (rv33)
                             (if rv33
                               (for-set-k
                                $seq16
                                $loop17
                                (lambda (rv32) (k31 (void))))
                               ((lambda (k34)
                                  ((cps tuple?)
                                   $seq16
                                   (lambda (rv35)
                                     (if rv35
                                       (for-tuple-k $seq16 $loop17 k34)
                                       ((lambda (k36)
                                          ((cps py-list?)
                                           $seq16
                                           (lambda (rv37)
                                             (if rv37
                                               (for-py-list-k
                                                $seq16
                                                $loop17
                                                k36)
                                               ((lambda (k38)
                                                  ((cps dict?)
                                                   $seq16
                                                   (lambda (rv39)
                                                     (if rv39
                                                       (for-dict-k
                                                        $seq16
                                                        $loop17
                                                        k38)
                                                       (k38 (void))))))
                                                k36)))))
                                        k34)))))
                                (lambda (rv32) (k31 (void))))))))
                        rv40
                        (lambda (i17 k41)
                          ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                           (lambda (continue k42)
                             (set-then!
                              g$y
                              i17
                              ((lambda (k43)
                                 ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                                  (lambda (break k44)
                                    ((lambda (rv54)
                                       ((lambda ($seq16 $loop17 k45)
                                          ((cps set?)
                                           $seq16
                                           (lambda (rv47)
                                             (if rv47
                                               (for-set-k
                                                $seq16
                                                $loop17
                                                (lambda (rv46) (k45 (void))))
                                               ((lambda (k48)
                                                  ((cps tuple?)
                                                   $seq16
                                                   (lambda (rv49)
                                                     (if rv49
                                                       (for-tuple-k
                                                        $seq16
                                                        $loop17
                                                        k48)
                                                       ((lambda (k50)
                                                          ((cps py-list?)
                                                           $seq16
                                                           (lambda (rv51)
                                                             (if rv51
                                                               (for-py-list-k
                                                                $seq16
                                                                $loop17
                                                                k50)
                                                               ((lambda (k52)
                                                                  ((cps dict?)
                                                                   $seq16
                                                                   (lambda (rv53)
                                                                     (if rv53
                                                                       (for-dict-k
                                                                        $seq16
                                                                        $loop17
                                                                        k52)
                                                                       (k52
                                                                        (void))))))
                                                                k50)))))
                                                        k48)))))
                                                (lambda (rv46)
                                                  (k45 (void))))))))
                                        rv54
                                        (lambda (i18 k55)
                                          ((lambda (f cc)
                                             (f (lambda (x k) (cc x)) cc))
                                           (lambda (continue k56)
                                             (set-then!
                                              g$z
                                              i18
                                              ((lambda (k57)
                                                 ((cps py-print)
                                                  g$x
                                                  (lambda (rv58)
                                                    ((cps py-print)
                                                     g$y
                                                     (lambda (rv59)
                                                       ((cps py-print)
                                                        g$z
                                                        k57))))))
                                               k56)))
                                           k55))
                                        k44))
                                     (py-list* 3)))
                                  k43))
                               k42)))
                           k41))
                        k30))
                     (py-list* 2)))
                  k29))
               k28)))
           k27))
        k16))
     (py-list* 1)))
  $halt))
