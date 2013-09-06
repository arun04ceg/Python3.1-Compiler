(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$j (void))
 (define g$i (void))
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
              g$i
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
                              g$j
                              i17
                              ((lambda (k43)
                                 ((cps +)
                                  g$i
                                  g$j
                                  (lambda (rv44) ((cps py-print) rv44 k43))))
                               k42)))
                           k41))
                        k30))
                     (py-list* 10 21 541)))
                  k29))
               k28)))
           k27))
        k16))
     (py-list* 1 2 4 5)))
  $halt))
