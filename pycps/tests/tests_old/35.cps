(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$a (void))
 ((lambda (rv16)
    (set-then!
     g$a
     rv16
     (set-then!
      g$b
      (lambda (k17)
        ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
         (lambda (return k18)
           ((lambda (j i k19)
              (set-then!
               j
               0
               ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                (lambda (break k21)
                  ((lambda ($seq16 $loop17 k22)
                     ((cps set?)
                      $seq16
                      (lambda (rv24)
                        (if rv24
                          (for-set-k
                           $seq16
                           $loop17
                           (lambda (rv23) (k22 (void))))
                          ((lambda (k25)
                             ((cps tuple?)
                              $seq16
                              (lambda (rv26)
                                (if rv26
                                  (for-tuple-k $seq16 $loop17 k25)
                                  ((lambda (k27)
                                     ((cps py-list?)
                                      $seq16
                                      (lambda (rv28)
                                        (if rv28
                                          (for-py-list-k $seq16 $loop17 k27)
                                          ((lambda (k29)
                                             ((cps dict?)
                                              $seq16
                                              (lambda (rv30)
                                                (if rv30
                                                  (for-dict-k
                                                   $seq16
                                                   $loop17
                                                   k29)
                                                  (k29 (void))))))
                                           k27)))))
                                   k25)))))
                           (lambda (rv23) (k22 (void))))))))
                   g$a
                   (lambda (i16 k31)
                     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                      (lambda (continue k32)
                        (set-then!
                         i
                         i16
                         ((lambda (k33)
                            ((cps +)
                             j
                             i
                             (lambda (rv34) (set-then! j rv34 (k33 (void))))))
                          k32)))
                      k31))
                   k21))
                (lambda (rv20) ((cps py-print) j k19)))))
            (void)
            (void)
            k18))
         k17))
      (g$b (lambda (rv35) ((cps py-print) g$a $halt))))))
  (py-list* 1 2 3 4)))
