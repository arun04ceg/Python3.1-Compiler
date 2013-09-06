(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$g (void))
 (set-then!
  g$g
  200
  (set-then!
   g$f
   (lambda (k16)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k17)
        ((lambda (h x k18)
           (if #f
             ((lambda (k39) (k39 (void)))
              (lambda (rv19)
                ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                 (lambda (break k26)
                   ((lambda (rv36)
                      ((lambda ($seq16 $loop17 k27)
                         ((cps set?)
                          $seq16
                          (lambda (rv29)
                            (if rv29
                              (for-set-k
                               $seq16
                               $loop17
                               (lambda (rv28) (k27 (void))))
                              ((lambda (k30)
                                 ((cps tuple?)
                                  $seq16
                                  (lambda (rv31)
                                    (if rv31
                                      (for-tuple-k $seq16 $loop17 k30)
                                      ((lambda (k32)
                                         ((cps py-list?)
                                          $seq16
                                          (lambda (rv33)
                                            (if rv33
                                              (for-py-list-k
                                               $seq16
                                               $loop17
                                               k32)
                                              ((lambda (k34)
                                                 ((cps dict?)
                                                  $seq16
                                                  (lambda (rv35)
                                                    (if rv35
                                                      (for-dict-k
                                                       $seq16
                                                       $loop17
                                                       k34)
                                                      (k34 (void))))))
                                               k32)))))
                                       k30)))))
                               (lambda (rv28) (k27 (void))))))))
                       rv36
                       (lambda (i16 k37)
                         ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                          (lambda (continue k38)
                            (set-then! g$g i16 (k38 (void))))
                          k37))
                       k26))
                    (py-list* 1 2 3)))
                 (lambda (rv20)
                   (set-then!
                    x
                    314
                    (set-then!
                     h
                     (lambda (k21)
                       ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                        (lambda (return k22)
                          ((lambda (g k23)
                             (set-then! g x ((cps py-print) g k23)))
                           (void)
                           k22))
                        k21))
                     (h
                      (lambda (rv24)
                        ((cps py-print)
                         g$g
                         (lambda (rv25) (return g$g k18)))))))))))
             ((lambda (rv19)
                ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                 (lambda (break k26)
                   ((lambda (rv36)
                      ((lambda ($seq16 $loop17 k27)
                         ((cps set?)
                          $seq16
                          (lambda (rv29)
                            (if rv29
                              (for-set-k
                               $seq16
                               $loop17
                               (lambda (rv28) (k27 (void))))
                              ((lambda (k30)
                                 ((cps tuple?)
                                  $seq16
                                  (lambda (rv31)
                                    (if rv31
                                      (for-tuple-k $seq16 $loop17 k30)
                                      ((lambda (k32)
                                         ((cps py-list?)
                                          $seq16
                                          (lambda (rv33)
                                            (if rv33
                                              (for-py-list-k
                                               $seq16
                                               $loop17
                                               k32)
                                              ((lambda (k34)
                                                 ((cps dict?)
                                                  $seq16
                                                  (lambda (rv35)
                                                    (if rv35
                                                      (for-dict-k
                                                       $seq16
                                                       $loop17
                                                       k34)
                                                      (k34 (void))))))
                                               k32)))))
                                       k30)))))
                               (lambda (rv28) (k27 (void))))))))
                       rv36
                       (lambda (i16 k37)
                         ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                          (lambda (continue k38)
                            (set-then! g$g i16 (k38 (void))))
                          k37))
                       k26))
                    (py-list* 1 2 3)))
                 (lambda (rv20)
                   (set-then!
                    x
                    314
                    (set-then!
                     h
                     (lambda (k21)
                       ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                        (lambda (return k22)
                          ((lambda (g k23)
                             (set-then! g x ((cps py-print) g k23)))
                           (void)
                           k22))
                        k21))
                     (h
                      (lambda (rv24)
                        ((cps py-print)
                         g$g
                         (lambda (rv25) (return g$g k18))))))))))
              (void))))
         (void)
         (void)
         k17))
      k16))
   (g$f
    (lambda (rv41)
      ((cps py-print) rv41 (lambda (rv40) ((cps py-print) g$g $halt))))))))
