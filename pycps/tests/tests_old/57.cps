(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$i (void))
 (define g$x (void))
 ((lambda (rv16)
    (set-then!
     g$x
     rv16
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (break k17)
        ((lambda ($seq16 $loop17 k18)
           ((cps set?)
            $seq16
            (lambda (rv20)
              (if rv20
                (for-set-k $seq16 $loop17 (lambda (rv19) (k18 (void))))
                ((lambda (k21)
                   ((cps tuple?)
                    $seq16
                    (lambda (rv22)
                      (if rv22
                        (for-tuple-k $seq16 $loop17 k21)
                        ((lambda (k23)
                           ((cps py-list?)
                            $seq16
                            (lambda (rv24)
                              (if rv24
                                (for-py-list-k $seq16 $loop17 k23)
                                ((lambda (k25)
                                   ((cps dict?)
                                    $seq16
                                    (lambda (rv26)
                                      (if rv26
                                        (for-dict-k $seq16 $loop17 k25)
                                        (k25 (void))))))
                                 k23)))))
                         k21)))))
                 (lambda (rv19) (k18 (void))))))))
         g$x
         (lambda (i16 k27)
           ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
            (lambda (continue k28)
              (set-then!
               g$i
               i16
               ((lambda (k29) ((cps py-print) g$i k29)) k28)))
            k27))
         k17))
      $halt)))
  (set 3 1 2 5)))
