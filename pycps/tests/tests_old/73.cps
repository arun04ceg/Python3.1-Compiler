(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$ii (void))
 (define g$fncs (void))
 (define g$identity2 (void))
 (define g$identity (void))
 (set-then!
  g$identity
  (lambda (x k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17) ((lambda (k18) (return x k18)) k17))
     k16))
  (set-then!
   g$identity2
   (lambda (x k19)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k20)
        ((lambda (k21) ((cps *) 2 x (lambda (rv22) (return rv22 k21)))) k20))
      k19))
   ((lambda (rv23)
      (set-then!
       g$fncs
       rv23
       ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
        (lambda (break k24)
          ((lambda (rv34)
             ((lambda ($seq16 $loop17 k25)
                ((cps set?)
                 $seq16
                 (lambda (rv27)
                   (if rv27
                     (for-set-k $seq16 $loop17 (lambda (rv26) (k25 (void))))
                     ((lambda (k28)
                        ((cps tuple?)
                         $seq16
                         (lambda (rv29)
                           (if rv29
                             (for-tuple-k $seq16 $loop17 k28)
                             ((lambda (k30)
                                ((cps py-list?)
                                 $seq16
                                 (lambda (rv31)
                                   (if rv31
                                     (for-py-list-k $seq16 $loop17 k30)
                                     ((lambda (k32)
                                        ((cps dict?)
                                         $seq16
                                         (lambda (rv33)
                                           (if rv33
                                             (for-dict-k $seq16 $loop17 k32)
                                             (k32 (void))))))
                                      k30)))))
                              k28)))))
                      (lambda (rv26) (k25 (void))))))))
              rv34
              (lambda (i16 k35)
                ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                 (lambda (continue k36)
                   (set-then!
                    g$ii
                    i16
                    ((lambda (k37)
                       (g$identity
                        g$ii
                        (lambda (rv40)
                          ((cps py-print)
                           rv40
                           (lambda (rv38)
                             (g$identity2
                              g$ii
                              (lambda (rv39) ((cps py-print) rv39 k37))))))))
                     k36)))
                 k35))
              k24))
           (py-list* 1 2 3 4 5)))
        $halt)))
    (py-list* g$identity g$identity2)))))
