(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (define g$y (void))
 (define g$a (void))
 (set-then!
  g$a
  (lambda (b k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17)
       ((lambda (k18)
          ((cps *)
           b
           2
           (lambda (rv20) ((lambda (rv19) (return rv19 k18)) (tuple b rv20)))))
        k17))
     k16))
  (g$a
   3
   (lambda (rv42)
     ((lambda (t16 k23)
        ((lambda (e18 k34)
           ((lambda (i17 k35)
              ((lambda (k36)
                 ((cps py-list?)
                  e18
                  (lambda (rv37)
                    (if rv37
                      ((cps py-list-ref) e18 i17 k36)
                      ((lambda (k38)
                         ((cps tuple?)
                          e18
                          (lambda (rv39)
                            (if rv39
                              ((cps tuple-ref) e18 i17 k38)
                              ((lambda (k40)
                                 ((cps dict?)
                                  e18
                                  (lambda (rv41)
                                    (if rv41
                                      ((cps dict-ref) e18 i17 k40)
                                      (error "cannot index object" k40)))))
                               k38)))))
                       k36)))))
               k35))
            0
            k34))
         t16
         (lambda (rv24)
           (set-then!
            g$x
            rv24
            ((lambda (e20 k26)
               ((lambda (i19 k27)
                  ((lambda (k28)
                     ((cps py-list?)
                      e20
                      (lambda (rv29)
                        (if rv29
                          ((cps py-list-ref) e20 i19 k28)
                          ((lambda (k30)
                             ((cps tuple?)
                              e20
                              (lambda (rv31)
                                (if rv31
                                  ((cps tuple-ref) e20 i19 k30)
                                  ((lambda (k32)
                                     ((cps dict?)
                                      e20
                                      (lambda (rv33)
                                        (if rv33
                                          ((cps dict-ref) e20 i19 k32)
                                          (error "cannot index object" k32)))))
                                   k30)))))
                           k28)))))
                   k27))
                1
                k26))
             t16
             (lambda (rv25) (set-then! g$y rv25 (k23 (void)))))))))
      rv42
      (lambda (rv21)
        ((cps py-print) g$x (lambda (rv22) ((cps py-print) g$y $halt)))))))))
