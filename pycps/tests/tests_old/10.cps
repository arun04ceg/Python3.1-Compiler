(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$g (void))
 (define g$a (void))
 (set-then!
  g$f
  (lambda (k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17)
       ((lambda (k18)
          ((cps py-print) "called f" (lambda (rv19) (return 1 k18))))
        k17))
     k16))
  (set-then!
   g$g
   (lambda (k20)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k21)
        ((lambda (k22)
           ((cps py-print) "called g" (lambda (rv23) (return 0 k22))))
         k21))
      k20))
   ((lambda (rv68)
      ((lambda (rv69)
         ((lambda (rv70)
            ((lambda (rv24)
               (set-then!
                g$a
                rv24
                ((lambda (e17 k59)
                   (g$f
                    (lambda (rv67)
                      ((lambda (i16 k60)
                         ((lambda (k61)
                            ((cps py-list?)
                             e17
                             (lambda (rv62)
                               (if rv62
                                 ((cps py-list-ref) e17 i16 k61)
                                 ((lambda (k63)
                                    ((cps tuple?)
                                     e17
                                     (lambda (rv64)
                                       (if rv64
                                         ((cps tuple-ref) e17 i16 k63)
                                         ((lambda (k65)
                                            ((cps dict?)
                                             e17
                                             (lambda (rv66)
                                               (if rv66
                                                 ((cps dict-ref) e17 i16 k65)
                                                 (error
                                                  "cannot index object"
                                                  k65)))))
                                          k63)))))
                                  k61)))))
                          k60))
                       rv67
                       k59))))
                 g$a
                 (lambda (rv58)
                   ((lambda (b19 k36)
                      (g$g
                       (lambda (rv57)
                         ((lambda (i18 k37)
                            ((lambda (e22 k49)
                               ((lambda (i21 k50)
                                  ((lambda (k51)
                                     ((cps py-list?)
                                      e22
                                      (lambda (rv52)
                                        (if rv52
                                          ((cps py-list-ref) e22 i21 k51)
                                          ((lambda (k53)
                                             ((cps tuple?)
                                              e22
                                              (lambda (rv54)
                                                (if rv54
                                                  ((cps tuple-ref) e22 i21 k53)
                                                  ((lambda (k55)
                                                     ((cps dict?)
                                                      e22
                                                      (lambda (rv56)
                                                        (if rv56
                                                          ((cps dict-ref)
                                                           e22
                                                           i21
                                                           k55)
                                                          (error
                                                           "cannot index object"
                                                           k55)))))
                                                   k53)))))
                                           k51)))))
                                   k50))
                                i18
                                k49))
                             b19
                             (lambda (rv48)
                               ((lambda (v20 k38)
                                  ((lambda (k39)
                                     ((cps tuple?)
                                      b19
                                      (lambda (rv40)
                                        (if rv40
                                          ((cps +)
                                           v20
                                           30
                                           (lambda (rv41)
                                             ((cps tuple-set!)
                                              b19
                                              i18
                                              rv41
                                              k39)))
                                          ((lambda (k42)
                                             ((cps py-list?)
                                              b19
                                              (lambda (rv43)
                                                (if rv43
                                                  ((cps +)
                                                   v20
                                                   30
                                                   (lambda (rv44)
                                                     ((cps py-list-set!)
                                                      b19
                                                      i18
                                                      rv44
                                                      k42)))
                                                  ((lambda (k45)
                                                     ((cps dict?)
                                                      b19
                                                      (lambda (rv46)
                                                        (if rv46
                                                          ((cps +)
                                                           v20
                                                           30
                                                           (lambda (rv47)
                                                             ((cps dict-set!)
                                                              b19
                                                              i18
                                                              rv47
                                                              k45)))
                                                          (k45 (void))))))
                                                   k42)))))
                                           k39)))))
                                   k38))
                                rv48
                                k37))))
                          rv57
                          k36))))
                    rv58
                    (lambda (rv25)
                      ((lambda (e24 k27)
                         (g$f
                          (lambda (rv35)
                            ((lambda (i23 k28)
                               ((lambda (k29)
                                  ((cps py-list?)
                                   e24
                                   (lambda (rv30)
                                     (if rv30
                                       ((cps py-list-ref) e24 i23 k29)
                                       ((lambda (k31)
                                          ((cps tuple?)
                                           e24
                                           (lambda (rv32)
                                             (if rv32
                                               ((cps tuple-ref) e24 i23 k31)
                                               ((lambda (k33)
                                                  ((cps dict?)
                                                   e24
                                                   (lambda (rv34)
                                                     (if rv34
                                                       ((cps dict-ref)
                                                        e24
                                                        i23
                                                        k33)
                                                       (error
                                                        "cannot index object"
                                                        k33)))))
                                                k31)))))
                                        k29)))))
                                k28))
                             rv35
                             k27))))
                       g$a
                       (lambda (rv26) ((cps py-print) rv26 $halt)))))))))
             (py-list* rv68 rv69 rv70)))
          (py-list* 50 60)))
       (py-list* 30 40)))
    (py-list* 10 20)))))
