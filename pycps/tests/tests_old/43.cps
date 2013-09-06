(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 ((lambda (rv16)
    (set-then!
     g$a
     rv16
     ((lambda (b17 k18)
        ((lambda (i16 k19)
           ((lambda (e20 k31)
              ((lambda (i19 k32)
                 ((lambda (k33)
                    ((cps py-list?)
                     e20
                     (lambda (rv34)
                       (if rv34
                         ((cps py-list-ref) e20 i19 k33)
                         ((lambda (k35)
                            ((cps tuple?)
                             e20
                             (lambda (rv36)
                               (if rv36
                                 ((cps tuple-ref) e20 i19 k35)
                                 ((lambda (k37)
                                    ((cps dict?)
                                     e20
                                     (lambda (rv38)
                                       (if rv38
                                         ((cps dict-ref) e20 i19 k37)
                                         (error "cannot index object" k37)))))
                                  k35)))))
                          k33)))))
                  k32))
               i16
               k31))
            b17
            (lambda (rv30)
              ((lambda (v18 k20)
                 ((lambda (k21)
                    ((cps tuple?)
                     b17
                     (lambda (rv22)
                       (if rv22
                         ((cps +)
                          v18
                          5
                          (lambda (rv23) ((cps tuple-set!) b17 i16 rv23 k21)))
                         ((lambda (k24)
                            ((cps py-list?)
                             b17
                             (lambda (rv25)
                               (if rv25
                                 ((cps +)
                                  v18
                                  5
                                  (lambda (rv26)
                                    ((cps py-list-set!) b17 i16 rv26 k24)))
                                 ((lambda (k27)
                                    ((cps dict?)
                                     b17
                                     (lambda (rv28)
                                       (if rv28
                                         ((cps +)
                                          v18
                                          5
                                          (lambda (rv29)
                                            ((cps dict-set!)
                                             b17
                                             i16
                                             rv29
                                             k27)))
                                         (k27 (void))))))
                                  k24)))))
                          k21)))))
                  k20))
               rv30
               k19))))
         0
         k18))
      g$a
      (lambda (rv17) ((cps py-print) g$a $halt)))))
  (py-list* 1 2)))
