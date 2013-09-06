(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (set-then!
  g$x
  0
  ((lambda (k16)
     ((lambda (cv16 k23)
        ((lambda (k24)
           ((cps <)
            g$x
            cv16
            (lambda (rv25)
              (if rv25
                ((lambda (cv17 k26)
                   ((lambda (k27)
                      ((cps <)
                       cv16
                       cv17
                       (lambda (rv28)
                         (if rv28
                           ((lambda (cv18 k29)
                              ((lambda (k30)
                                 ((cps <)
                                  cv17
                                  cv18
                                  (lambda (rv31)
                                    (if rv31
                                      ((lambda (cv19 k32)
                                         ((lambda (k33)
                                            ((cps <)
                                             cv18
                                             cv19
                                             (lambda (rv34)
                                               (if rv34
                                                 ((cps <) cv19 5 k33)
                                                 (k33 #f)))))
                                          k32))
                                       4
                                       k30)
                                      (k30 #f)))))
                               k29))
                            3
                            k27)
                           (k27 #f)))))
                    k26))
                 2
                 k24)
                (k24 #f)))))
         k23))
      1
      (lambda (rv17)
        (if rv17
          ((lambda (k18) ((cps py-print) "Good!" k18)) k16)
          ((lambda (k19)
             ((cps <)
              g$x
              6
              (lambda (rv20)
                (if rv20
                  ((lambda (k21) ((cps py-print) "WTF?" k21)) k19)
                  ((lambda (k22) ((cps py-print) "Bad!" k22)) k19)))))
           k16)))))
   $halt)))
