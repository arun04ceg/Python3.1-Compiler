(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (set-then!
  g$a
  (dict (1 2))
  ((lambda (b17 k17)
     ((lambda (i16 k18)
        ((lambda (k19)
           ((cps tuple?)
            b17
            (lambda (rv20)
              (if rv20
                ((cps tuple-set!) b17 i16 3 k19)
                ((lambda (k21)
                   ((cps py-list?)
                    b17
                    (lambda (rv22)
                      (if rv22
                        ((cps py-list-set!) b17 i16 3 k21)
                        ((lambda (k23)
                           ((cps dict?)
                            b17
                            (lambda (rv24)
                              (if rv24
                                ((cps dict-set!) b17 i16 3 k23)
                                (k23 (void))))))
                         k21)))))
                 k19)))))
         k18))
      1
      k17))
   g$a
   (lambda (rv16) ((cps py-print) g$a $halt)))))
