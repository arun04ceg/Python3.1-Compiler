(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((lambda (e17 k17)
    ((lambda (i16 k18)
       ((lambda (k19)
          ((cps py-list?)
           e17
           (lambda (rv20)
             (if rv20
               ((cps py-list-ref) e17 i16 k19)
               ((lambda (k21)
                  ((cps tuple?)
                   e17
                   (lambda (rv22)
                     (if rv22
                       ((cps tuple-ref) e17 i16 k21)
                       ((lambda (k23)
                          ((cps dict?)
                           e17
                           (lambda (rv24)
                             (if rv24
                               ((cps dict-ref) e17 i16 k23)
                               (error "cannot index object" k23)))))
                        k21)))))
                k19)))))
        k18))
     "x"
     k17))
  (dict ("x" 3) ("y" 4))
  (lambda (rv16) ((cps py-print) rv16 $halt))))
