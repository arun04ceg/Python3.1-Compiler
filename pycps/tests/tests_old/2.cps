(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$fact (void))
 (set-then!
  g$fact
  (lambda (n k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17)
       ((lambda (k18)
          ((lambda (k19)
             ((cps <)
              n
              0
              (lambda (rv20)
                (if rv20
                  ((lambda (k21) (return #f k21)) k19)
                  ((lambda (k22)
                     ((cps equal?)
                      n
                      0
                      (lambda (rv23)
                        (if rv23
                          ((lambda (k24) (return 1 k24)) k22)
                          ((lambda (k25)
                             ((cps -)
                              n
                              1
                              (lambda (rv28)
                                (g$fact
                                 rv28
                                 (lambda (rv27)
                                   ((cps *)
                                    n
                                    rv27
                                    (lambda (rv26) (return rv26 k25))))))))
                           k22)))))
                   k19)))))
           k18))
        k17))
     k16))
  (g$fact 5 (lambda (rv29) ((cps py-print) rv29 $halt)))))
