(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$a (void))
 (define g$identity (void))
 (set-then!
  g$identity
  (lambda (x k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17) ((lambda (k18) (return x k18)) k17))
     k16))
  (set-then!
   g$a
   (dict (3 5))
   ((cps py-print)
    g$a
    (lambda (rv19)
      (g$identity
       4
       (lambda (rv28)
         (g$identity
          rv28
          (lambda (rv26)
            (g$identity
             1
             (lambda (rv27)
               ((lambda (rv20)
                  (set-then!
                   g$a
                   rv20
                   ((cps py-print)
                    g$a
                    (lambda (rv21)
                      (g$identity
                       4
                       (lambda (rv25)
                         (g$identity
                          rv25
                          (lambda (rv23)
                            (g$identity
                             1
                             (lambda (rv24)
                               ((lambda (rv22)
                                  (set-then!
                                   g$a
                                   rv22
                                   ((cps py-print) g$a $halt)))
                                (tuple rv23 rv24 3 5))))))))))))
                (py-list* rv26 rv27 3 5)))))))))))))
