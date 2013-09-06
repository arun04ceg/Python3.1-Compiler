(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$a (void))
 (set-then!
  g$a
  3
  (set-then!
   g$b
   (lambda (k16)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k17)
        ((lambda (b k18)
           (set-then!
            g$a
            2
            (set-then!
             b
             (lambda (k19)
               ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                (lambda (return k20)
                  ((lambda (b a k21)
                     (set-then!
                      a
                      4
                      (set-then!
                       b
                       (lambda (k22)
                         ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                          (lambda (return k23)
                            ((lambda (b k24)
                               (set-then!
                                a
                                5
                                (set-then!
                                 b
                                 (lambda (k25)
                                   ((lambda (f cc)
                                      (f (lambda (x k) (cc x)) cc))
                                    (lambda (return k26)
                                      ((lambda (a k27)
                                         (set-then!
                                          a
                                          6
                                          ((cps py-print) a k27)))
                                       (void)
                                       k26))
                                    k25))
                                 ((cps py-print)
                                  a
                                  (lambda (rv28)
                                    (b
                                     (lambda (rv29)
                                       ((cps py-print) a k24))))))))
                             (void)
                             k23))
                          k22))
                       ((cps py-print)
                        a
                        (lambda (rv30)
                          (b (lambda (rv31) ((cps py-print) a k21))))))))
                   (void)
                   (void)
                   k20))
                k19))
             ((cps py-print)
              g$a
              (lambda (rv32) (b (lambda (rv33) ((cps py-print) g$a k18))))))))
         (void)
         k17))
      k16))
   ((cps py-print)
    g$a
    (lambda (rv34) (g$b (lambda (rv35) ((cps py-print) g$a $halt))))))))
