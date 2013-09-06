(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$i (void))
 (set-then!
  g$i
  0
  ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
   (lambda (break k28)
     ((lambda (loop k29)
        (set-then!
         loop
         (lambda (k30)
           ((lambda (k31)
              ((cps >)
               g$i
               150
               (lambda (rv32)
                 (if rv32
                   ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                    (lambda (continue k34) ((lambda (k35) (k35 (void))) k34))
                    (lambda (rv33) (loop k31)))
                   (k31 (void))))))
            k30))
         (loop (lambda (rv36) (k29 (void))))))
      (void)
      k28))
   (lambda (rv16)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (break k18)
        ((lambda (loop k19)
           (set-then!
            loop
            (lambda (k20)
              ((lambda (k21)
                 ((cps <)
                  g$i
                  150
                  (lambda (rv22)
                    (if rv22
                      ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                       (lambda (continue k24)
                         ((lambda (k25)
                            ((cps +)
                             g$i
                             1
                             (lambda (rv26)
                               (set-then! g$i rv26 (k25 (void))))))
                          k24))
                       (lambda (rv23) (loop k21)))
                      (k21 (void))))))
               k20))
            (loop (lambda (rv27) (k19 (void))))))
         (void)
         k18))
      (lambda (rv17) ((cps py-print) g$i $halt)))))))
