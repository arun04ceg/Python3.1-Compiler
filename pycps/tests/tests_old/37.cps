(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$c (void))
 (define g$a (void))
 (define g$q (void))
 (define g$e (void))
 (set-then!
  g$a
  3
  (set-then!
   g$b
   (lambda (c k16)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k17)
        ((lambda (k18) ((cps +) c 3 (lambda (rv19) ((cps py-print) rv19 k18))))
         k17))
      k16))
   (set-then!
    g$c
    (lambda (d k20)
      ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
       (lambda (return k21)
         ((cps +) d g$a (lambda (rv22) ((cps py-print) rv22 k21))))
       k20))
    (set-then!
     g$e
     (lambda (a b c k23)
       ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
        (lambda (return k24)
          ((cps +)
           a
           b
           (lambda (rv26)
             ((cps +) rv26 c (lambda (rv25) ((cps py-print) rv25 k24))))))
        k23))
     (set-then!
      g$q
      (lambda (b c k27)
        ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
         (lambda (return k28)
           ((lambda (k29)
              ((cps +)
               b
               c
               (lambda (rv31)
                 ((cps +)
                  rv31
                  g$a
                  (lambda (rv30) ((cps py-print) rv30 k29))))))
            k28))
         k27))
      (g$b
       4
       (lambda (rv32)
         (g$c
          1
          (lambda (rv33)
            (g$e 8 14 838 (lambda (rv34) (g$q 89 23 $halt)))))))))))))
