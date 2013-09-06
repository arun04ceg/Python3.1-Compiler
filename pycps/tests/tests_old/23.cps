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
        ((lambda (c a k18)
           (set-then!
            a
            4
            (set-then!
             c
             (lambda (k19)
               ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                (lambda (return k20) ((lambda (k21) (return a k21)) k20))
                k19))
             (c (lambda (rv22) ((cps py-print) rv22 k18))))))
         (void)
         (void)
         k17))
      k16))
   (g$b $halt))))
