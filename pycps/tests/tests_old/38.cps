(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$c (void))
 (define g$a (void))
 (define g$z (void))
 (define g$e (void))
 (define g$q (void))
 (set-then!
  g$a
  3
  (set-then!
   g$b
   (lambda (c k16)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k17)
        ((lambda (k18) ((cps +) c 3 (lambda (rv19) (return rv19 k18)))) k17))
      k16))
   (set-then!
    g$c
    (lambda (d k20)
      ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
       (lambda (return k21) ((cps +) d g$a (lambda (rv22) (return rv22 k21))))
       k20))
    (set-then!
     g$e
     (lambda (a b c k23)
       ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
        (lambda (return k24) (c a b (lambda (rv25) (return rv25 k24))))
        k23))
     (set-then!
      g$q
      (lambda (b c k26)
        ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
         (lambda (return k27)
           ((lambda (k28) (b c (lambda (rv29) (return rv29 k28)))) k27))
         k26))
      (set-then!
       g$z
       (lambda (c k30)
         ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
          (lambda (return k31)
            ((cps +)
             g$a
             c
             (lambda (rv33) (g$b rv33 (lambda (rv32) (return rv32 k31))))))
          k30))
       (g$b
        3
        (lambda (rv36)
          (g$c
           rv36
           (lambda (rv35)
             (g$e
              g$z
              rv35
              g$q
              (lambda (rv34) ((cps py-print) rv34 $halt))))))))))))))
