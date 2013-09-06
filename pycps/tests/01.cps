(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$abs (void))
 (set-then!
  g$abs
  (lambda (x k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17)
       ((lambda (k18)
          ((lambda (k19)
             ((cps <)
              x
              0
              (lambda (rv20)
                (if rv20
                  ((lambda (k21) ((cps -) x (lambda (rv22) (return rv22 k21))))
                   k19)
                  ((lambda (k23) (return x k23)) k19)))))
           k18))
        k17))
     k16))
  ((cps -)
   1
   (lambda (rv28)
     ((cps -)
      1
      (lambda (rv29)
        ((cps *)
         rv28
         rv29
         (lambda (rv26)
           ((cps -)
            1
            (lambda (rv27)
              ((cps *)
               rv26
               rv27
               (lambda (rv25)
                 (g$abs
                  rv25
                  (lambda (rv24) ((cps py-print) rv24 $halt)))))))))))))))
