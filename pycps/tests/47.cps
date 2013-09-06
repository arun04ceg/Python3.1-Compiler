(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$y (void))
 (set-then!
  g$y
  10
  (set-then!
   g$f
   (lambda (x k16)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k17)
        ((lambda (k18) (set-then! g$y 30 (k18 (void)))) k17))
      k16))
   ((cps py-print)
    g$y
    (lambda (rv19) (g$f 30 (lambda (rv20) ((cps py-print) g$y $halt))))))))
