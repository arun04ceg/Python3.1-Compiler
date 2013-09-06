(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (define g$g (void))
 (define g$x (void))
 (set-then!
  g$f
  (lambda (x k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17) ((lambda (k18) ((cps py-print) 20 k18)) k17))
     k16))
  (set-then!
   g$g
   (lambda (x k19)
     ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
      (lambda (return k20) ((lambda (k21) ((cps py-print) 30 k21)) k20))
      k19))
   (set-then!
    g$x
    #t
    ((lambda (k22)
       (if g$x
         ((lambda (k23) (g$f g$x (lambda (rv24) (g$g g$x k23)))) k22)
         (k22 (void))))
     $halt)))))
