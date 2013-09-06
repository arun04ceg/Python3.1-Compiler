(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$x (void))
 (define g$y (void))
 (define g$z (void))
 (set-then!
  g$x
  #f
  (set-then!
   g$y
   #f
   (set-then!
    g$z
    3
    ((lambda (k16)
       (if g$x
         ((lambda (k17) ((cps py-print) g$x k17)) k16)
         ((lambda (k18)
            (if g$y
              ((lambda (k19) ((cps py-print) g$y k19)) k18)
              ((lambda (k20) ((cps py-print) g$z k20)) k18)))
          k16)))
     $halt)))))
