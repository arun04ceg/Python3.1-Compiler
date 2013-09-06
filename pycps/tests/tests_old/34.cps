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
        ((lambda (a k18) (set-then! a 4 ((cps py-print) a k18))) (void) k17))
      k16))
   (g$b (lambda (rv19) ((cps py-print) g$a $halt))))))
