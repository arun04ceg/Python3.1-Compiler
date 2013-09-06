(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
  (lambda (break k16)
    ((lambda (loop k17)
       (set-then!
        loop
        (lambda (k18)
          ((lambda (k19)
             (if #f
               ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                (lambda (continue k21)
                  ((lambda (k22) ((cps py-print) 10 k22)) k21))
                (lambda (rv20) (loop k19)))
               (k19 (void))))
           k18))
        (loop (lambda (rv23) ((lambda (k24) ((cps py-print) 20 k24)) k17)))))
     (void)
     k16))
  $halt))
