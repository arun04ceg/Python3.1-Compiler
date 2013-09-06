(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$b (void))
 (define g$a (void))
 (set-then!
  g$a
  1
  (set-then!
   g$b
   2
   ((lambda (k16)
      ((cps not-equal?)
       g$a
       g$b
       (lambda (rv17)
         (if rv17
           ((lambda (k18) ((cps py-print) "Not equal" k18)) k16)
           ((lambda (k19) ((cps py-print) "Equal" k19)) k16)))))
    $halt))))
