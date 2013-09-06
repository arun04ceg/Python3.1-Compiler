(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((cps bitwise-xor)
  3
  2
  (lambda (rv17)
    ((cps bitwise-xor)
     3
     3
     (lambda (rv22)
       ((cps bitwise-or)
        4
        rv22
        (lambda (rv20)
          ((cps bitwise-not)
           3
           (lambda (rv21)
             ((cps -)
              rv20
              rv21
              (lambda (rv19)
                ((cps bitwise-and)
                 5
                 rv19
                 (lambda (rv18)
                   ((cps bitwise-or)
                    rv17
                    rv18
                    (lambda (rv16) ((cps py-print) rv16 $halt))))))))))))))))
