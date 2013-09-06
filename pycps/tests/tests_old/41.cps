(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (if #t
   ((cps py-print)
    "bob"
    (lambda (rv16)
      (if #f
        ((cps py-print)
         5
         (lambda (rv17)
           ((lambda (k18)
              ((cps <)
               3
               343151
               (lambda (rv19)
                 (if rv19 ((cps py-print) "why?" k18) (k18 (void))))))
            $halt)))
        ((lambda (rv17)
           ((lambda (k18)
              ((cps <)
               3
               343151
               (lambda (rv19)
                 (if rv19 ((cps py-print) "why?" k18) (k18 (void))))))
            $halt))
         (void)))))
   ((lambda (rv16)
      (if #f
        ((cps py-print)
         5
         (lambda (rv17)
           ((lambda (k18)
              ((cps <)
               3
               343151
               (lambda (rv19)
                 (if rv19 ((cps py-print) "why?" k18) (k18 (void))))))
            $halt)))
        ((lambda (rv17)
           ((lambda (k18)
              ((cps <)
               3
               343151
               (lambda (rv19)
                 (if rv19 ((cps py-print) "why?" k18) (k18 (void))))))
            $halt))
         (void))))
    (void))))
