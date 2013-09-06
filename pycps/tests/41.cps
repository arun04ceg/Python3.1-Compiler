(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$f (void))
 (set-then!
  g$f
  (lambda (k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17) ((lambda (k18) ($current-handler 3 k18)) k17))
     k16))
  ((lambda ($old-handler k19)
     ((lambda ($old-return k20)
        ((lambda ($old-continue k21)
           ((lambda ($old-break k22)
              ((lambda (return k23)
                 ((lambda (continue k24)
                    ((lambda (break k25)
                       ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                        (lambda ($ec17 k26)
                          (set-then!
                           $current-handler
                           (lambda ($ex16 k27)
                             (set-then!
                              $current-handler
                              $old-handler
                              ((lambda (ex k29)
                                 ((lambda (k30) ((cps py-print) 20 k30)) k29))
                               $ex16
                               (lambda (rv28) ($ec17 rv28 k27)))))
                           ((lambda (k33)
                              (g$f
                               (lambda (rv34) ((cps py-print) "fail" k33))))
                            (lambda (rv32)
                              ((lambda (rv k31)
                                 (set-then!
                                  $current-handler
                                  $old-handler
                                  (k31 rv)))
                               rv32
                               k26)))))
                        k25))
                     (lambda (k35)
                       (set-then!
                        $current-handler
                        $old-handler
                        ($old-break (void) k35)))
                     k24))
                  (lambda (k36)
                    (set-then!
                     $current-handler
                     $old-handler
                     ($old-continue (void) k36)))
                  k23))
               (lambda (rv k37)
                 (set-then! $current-handler $old-handler (return rv k37)))
               k22))
            break
            k21))
         continue
         k20))
      return
      k19))
   $current-handler
   $halt)))
