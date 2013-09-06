(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 ((lambda ($old-handler k16)
    ((lambda ($old-return k17)
       ((lambda ($old-continue k18)
          ((lambda ($old-break k19)
             ((lambda (return k20)
                ((lambda (continue k21)
                   ((lambda (break k22)
                      ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                       (lambda ($ec17 k23)
                         (set-then!
                          $current-handler
                          (lambda ($ex16 k24)
                            (set-then!
                             $current-handler
                             $old-handler
                             ((lambda (ex k26)
                                ((lambda (k27) ((cps py-print) 15 k27)) k26))
                              $ex16
                              (lambda (rv25) ($ec17 rv25 k24)))))
                          ((lambda (k30) ((cps py-print) 10 k30))
                           (lambda (rv29)
                             ((lambda (rv k28)
                                (set-then!
                                 $current-handler
                                 $old-handler
                                 (k28 rv)))
                              rv29
                              k23)))))
                       k22))
                    (lambda (k31)
                      (set-then!
                       $current-handler
                       $old-handler
                       ($old-break (void) k31)))
                    k21))
                 (lambda (k32)
                   (set-then!
                    $current-handler
                    $old-handler
                    ($old-continue (void) k32)))
                 k20))
              (lambda (rv k33)
                (set-then! $current-handler $old-handler (return rv k33)))
              k19))
           break
           k18))
        continue
        k17))
     return
     k16))
  $current-handler
  $halt))
