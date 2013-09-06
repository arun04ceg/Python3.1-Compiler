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
                                ((lambda (k27)
                                   ((cps py-print) "exception1" k27))
                                 k26))
                              $ex16
                              (lambda (rv25) ($ec17 rv25 k24)))))
                          ((lambda (k30)
                             ((cps py-print)
                              "try block1"
                              (lambda (rv31)
                                ((lambda ($old-handler k32)
                                   ((lambda ($old-return k33)
                                      ((lambda ($old-continue k34)
                                         ((lambda ($old-break k35)
                                            ((lambda (return k36)
                                               ((lambda (continue k37)
                                                  ((lambda (break k38)
                                                     ((lambda (f cc)
                                                        (f
                                                         (lambda (x k) (cc x))
                                                         cc))
                                                      (lambda ($ec17 k39)
                                                        (set-then!
                                                         $current-handler
                                                         (lambda ($ex16 k40)
                                                           (set-then!
                                                            $current-handler
                                                            $old-handler
                                                            ((lambda (ex k42)
                                                               ((lambda (k43)
                                                                  ((cps
                                                                    py-print)
                                                                   "exception2"
                                                                   k43))
                                                                k42))
                                                             $ex16
                                                             (lambda (rv41)
                                                               ($ec17
                                                                rv41
                                                                k40)))))
                                                         ((lambda (k46)
                                                            ((cps py-print)
                                                             "try block2"
                                                             k46))
                                                          (lambda (rv45)
                                                            ((lambda (rv k44)
                                                               (set-then!
                                                                $current-handler
                                                                $old-handler
                                                                (k44 rv)))
                                                             rv45
                                                             k39)))))
                                                      k38))
                                                   (lambda (k47)
                                                     (set-then!
                                                      $current-handler
                                                      $old-handler
                                                      ($old-break (void) k47)))
                                                   k37))
                                                (lambda (k48)
                                                  (set-then!
                                                   $current-handler
                                                   $old-handler
                                                   ($old-continue (void) k48)))
                                                k36))
                                             (lambda (rv k49)
                                               (set-then!
                                                $current-handler
                                                $old-handler
                                                (return rv k49)))
                                             k35))
                                          break
                                          k34))
                                       continue
                                       k33))
                                    return
                                    k32))
                                 $current-handler
                                 k30))))
                           (lambda (rv29)
                             ((lambda (rv k28)
                                (set-then!
                                 $current-handler
                                 $old-handler
                                 (k28 rv)))
                              rv29
                              k23)))))
                       k22))
                    (lambda (k50)
                      (set-then!
                       $current-handler
                       $old-handler
                       ($old-break (void) k50)))
                    k21))
                 (lambda (k51)
                   (set-then!
                    $current-handler
                    $old-handler
                    ($old-continue (void) k51)))
                 k20))
              (lambda (rv k52)
                (set-then! $current-handler $old-handler (return rv k52)))
              k19))
           break
           k18))
        continue
        k17))
     return
     k16))
  $current-handler
  $halt))
