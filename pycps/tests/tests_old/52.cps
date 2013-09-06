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
                                ((lambda (k27) ((cps py-print) "Good!" k27))
                                 k26))
                              $ex16
                              (lambda (rv25) ($ec17 rv25 k24)))))
                          ((lambda (k30)
                             ((lambda ($old-handler k31)
                                ((lambda ($old-return k32)
                                   ((lambda ($old-continue k33)
                                      ((lambda ($old-break k34)
                                         ((lambda (return k35)
                                            ((lambda (continue k36)
                                               ((lambda (break k37)
                                                  ((lambda (f cc)
                                                     (f
                                                      (lambda (x k) (cc x))
                                                      cc))
                                                   (lambda ($ec17 k38)
                                                     (set-then!
                                                      $current-handler
                                                      (lambda ($ex16 k39)
                                                        (set-then!
                                                         $current-handler
                                                         $old-handler
                                                         ((lambda (ex k41)
                                                            ((lambda (k42)
                                                               ($current-handler
                                                                "hi3"
                                                                k42))
                                                             k41))
                                                          $ex16
                                                          (lambda (rv40)
                                                            ($ec17
                                                             rv40
                                                             k39)))))
                                                      ((lambda (k45)
                                                         ((lambda ($old-handler
                                                                   k46)
                                                            ((lambda ($old-return
                                                                      k47)
                                                               ((lambda ($old-continue
                                                                         k48)
                                                                  ((lambda ($old-break
                                                                            k49)
                                                                     ((lambda (return
                                                                               k50)
                                                                        ((lambda (continue
                                                                                  k51)
                                                                           ((lambda (break
                                                                                     k52)
                                                                              ((lambda (f
                                                                                        cc)
                                                                                 (f
                                                                                  (lambda (x
                                                                                           k)
                                                                                    (cc
                                                                                     x))
                                                                                  cc))
                                                                               (lambda ($ec17
                                                                                        k53)
                                                                                 (set-then!
                                                                                  $current-handler
                                                                                  (lambda ($ex16
                                                                                           k54)
                                                                                    (set-then!
                                                                                     $current-handler
                                                                                     $old-handler
                                                                                     ((lambda (ex
                                                                                               k56)
                                                                                        ((lambda (k57)
                                                                                           ($current-handler
                                                                                            "hi2"
                                                                                            k57))
                                                                                         k56))
                                                                                      $ex16
                                                                                      (lambda (rv55)
                                                                                        ($ec17
                                                                                         rv55
                                                                                         k54)))))
                                                                                  ((lambda (k60)
                                                                                     ((cps
                                                                                       py-print)
                                                                                      "Begin..."
                                                                                      (lambda (rv61)
                                                                                        ($current-handler
                                                                                         "hi1"
                                                                                         k60))))
                                                                                   (lambda (rv59)
                                                                                     ((lambda (rv
                                                                                               k58)
                                                                                        (set-then!
                                                                                         $current-handler
                                                                                         $old-handler
                                                                                         (k58
                                                                                          rv)))
                                                                                      rv59
                                                                                      k53)))))
                                                                               k52))
                                                                            (lambda (k62)
                                                                              (set-then!
                                                                               $current-handler
                                                                               $old-handler
                                                                               ($old-break
                                                                                (void)
                                                                                k62)))
                                                                            k51))
                                                                         (lambda (k63)
                                                                           (set-then!
                                                                            $current-handler
                                                                            $old-handler
                                                                            ($old-continue
                                                                             (void)
                                                                             k63)))
                                                                         k50))
                                                                      (lambda (rv
                                                                               k64)
                                                                        (set-then!
                                                                         $current-handler
                                                                         $old-handler
                                                                         (return
                                                                          rv
                                                                          k64)))
                                                                      k49))
                                                                   break
                                                                   k48))
                                                                continue
                                                                k47))
                                                             return
                                                             k46))
                                                          $current-handler
                                                          k45))
                                                       (lambda (rv44)
                                                         ((lambda (rv k43)
                                                            (set-then!
                                                             $current-handler
                                                             $old-handler
                                                             (k43 rv)))
                                                          rv44
                                                          k38)))))
                                                   k37))
                                                (lambda (k65)
                                                  (set-then!
                                                   $current-handler
                                                   $old-handler
                                                   ($old-break (void) k65)))
                                                k36))
                                             (lambda (k66)
                                               (set-then!
                                                $current-handler
                                                $old-handler
                                                ($old-continue (void) k66)))
                                             k35))
                                          (lambda (rv k67)
                                            (set-then!
                                             $current-handler
                                             $old-handler
                                             (return rv k67)))
                                          k34))
                                       break
                                       k33))
                                    continue
                                    k32))
                                 return
                                 k31))
                              $current-handler
                              k30))
                           (lambda (rv29)
                             ((lambda (rv k28)
                                (set-then!
                                 $current-handler
                                 $old-handler
                                 (k28 rv)))
                              rv29
                              k23)))))
                       k22))
                    (lambda (k68)
                      (set-then!
                       $current-handler
                       $old-handler
                       ($old-break (void) k68)))
                    k21))
                 (lambda (k69)
                   (set-then!
                    $current-handler
                    $old-handler
                    ($old-continue (void) k69)))
                 k20))
              (lambda (rv k70)
                (set-then! $current-handler $old-handler (return rv k70)))
              k19))
           break
           k18))
        continue
        k17))
     return
     k16))
  $current-handler
  $halt))
