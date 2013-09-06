(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$nested_loop (void))
 (set-then!
  g$nested_loop
  (lambda (var k16)
    ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
     (lambda (return k17)
       ((lambda (j i k18)
          ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
           (lambda (break k19)
             ((lambda (rv29)
                ((lambda ($seq16 $loop17 k20)
                   ((cps set?)
                    $seq16
                    (lambda (rv22)
                      (if rv22
                        (for-set-k $seq16 $loop17 (lambda (rv21) (k20 (void))))
                        ((lambda (k23)
                           ((cps tuple?)
                            $seq16
                            (lambda (rv24)
                              (if rv24
                                (for-tuple-k $seq16 $loop17 k23)
                                ((lambda (k25)
                                   ((cps py-list?)
                                    $seq16
                                    (lambda (rv26)
                                      (if rv26
                                        (for-py-list-k $seq16 $loop17 k25)
                                        ((lambda (k27)
                                           ((cps dict?)
                                            $seq16
                                            (lambda (rv28)
                                              (if rv28
                                                (for-dict-k $seq16 $loop17 k27)
                                                (k27 (void))))))
                                         k25)))))
                                 k23)))))
                         (lambda (rv21) (k20 (void))))))))
                 rv29
                 (lambda (i16 k30)
                   ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
                    (lambda (continue k31)
                      (set-then!
                       i
                       i16
                       ((lambda (k32)
                          ((cps py-print)
                           i
                           (lambda (rv33)
                             ((lambda ($old-handler k43)
                                ((lambda ($old-return k44)
                                   ((lambda ($old-continue k45)
                                      ((lambda ($old-break k46)
                                         ((lambda (return k47)
                                            ((lambda (continue k48)
                                               ((lambda (break k49)
                                                  ((lambda (f cc)
                                                     (f
                                                      (lambda (x k) (cc x))
                                                      cc))
                                                   (lambda ($ec17 k50)
                                                     (set-then!
                                                      $current-handler
                                                      (lambda ($ex16 k51)
                                                        (set-then!
                                                         $current-handler
                                                         $old-handler
                                                         ((lambda (ex k53)
                                                            ((lambda (k54)
                                                               ((cps py-print)
                                                                "caught exception"
                                                                k54))
                                                             k53))
                                                          $ex16
                                                          (lambda (rv52)
                                                            ($ec17
                                                             rv52
                                                             k51)))))
                                                      ((lambda (k57)
                                                         ((lambda (f cc)
                                                            (f
                                                             (lambda (x k)
                                                               (cc x))
                                                             cc))
                                                          (lambda (break k60)
                                                            ((lambda (rv70)
                                                               ((lambda ($seq16
                                                                         $loop17
                                                                         k61)
                                                                  ((cps set?)
                                                                   $seq16
                                                                   (lambda (rv63)
                                                                     (if rv63
                                                                       (for-set-k
                                                                        $seq16
                                                                        $loop17
                                                                        (lambda (rv62)
                                                                          (k61
                                                                           (void))))
                                                                       ((lambda (k64)
                                                                          ((cps
                                                                            tuple?)
                                                                           $seq16
                                                                           (lambda (rv65)
                                                                             (if rv65
                                                                               (for-tuple-k
                                                                                $seq16
                                                                                $loop17
                                                                                k64)
                                                                               ((lambda (k66)
                                                                                  ((cps
                                                                                    py-list?)
                                                                                   $seq16
                                                                                   (lambda (rv67)
                                                                                     (if rv67
                                                                                       (for-py-list-k
                                                                                        $seq16
                                                                                        $loop17
                                                                                        k66)
                                                                                       ((lambda (k68)
                                                                                          ((cps
                                                                                            dict?)
                                                                                           $seq16
                                                                                           (lambda (rv69)
                                                                                             (if rv69
                                                                                               (for-dict-k
                                                                                                $seq16
                                                                                                $loop17
                                                                                                k68)
                                                                                               (k68
                                                                                                (void))))))
                                                                                        k66)))))
                                                                                k64)))))
                                                                        (lambda (rv62)
                                                                          (k61
                                                                           (void))))))))
                                                                rv70
                                                                (lambda (i17
                                                                         k71)
                                                                  ((lambda (f
                                                                            cc)
                                                                     (f
                                                                      (lambda (x
                                                                               k)
                                                                        (cc x))
                                                                      cc))
                                                                   (lambda (continue
                                                                            k72)
                                                                     (set-then!
                                                                      j
                                                                      i17
                                                                      ((lambda (k73)
                                                                         ((cps
                                                                           py-print)
                                                                          "begin inner"
                                                                          (lambda (rv74)
                                                                            ((cps
                                                                              py-print)
                                                                             i
                                                                             (lambda (rv75)
                                                                               ((cps
                                                                                 py-print)
                                                                                j
                                                                                (lambda (rv76)
                                                                                  ((cps
                                                                                    >)
                                                                                   i
                                                                                   4
                                                                                   (lambda (rv94)
                                                                                     (if rv94
                                                                                       ((cps
                                                                                         equal?)
                                                                                        j
                                                                                        4
                                                                                        (lambda (rv78)
                                                                                          (if rv78
                                                                                            ((lambda (k79)
                                                                                               ((cps
                                                                                                 py-print)
                                                                                                "raise"
                                                                                                (lambda (rv80)
                                                                                                  ($current-handler
                                                                                                   "reached 6"
                                                                                                   k79))))
                                                                                             (lambda (rv77)
                                                                                               ((cps
                                                                                                 py-print)
                                                                                                "end inner"
                                                                                                k73)))
                                                                                            ((lambda (k81)
                                                                                               ((cps
                                                                                                 <)
                                                                                                i
                                                                                                2
                                                                                                (lambda (rv93)
                                                                                                  (if rv93
                                                                                                    ((cps
                                                                                                      >)
                                                                                                     j
                                                                                                     5
                                                                                                     (lambda (rv82)
                                                                                                       (if rv82
                                                                                                         ((lambda (k83)
                                                                                                            ((cps
                                                                                                              py-print)
                                                                                                             "inner break"
                                                                                                             (lambda (rv84)
                                                                                                               (break
                                                                                                                (void)
                                                                                                                k83))))
                                                                                                          k81)
                                                                                                         ((lambda (k85)
                                                                                                            ((lambda (cv18
                                                                                                                      k90)
                                                                                                               ((lambda (k91)
                                                                                                                  ((cps
                                                                                                                    <)
                                                                                                                   2
                                                                                                                   cv18
                                                                                                                   (lambda (rv92)
                                                                                                                     (if rv92
                                                                                                                       ((cps
                                                                                                                         <)
                                                                                                                        cv18
                                                                                                                        4
                                                                                                                        k91)
                                                                                                                       (k91
                                                                                                                        #f)))))
                                                                                                                k90))
                                                                                                             i
                                                                                                             (lambda (rv89)
                                                                                                               (if rv89
                                                                                                                 ((cps
                                                                                                                   equal?)
                                                                                                                  j
                                                                                                                  4
                                                                                                                  (lambda (rv86)
                                                                                                                    (if rv86
                                                                                                                      ((lambda (k87)
                                                                                                                         ((cps
                                                                                                                           py-print)
                                                                                                                          "inner continue"
                                                                                                                          (lambda (rv88)
                                                                                                                            (continue
                                                                                                                             (void)
                                                                                                                             k87))))
                                                                                                                       k85)
                                                                                                                      (k85
                                                                                                                       (void)))))
                                                                                                                 ((lambda (rv86)
                                                                                                                    (if rv86
                                                                                                                      ((lambda (k87)
                                                                                                                         ((cps
                                                                                                                           py-print)
                                                                                                                          "inner continue"
                                                                                                                          (lambda (rv88)
                                                                                                                            (continue
                                                                                                                             (void)
                                                                                                                             k87))))
                                                                                                                       k85)
                                                                                                                      (k85
                                                                                                                       (void))))
                                                                                                                  #f)))))
                                                                                                          k81))))
                                                                                                    ((lambda (rv82)
                                                                                                       (if rv82
                                                                                                         ((lambda (k83)
                                                                                                            ((cps
                                                                                                              py-print)
                                                                                                             "inner break"
                                                                                                             (lambda (rv84)
                                                                                                               (break
                                                                                                                (void)
                                                                                                                k83))))
                                                                                                          k81)
                                                                                                         ((lambda (k85)
                                                                                                            ((lambda (cv18
                                                                                                                      k90)
                                                                                                               ((lambda (k91)
                                                                                                                  ((cps
                                                                                                                    <)
                                                                                                                   2
                                                                                                                   cv18
                                                                                                                   (lambda (rv92)
                                                                                                                     (if rv92
                                                                                                                       ((cps
                                                                                                                         <)
                                                                                                                        cv18
                                                                                                                        4
                                                                                                                        k91)
                                                                                                                       (k91
                                                                                                                        #f)))))
                                                                                                                k90))
                                                                                                             i
                                                                                                             (lambda (rv89)
                                                                                                               (if rv89
                                                                                                                 ((cps
                                                                                                                   equal?)
                                                                                                                  j
                                                                                                                  4
                                                                                                                  (lambda (rv86)
                                                                                                                    (if rv86
                                                                                                                      ((lambda (k87)
                                                                                                                         ((cps
                                                                                                                           py-print)
                                                                                                                          "inner continue"
                                                                                                                          (lambda (rv88)
                                                                                                                            (continue
                                                                                                                             (void)
                                                                                                                             k87))))
                                                                                                                       k85)
                                                                                                                      (k85
                                                                                                                       (void)))))
                                                                                                                 ((lambda (rv86)
                                                                                                                    (if rv86
                                                                                                                      ((lambda (k87)
                                                                                                                         ((cps
                                                                                                                           py-print)
                                                                                                                          "inner continue"
                                                                                                                          (lambda (rv88)
                                                                                                                            (continue
                                                                                                                             (void)
                                                                                                                             k87))))
                                                                                                                       k85)
                                                                                                                      (k85
                                                                                                                       (void))))
                                                                                                                  #f)))))
                                                                                                          k81)))
                                                                                                     #f)))))
                                                                                             (lambda (rv77)
                                                                                               ((cps
                                                                                                 py-print)
                                                                                                "end inner"
                                                                                                k73))))))
                                                                                       ((lambda (rv78)
                                                                                          (if rv78
                                                                                            ((lambda (k79)
                                                                                               ((cps
                                                                                                 py-print)
                                                                                                "raise"
                                                                                                (lambda (rv80)
                                                                                                  ($current-handler
                                                                                                   "reached 6"
                                                                                                   k79))))
                                                                                             (lambda (rv77)
                                                                                               ((cps
                                                                                                 py-print)
                                                                                                "end inner"
                                                                                                k73)))
                                                                                            ((lambda (k81)
                                                                                               ((cps
                                                                                                 <)
                                                                                                i
                                                                                                2
                                                                                                (lambda (rv93)
                                                                                                  (if rv93
                                                                                                    ((cps
                                                                                                      >)
                                                                                                     j
                                                                                                     5
                                                                                                     (lambda (rv82)
                                                                                                       (if rv82
                                                                                                         ((lambda (k83)
                                                                                                            ((cps
                                                                                                              py-print)
                                                                                                             "inner break"
                                                                                                             (lambda (rv84)
                                                                                                               (break
                                                                                                                (void)
                                                                                                                k83))))
                                                                                                          k81)
                                                                                                         ((lambda (k85)
                                                                                                            ((lambda (cv18
                                                                                                                      k90)
                                                                                                               ((lambda (k91)
                                                                                                                  ((cps
                                                                                                                    <)
                                                                                                                   2
                                                                                                                   cv18
                                                                                                                   (lambda (rv92)
                                                                                                                     (if rv92
                                                                                                                       ((cps
                                                                                                                         <)
                                                                                                                        cv18
                                                                                                                        4
                                                                                                                        k91)
                                                                                                                       (k91
                                                                                                                        #f)))))
                                                                                                                k90))
                                                                                                             i
                                                                                                             (lambda (rv89)
                                                                                                               (if rv89
                                                                                                                 ((cps
                                                                                                                   equal?)
                                                                                                                  j
                                                                                                                  4
                                                                                                                  (lambda (rv86)
                                                                                                                    (if rv86
                                                                                                                      ((lambda (k87)
                                                                                                                         ((cps
                                                                                                                           py-print)
                                                                                                                          "inner continue"
                                                                                                                          (lambda (rv88)
                                                                                                                            (continue
                                                                                                                             (void)
                                                                                                                             k87))))
                                                                                                                       k85)
                                                                                                                      (k85
                                                                                                                       (void)))))
                                                                                                                 ((lambda (rv86)
                                                                                                                    (if rv86
                                                                                                                      ((lambda (k87)
                                                                                                                         ((cps
                                                                                                                           py-print)
                                                                                                                          "inner continue"
                                                                                                                          (lambda (rv88)
                                                                                                                            (continue
                                                                                                                             (void)
                                                                                                                             k87))))
                                                                                                                       k85)
                                                                                                                      (k85
                                                                                                                       (void))))
                                                                                                                  #f)))))
                                                                                                          k81))))
                                                                                                    ((lambda (rv82)
                                                                                                       (if rv82
                                                                                                         ((lambda (k83)
                                                                                                            ((cps
                                                                                                              py-print)
                                                                                                             "inner break"
                                                                                                             (lambda (rv84)
                                                                                                               (break
                                                                                                                (void)
                                                                                                                k83))))
                                                                                                          k81)
                                                                                                         ((lambda (k85)
                                                                                                            ((lambda (cv18
                                                                                                                      k90)
                                                                                                               ((lambda (k91)
                                                                                                                  ((cps
                                                                                                                    <)
                                                                                                                   2
                                                                                                                   cv18
                                                                                                                   (lambda (rv92)
                                                                                                                     (if rv92
                                                                                                                       ((cps
                                                                                                                         <)
                                                                                                                        cv18
                                                                                                                        4
                                                                                                                        k91)
                                                                                                                       (k91
                                                                                                                        #f)))))
                                                                                                                k90))
                                                                                                             i
                                                                                                             (lambda (rv89)
                                                                                                               (if rv89
                                                                                                                 ((cps
                                                                                                                   equal?)
                                                                                                                  j
                                                                                                                  4
                                                                                                                  (lambda (rv86)
                                                                                                                    (if rv86
                                                                                                                      ((lambda (k87)
                                                                                                                         ((cps
                                                                                                                           py-print)
                                                                                                                          "inner continue"
                                                                                                                          (lambda (rv88)
                                                                                                                            (continue
                                                                                                                             (void)
                                                                                                                             k87))))
                                                                                                                       k85)
                                                                                                                      (k85
                                                                                                                       (void)))))
                                                                                                                 ((lambda (rv86)
                                                                                                                    (if rv86
                                                                                                                      ((lambda (k87)
                                                                                                                         ((cps
                                                                                                                           py-print)
                                                                                                                          "inner continue"
                                                                                                                          (lambda (rv88)
                                                                                                                            (continue
                                                                                                                             (void)
                                                                                                                             k87))))
                                                                                                                       k85)
                                                                                                                      (k85
                                                                                                                       (void))))
                                                                                                                  #f)))))
                                                                                                          k81)))
                                                                                                     #f)))))
                                                                                             (lambda (rv77)
                                                                                               ((cps
                                                                                                 py-print)
                                                                                                "end inner"
                                                                                                k73)))))
                                                                                        #f)))))))))))
                                                                       k72)))
                                                                   k71))
                                                                k60))
                                                             (tuple 7 6 5 4)))
                                                          (lambda (rv58)
                                                            ((cps py-print)
                                                             "j after inner:"
                                                             (lambda (rv59)
                                                               ((cps py-print)
                                                                j
                                                                k57))))))
                                                       (lambda (rv56)
                                                         ((lambda (rv k55)
                                                            (set-then!
                                                             $current-handler
                                                             $old-handler
                                                             (k55 rv)))
                                                          rv56
                                                          k50)))))
                                                   k49))
                                                (lambda (k95)
                                                  (set-then!
                                                   $current-handler
                                                   $old-handler
                                                   ($old-break (void) k95)))
                                                k48))
                                             (lambda (k96)
                                               (set-then!
                                                $current-handler
                                                $old-handler
                                                ($old-continue (void) k96)))
                                             k47))
                                          (lambda (rv k97)
                                            (set-then!
                                             $current-handler
                                             $old-handler
                                             (return rv k97)))
                                          k46))
                                       break
                                       k45))
                                    continue
                                    k44))
                                 return
                                 k43))
                              $current-handler
                              (lambda (rv34)
                                ((cps equal?)
                                 i
                                 3
                                 (lambda (rv36)
                                   (if rv36
                                     ((lambda (k37)
                                        ((cps py-print)
                                         "outer continue"
                                         (lambda (rv38)
                                           (continue (void) k37))))
                                      (lambda (rv35)
                                        ((cps py-print) "end outer" k32)))
                                     ((lambda (k39)
                                        ((cps equal?)
                                         i
                                         var
                                         (lambda (rv40)
                                           (if rv40
                                             ((lambda (k41)
                                                ((cps py-print)
                                                 "outer break"
                                                 (lambda (rv42)
                                                   (break (void) k41))))
                                              k39)
                                             (k39 (void))))))
                                      (lambda (rv35)
                                        ((cps py-print)
                                         "end outer"
                                         k32)))))))))))
                        k31)))
                    k30))
                 k19))
              (py-list* 1 2 3 4 5)))
           k18))
        (void)
        (void)
        k17))
     k16))
  ((cps -)
   1
   (lambda (rv99)
     (g$nested_loop rv99 (lambda (rv98) (g$nested_loop 1 $halt)))))))
