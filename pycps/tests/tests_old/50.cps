(program
 (define break (void))
 (define return (void))
 (define continue (void))
 (define $current-handler (void))
 (define g$d (void))
 (define g$b (void))
 (define g$c (void))
 (define g$a (void))
 (define g$result (void))
 (define g$func (void))
 (set-then!
  g$a
  "a"
  ((lambda (rv16)
     (set-then!
      g$b
      rv16
      (set-then!
       g$c
       "old c"
       (set-then!
        g$d
        (dict ("a" (dict ("b" (py-list* 1)))))
        (set-then!
         g$func
         (lambda (a k17)
           ((lambda (f cc) (f (lambda (x k) (cc x)) cc))
            (lambda (return k18)
              ((lambda (e k19)
                 (set-then!
                  e
                  g$b
                  ((lambda (e17 k463)
                     ((lambda (i16 k464)
                        ((lambda (k465)
                           ((cps py-list?)
                            e17
                            (lambda (rv466)
                              (if rv466
                                ((cps py-list-ref) e17 i16 k465)
                                ((lambda (k467)
                                   ((cps tuple?)
                                    e17
                                    (lambda (rv468)
                                      (if rv468
                                        ((cps tuple-ref) e17 i16 k467)
                                        ((lambda (k469)
                                           ((cps dict?)
                                            e17
                                            (lambda (rv470)
                                              (if rv470
                                                ((cps dict-ref) e17 i16 k469)
                                                (error
                                                 "cannot index object"
                                                 k469)))))
                                         k467)))))
                                 k465)))))
                         k464))
                      0
                      k463))
                   e
                   (lambda (rv462)
                     ((lambda (e19 k454)
                        ((lambda (i18 k455)
                           ((lambda (k456)
                              ((cps py-list?)
                               e19
                               (lambda (rv457)
                                 (if rv457
                                   ((cps py-list-ref) e19 i18 k456)
                                   ((lambda (k458)
                                      ((cps tuple?)
                                       e19
                                       (lambda (rv459)
                                         (if rv459
                                           ((cps tuple-ref) e19 i18 k458)
                                           ((lambda (k460)
                                              ((cps dict?)
                                               e19
                                               (lambda (rv461)
                                                 (if rv461
                                                   ((cps dict-ref)
                                                    e19
                                                    i18
                                                    k460)
                                                   (error
                                                    "cannot index object"
                                                    k460)))))
                                            k458)))))
                                    k456)))))
                            k455))
                         "a"
                         k454))
                      rv462
                      (lambda (rv453)
                        ((lambda (e21 k445)
                           ((lambda (i20 k446)
                              ((lambda (k447)
                                 ((cps py-list?)
                                  e21
                                  (lambda (rv448)
                                    (if rv448
                                      ((cps py-list-ref) e21 i20 k447)
                                      ((lambda (k449)
                                         ((cps tuple?)
                                          e21
                                          (lambda (rv450)
                                            (if rv450
                                              ((cps tuple-ref) e21 i20 k449)
                                              ((lambda (k451)
                                                 ((cps dict?)
                                                  e21
                                                  (lambda (rv452)
                                                    (if rv452
                                                      ((cps dict-ref)
                                                       e21
                                                       i20
                                                       k451)
                                                      (error
                                                       "cannot index object"
                                                       k451)))))
                                               k449)))))
                                       k447)))))
                               k446))
                            0
                            k445))
                         rv453
                         (lambda (rv444)
                           ((lambda (e23 k436)
                              ((lambda (i22 k437)
                                 ((lambda (k438)
                                    ((cps py-list?)
                                     e23
                                     (lambda (rv439)
                                       (if rv439
                                         ((cps py-list-ref) e23 i22 k438)
                                         ((lambda (k440)
                                            ((cps tuple?)
                                             e23
                                             (lambda (rv441)
                                               (if rv441
                                                 ((cps tuple-ref) e23 i22 k440)
                                                 ((lambda (k442)
                                                    ((cps dict?)
                                                     e23
                                                     (lambda (rv443)
                                                       (if rv443
                                                         ((cps dict-ref)
                                                          e23
                                                          i22
                                                          k442)
                                                         (error
                                                          "cannot index object"
                                                          k442)))))
                                                  k440)))))
                                          k438)))))
                                  k437))
                               "b"
                               k436))
                            rv444
                            (lambda (rv435)
                              ((lambda (e25 k427)
                                 ((lambda (i24 k428)
                                    ((lambda (k429)
                                       ((cps py-list?)
                                        e25
                                        (lambda (rv430)
                                          (if rv430
                                            ((cps py-list-ref) e25 i24 k429)
                                            ((lambda (k431)
                                               ((cps tuple?)
                                                e25
                                                (lambda (rv432)
                                                  (if rv432
                                                    ((cps tuple-ref)
                                                     e25
                                                     i24
                                                     k431)
                                                    ((lambda (k433)
                                                       ((cps dict?)
                                                        e25
                                                        (lambda (rv434)
                                                          (if rv434
                                                            ((cps dict-ref)
                                                             e25
                                                             i24
                                                             k433)
                                                            (error
                                                             "cannot index object"
                                                             k433)))))
                                                     k431)))))
                                             k429)))))
                                     k428))
                                  0
                                  k427))
                               rv435
                               (lambda (rv426)
                                 ((lambda (e27 k418)
                                    ((lambda (i26 k419)
                                       ((lambda (k420)
                                          ((cps py-list?)
                                           e27
                                           (lambda (rv421)
                                             (if rv421
                                               ((cps py-list-ref) e27 i26 k420)
                                               ((lambda (k422)
                                                  ((cps tuple?)
                                                   e27
                                                   (lambda (rv423)
                                                     (if rv423
                                                       ((cps tuple-ref)
                                                        e27
                                                        i26
                                                        k422)
                                                       ((lambda (k424)
                                                          ((cps dict?)
                                                           e27
                                                           (lambda (rv425)
                                                             (if rv425
                                                               ((cps dict-ref)
                                                                e27
                                                                i26
                                                                k424)
                                                               (error
                                                                "cannot index object"
                                                                k424)))))
                                                        k422)))))
                                                k420)))))
                                        k419))
                                     "c"
                                     k418))
                                  rv426
                                  (lambda (rv417)
                                    ((lambda (e29 k409)
                                       ((lambda (i28 k410)
                                          ((lambda (k411)
                                             ((cps py-list?)
                                              e29
                                              (lambda (rv412)
                                                (if rv412
                                                  ((cps py-list-ref)
                                                   e29
                                                   i28
                                                   k411)
                                                  ((lambda (k413)
                                                     ((cps tuple?)
                                                      e29
                                                      (lambda (rv414)
                                                        (if rv414
                                                          ((cps tuple-ref)
                                                           e29
                                                           i28
                                                           k413)
                                                          ((lambda (k415)
                                                             ((cps dict?)
                                                              e29
                                                              (lambda (rv416)
                                                                (if rv416
                                                                  ((cps
                                                                    dict-ref)
                                                                   e29
                                                                   i28
                                                                   k415)
                                                                  (error
                                                                   "cannot index object"
                                                                   k415)))))
                                                           k413)))))
                                                   k411)))))
                                           k410))
                                        0
                                        k409))
                                     rv417
                                     (lambda (rv408)
                                       ((lambda (e31 k400)
                                          ((lambda (i30 k401)
                                             ((lambda (k402)
                                                ((cps py-list?)
                                                 e31
                                                 (lambda (rv403)
                                                   (if rv403
                                                     ((cps py-list-ref)
                                                      e31
                                                      i30
                                                      k402)
                                                     ((lambda (k404)
                                                        ((cps tuple?)
                                                         e31
                                                         (lambda (rv405)
                                                           (if rv405
                                                             ((cps tuple-ref)
                                                              e31
                                                              i30
                                                              k404)
                                                             ((lambda (k406)
                                                                ((cps dict?)
                                                                 e31
                                                                 (lambda (rv407)
                                                                   (if rv407
                                                                     ((cps
                                                                       dict-ref)
                                                                      e31
                                                                      i30
                                                                      k406)
                                                                     (error
                                                                      "cannot index object"
                                                                      k406)))))
                                                              k404)))))
                                                      k402)))))
                                              k401))
                                           "d"
                                           k400))
                                        rv408
                                        (lambda (rv399)
                                          ((lambda (e33 k391)
                                             ((lambda (i32 k392)
                                                ((lambda (k393)
                                                   ((cps py-list?)
                                                    e33
                                                    (lambda (rv394)
                                                      (if rv394
                                                        ((cps py-list-ref)
                                                         e33
                                                         i32
                                                         k393)
                                                        ((lambda (k395)
                                                           ((cps tuple?)
                                                            e33
                                                            (lambda (rv396)
                                                              (if rv396
                                                                ((cps
                                                                  tuple-ref)
                                                                 e33
                                                                 i32
                                                                 k395)
                                                                ((lambda (k397)
                                                                   ((cps dict?)
                                                                    e33
                                                                    (lambda (rv398)
                                                                      (if rv398
                                                                        ((cps
                                                                          dict-ref)
                                                                         e33
                                                                         i32
                                                                         k397)
                                                                        (error
                                                                         "cannot index object"
                                                                         k397)))))
                                                                 k395)))))
                                                         k393)))))
                                                 k392))
                                              0
                                              k391))
                                           rv399
                                           (lambda (rv390)
                                             ((lambda (e35 k382)
                                                ((lambda (i34 k383)
                                                   ((lambda (k384)
                                                      ((cps py-list?)
                                                       e35
                                                       (lambda (rv385)
                                                         (if rv385
                                                           ((cps py-list-ref)
                                                            e35
                                                            i34
                                                            k384)
                                                           ((lambda (k386)
                                                              ((cps tuple?)
                                                               e35
                                                               (lambda (rv387)
                                                                 (if rv387
                                                                   ((cps
                                                                     tuple-ref)
                                                                    e35
                                                                    i34
                                                                    k386)
                                                                   ((lambda (k388)
                                                                      ((cps
                                                                        dict?)
                                                                       e35
                                                                       (lambda (rv389)
                                                                         (if rv389
                                                                           ((cps
                                                                             dict-ref)
                                                                            e35
                                                                            i34
                                                                            k388)
                                                                           (error
                                                                            "cannot index object"
                                                                            k388)))))
                                                                    k386)))))
                                                            k384)))))
                                                    k383))
                                                 "e"
                                                 k382))
                                              rv390
                                              (lambda (rv381)
                                                ((lambda (e37 k373)
                                                   ((lambda (i36 k374)
                                                      ((lambda (k375)
                                                         ((cps py-list?)
                                                          e37
                                                          (lambda (rv376)
                                                            (if rv376
                                                              ((cps
                                                                py-list-ref)
                                                               e37
                                                               i36
                                                               k375)
                                                              ((lambda (k377)
                                                                 ((cps tuple?)
                                                                  e37
                                                                  (lambda (rv378)
                                                                    (if rv378
                                                                      ((cps
                                                                        tuple-ref)
                                                                       e37
                                                                       i36
                                                                       k377)
                                                                      ((lambda (k379)
                                                                         ((cps
                                                                           dict?)
                                                                          e37
                                                                          (lambda (rv380)
                                                                            (if rv380
                                                                              ((cps
                                                                                dict-ref)
                                                                               e37
                                                                               i36
                                                                               k379)
                                                                              (error
                                                                               "cannot index object"
                                                                               k379)))))
                                                                       k377)))))
                                                               k375)))))
                                                       k374))
                                                    1
                                                    k373))
                                                 rv381
                                                 (lambda (rv372)
                                                   ((cps py-print)
                                                    rv372
                                                    (lambda (rv20)
                                                      ((cps *)
                                                       3
                                                       100
                                                       (lambda (rv344)
                                                         ((lambda (e40 k364)
                                                            ((lambda (i39 k365)
                                                               ((lambda (k366)
                                                                  ((cps
                                                                    py-list?)
                                                                   e40
                                                                   (lambda (rv367)
                                                                     (if rv367
                                                                       ((cps
                                                                         py-list-ref)
                                                                        e40
                                                                        i39
                                                                        k366)
                                                                       ((lambda (k368)
                                                                          ((cps
                                                                            tuple?)
                                                                           e40
                                                                           (lambda (rv369)
                                                                             (if rv369
                                                                               ((cps
                                                                                 tuple-ref)
                                                                                e40
                                                                                i39
                                                                                k368)
                                                                               ((lambda (k370)
                                                                                  ((cps
                                                                                    dict?)
                                                                                   e40
                                                                                   (lambda (rv371)
                                                                                     (if rv371
                                                                                       ((cps
                                                                                         dict-ref)
                                                                                        e40
                                                                                        i39
                                                                                        k370)
                                                                                       (error
                                                                                        "cannot index object"
                                                                                        k370)))))
                                                                                k368)))))
                                                                        k366)))))
                                                                k365))
                                                             "a"
                                                             k364))
                                                          g$d
                                                          (lambda (rv363)
                                                            ((lambda (e42 k355)
                                                               ((lambda (i41
                                                                         k356)
                                                                  ((lambda (k357)
                                                                     ((cps
                                                                       py-list?)
                                                                      e42
                                                                      (lambda (rv358)
                                                                        (if rv358
                                                                          ((cps
                                                                            py-list-ref)
                                                                           e42
                                                                           i41
                                                                           k357)
                                                                          ((lambda (k359)
                                                                             ((cps
                                                                               tuple?)
                                                                              e42
                                                                              (lambda (rv360)
                                                                                (if rv360
                                                                                  ((cps
                                                                                    tuple-ref)
                                                                                   e42
                                                                                   i41
                                                                                   k359)
                                                                                  ((lambda (k361)
                                                                                     ((cps
                                                                                       dict?)
                                                                                      e42
                                                                                      (lambda (rv362)
                                                                                        (if rv362
                                                                                          ((cps
                                                                                            dict-ref)
                                                                                           e42
                                                                                           i41
                                                                                           k361)
                                                                                          (error
                                                                                           "cannot index object"
                                                                                           k361)))))
                                                                                   k359)))))
                                                                           k357)))))
                                                                   k356))
                                                                "b"
                                                                k355))
                                                             rv363
                                                             (lambda (rv354)
                                                               ((lambda (e44
                                                                         k346)
                                                                  ((lambda (i43
                                                                            k347)
                                                                     ((lambda (k348)
                                                                        ((cps
                                                                          py-list?)
                                                                         e44
                                                                         (lambda (rv349)
                                                                           (if rv349
                                                                             ((cps
                                                                               py-list-ref)
                                                                              e44
                                                                              i43
                                                                              k348)
                                                                             ((lambda (k350)
                                                                                ((cps
                                                                                  tuple?)
                                                                                 e44
                                                                                 (lambda (rv351)
                                                                                   (if rv351
                                                                                     ((cps
                                                                                       tuple-ref)
                                                                                      e44
                                                                                      i43
                                                                                      k350)
                                                                                     ((lambda (k352)
                                                                                        ((cps
                                                                                          dict?)
                                                                                         e44
                                                                                         (lambda (rv353)
                                                                                           (if rv353
                                                                                             ((cps
                                                                                               dict-ref)
                                                                                              e44
                                                                                              i43
                                                                                              k352)
                                                                                             (error
                                                                                              "cannot index object"
                                                                                              k352)))))
                                                                                      k350)))))
                                                                              k348)))))
                                                                      k347))
                                                                   0
                                                                   k346))
                                                                rv354
                                                                (lambda (rv345)
                                                                  ((cps +)
                                                                   rv344
                                                                   rv345
                                                                   (lambda (rv343)
                                                                     ((lambda (rv342)
                                                                        ((lambda (t38
                                                                                  k125)
                                                                           ((lambda (e46
                                                                                     k334)
                                                                              ((lambda (i45
                                                                                        k335)
                                                                                 ((lambda (k336)
                                                                                    ((cps
                                                                                      py-list?)
                                                                                     e46
                                                                                     (lambda (rv337)
                                                                                       (if rv337
                                                                                         ((cps
                                                                                           py-list-ref)
                                                                                          e46
                                                                                          i45
                                                                                          k336)
                                                                                         ((lambda (k338)
                                                                                            ((cps
                                                                                              tuple?)
                                                                                             e46
                                                                                             (lambda (rv339)
                                                                                               (if rv339
                                                                                                 ((cps
                                                                                                   tuple-ref)
                                                                                                  e46
                                                                                                  i45
                                                                                                  k338)
                                                                                                 ((lambda (k340)
                                                                                                    ((cps
                                                                                                      dict?)
                                                                                                     e46
                                                                                                     (lambda (rv341)
                                                                                                       (if rv341
                                                                                                         ((cps
                                                                                                           dict-ref)
                                                                                                          e46
                                                                                                          i45
                                                                                                          k340)
                                                                                                         (error
                                                                                                          "cannot index object"
                                                                                                          k340)))))
                                                                                                  k338)))))
                                                                                          k336)))))
                                                                                  k335))
                                                                               0
                                                                               k334))
                                                                            t38
                                                                            (lambda (rv126)
                                                                              (set-then!
                                                                               a
                                                                               rv126
                                                                               ((lambda (e48
                                                                                         k326)
                                                                                  ((lambda (i47
                                                                                            k327)
                                                                                     ((lambda (k328)
                                                                                        ((cps
                                                                                          py-list?)
                                                                                         e48
                                                                                         (lambda (rv329)
                                                                                           (if rv329
                                                                                             ((cps
                                                                                               py-list-ref)
                                                                                              e48
                                                                                              i47
                                                                                              k328)
                                                                                             ((lambda (k330)
                                                                                                ((cps
                                                                                                  tuple?)
                                                                                                 e48
                                                                                                 (lambda (rv331)
                                                                                                   (if rv331
                                                                                                     ((cps
                                                                                                       tuple-ref)
                                                                                                      e48
                                                                                                      i47
                                                                                                      k330)
                                                                                                     ((lambda (k332)
                                                                                                        ((cps
                                                                                                          dict?)
                                                                                                         e48
                                                                                                         (lambda (rv333)
                                                                                                           (if rv333
                                                                                                             ((cps
                                                                                                               dict-ref)
                                                                                                              e48
                                                                                                              i47
                                                                                                              k332)
                                                                                                             (error
                                                                                                              "cannot index object"
                                                                                                              k332)))))
                                                                                                      k330)))))
                                                                                              k328)))))
                                                                                      k327))
                                                                                   0
                                                                                   k326))
                                                                                g$b
                                                                                (lambda (rv325)
                                                                                  ((lambda (e50
                                                                                            k317)
                                                                                     ((lambda (i49
                                                                                               k318)
                                                                                        ((lambda (k319)
                                                                                           ((cps
                                                                                             py-list?)
                                                                                            e50
                                                                                            (lambda (rv320)
                                                                                              (if rv320
                                                                                                ((cps
                                                                                                  py-list-ref)
                                                                                                 e50
                                                                                                 i49
                                                                                                 k319)
                                                                                                ((lambda (k321)
                                                                                                   ((cps
                                                                                                     tuple?)
                                                                                                    e50
                                                                                                    (lambda (rv322)
                                                                                                      (if rv322
                                                                                                        ((cps
                                                                                                          tuple-ref)
                                                                                                         e50
                                                                                                         i49
                                                                                                         k321)
                                                                                                        ((lambda (k323)
                                                                                                           ((cps
                                                                                                             dict?)
                                                                                                            e50
                                                                                                            (lambda (rv324)
                                                                                                              (if rv324
                                                                                                                ((cps
                                                                                                                  dict-ref)
                                                                                                                 e50
                                                                                                                 i49
                                                                                                                 k323)
                                                                                                                (error
                                                                                                                 "cannot index object"
                                                                                                                 k323)))))
                                                                                                         k321)))))
                                                                                                 k319)))))
                                                                                         k318))
                                                                                      "a"
                                                                                      k317))
                                                                                   rv325
                                                                                   (lambda (rv316)
                                                                                     ((lambda (e52
                                                                                               k308)
                                                                                        ((lambda (i51
                                                                                                  k309)
                                                                                           ((lambda (k310)
                                                                                              ((cps
                                                                                                py-list?)
                                                                                               e52
                                                                                               (lambda (rv311)
                                                                                                 (if rv311
                                                                                                   ((cps
                                                                                                     py-list-ref)
                                                                                                    e52
                                                                                                    i51
                                                                                                    k310)
                                                                                                   ((lambda (k312)
                                                                                                      ((cps
                                                                                                        tuple?)
                                                                                                       e52
                                                                                                       (lambda (rv313)
                                                                                                         (if rv313
                                                                                                           ((cps
                                                                                                             tuple-ref)
                                                                                                            e52
                                                                                                            i51
                                                                                                            k312)
                                                                                                           ((lambda (k314)
                                                                                                              ((cps
                                                                                                                dict?)
                                                                                                               e52
                                                                                                               (lambda (rv315)
                                                                                                                 (if rv315
                                                                                                                   ((cps
                                                                                                                     dict-ref)
                                                                                                                    e52
                                                                                                                    i51
                                                                                                                    k314)
                                                                                                                   (error
                                                                                                                    "cannot index object"
                                                                                                                    k314)))))
                                                                                                            k312)))))
                                                                                                    k310)))))
                                                                                            k309))
                                                                                         0
                                                                                         k308))
                                                                                      rv316
                                                                                      (lambda (rv307)
                                                                                        ((lambda (e54
                                                                                                  k299)
                                                                                           ((lambda (i53
                                                                                                     k300)
                                                                                              ((lambda (k301)
                                                                                                 ((cps
                                                                                                   py-list?)
                                                                                                  e54
                                                                                                  (lambda (rv302)
                                                                                                    (if rv302
                                                                                                      ((cps
                                                                                                        py-list-ref)
                                                                                                       e54
                                                                                                       i53
                                                                                                       k301)
                                                                                                      ((lambda (k303)
                                                                                                         ((cps
                                                                                                           tuple?)
                                                                                                          e54
                                                                                                          (lambda (rv304)
                                                                                                            (if rv304
                                                                                                              ((cps
                                                                                                                tuple-ref)
                                                                                                               e54
                                                                                                               i53
                                                                                                               k303)
                                                                                                              ((lambda (k305)
                                                                                                                 ((cps
                                                                                                                   dict?)
                                                                                                                  e54
                                                                                                                  (lambda (rv306)
                                                                                                                    (if rv306
                                                                                                                      ((cps
                                                                                                                        dict-ref)
                                                                                                                       e54
                                                                                                                       i53
                                                                                                                       k305)
                                                                                                                      (error
                                                                                                                       "cannot index object"
                                                                                                                       k305)))))
                                                                                                               k303)))))
                                                                                                       k301)))))
                                                                                               k300))
                                                                                            "b"
                                                                                            k299))
                                                                                         rv307
                                                                                         (lambda (rv298)
                                                                                           ((lambda (e56
                                                                                                     k290)
                                                                                              ((lambda (i55
                                                                                                        k291)
                                                                                                 ((lambda (k292)
                                                                                                    ((cps
                                                                                                      py-list?)
                                                                                                     e56
                                                                                                     (lambda (rv293)
                                                                                                       (if rv293
                                                                                                         ((cps
                                                                                                           py-list-ref)
                                                                                                          e56
                                                                                                          i55
                                                                                                          k292)
                                                                                                         ((lambda (k294)
                                                                                                            ((cps
                                                                                                              tuple?)
                                                                                                             e56
                                                                                                             (lambda (rv295)
                                                                                                               (if rv295
                                                                                                                 ((cps
                                                                                                                   tuple-ref)
                                                                                                                  e56
                                                                                                                  i55
                                                                                                                  k294)
                                                                                                                 ((lambda (k296)
                                                                                                                    ((cps
                                                                                                                      dict?)
                                                                                                                     e56
                                                                                                                     (lambda (rv297)
                                                                                                                       (if rv297
                                                                                                                         ((cps
                                                                                                                           dict-ref)
                                                                                                                          e56
                                                                                                                          i55
                                                                                                                          k296)
                                                                                                                         (error
                                                                                                                          "cannot index object"
                                                                                                                          k296)))))
                                                                                                                  k294)))))
                                                                                                          k292)))))
                                                                                                  k291))
                                                                                               0
                                                                                               k290))
                                                                                            rv298
                                                                                            (lambda (rv289)
                                                                                              ((lambda (e58
                                                                                                        k281)
                                                                                                 ((lambda (i57
                                                                                                           k282)
                                                                                                    ((lambda (k283)
                                                                                                       ((cps
                                                                                                         py-list?)
                                                                                                        e58
                                                                                                        (lambda (rv284)
                                                                                                          (if rv284
                                                                                                            ((cps
                                                                                                              py-list-ref)
                                                                                                             e58
                                                                                                             i57
                                                                                                             k283)
                                                                                                            ((lambda (k285)
                                                                                                               ((cps
                                                                                                                 tuple?)
                                                                                                                e58
                                                                                                                (lambda (rv286)
                                                                                                                  (if rv286
                                                                                                                    ((cps
                                                                                                                      tuple-ref)
                                                                                                                     e58
                                                                                                                     i57
                                                                                                                     k285)
                                                                                                                    ((lambda (k287)
                                                                                                                       ((cps
                                                                                                                         dict?)
                                                                                                                        e58
                                                                                                                        (lambda (rv288)
                                                                                                                          (if rv288
                                                                                                                            ((cps
                                                                                                                              dict-ref)
                                                                                                                             e58
                                                                                                                             i57
                                                                                                                             k287)
                                                                                                                            (error
                                                                                                                             "cannot index object"
                                                                                                                             k287)))))
                                                                                                                     k285)))))
                                                                                                             k283)))))
                                                                                                     k282))
                                                                                                  "c"
                                                                                                  k281))
                                                                                               rv289
                                                                                               (lambda (rv280)
                                                                                                 ((lambda (e60
                                                                                                           k272)
                                                                                                    ((lambda (i59
                                                                                                              k273)
                                                                                                       ((lambda (k274)
                                                                                                          ((cps
                                                                                                            py-list?)
                                                                                                           e60
                                                                                                           (lambda (rv275)
                                                                                                             (if rv275
                                                                                                               ((cps
                                                                                                                 py-list-ref)
                                                                                                                e60
                                                                                                                i59
                                                                                                                k274)
                                                                                                               ((lambda (k276)
                                                                                                                  ((cps
                                                                                                                    tuple?)
                                                                                                                   e60
                                                                                                                   (lambda (rv277)
                                                                                                                     (if rv277
                                                                                                                       ((cps
                                                                                                                         tuple-ref)
                                                                                                                        e60
                                                                                                                        i59
                                                                                                                        k276)
                                                                                                                       ((lambda (k278)
                                                                                                                          ((cps
                                                                                                                            dict?)
                                                                                                                           e60
                                                                                                                           (lambda (rv279)
                                                                                                                             (if rv279
                                                                                                                               ((cps
                                                                                                                                 dict-ref)
                                                                                                                                e60
                                                                                                                                i59
                                                                                                                                k278)
                                                                                                                               (error
                                                                                                                                "cannot index object"
                                                                                                                                k278)))))
                                                                                                                        k276)))))
                                                                                                                k274)))))
                                                                                                        k273))
                                                                                                     0
                                                                                                     k272))
                                                                                                  rv280
                                                                                                  (lambda (rv271)
                                                                                                    ((lambda (e62
                                                                                                              k263)
                                                                                                       ((lambda (i61
                                                                                                                 k264)
                                                                                                          ((lambda (k265)
                                                                                                             ((cps
                                                                                                               py-list?)
                                                                                                              e62
                                                                                                              (lambda (rv266)
                                                                                                                (if rv266
                                                                                                                  ((cps
                                                                                                                    py-list-ref)
                                                                                                                   e62
                                                                                                                   i61
                                                                                                                   k265)
                                                                                                                  ((lambda (k267)
                                                                                                                     ((cps
                                                                                                                       tuple?)
                                                                                                                      e62
                                                                                                                      (lambda (rv268)
                                                                                                                        (if rv268
                                                                                                                          ((cps
                                                                                                                            tuple-ref)
                                                                                                                           e62
                                                                                                                           i61
                                                                                                                           k267)
                                                                                                                          ((lambda (k269)
                                                                                                                             ((cps
                                                                                                                               dict?)
                                                                                                                              e62
                                                                                                                              (lambda (rv270)
                                                                                                                                (if rv270
                                                                                                                                  ((cps
                                                                                                                                    dict-ref)
                                                                                                                                   e62
                                                                                                                                   i61
                                                                                                                                   k269)
                                                                                                                                  (error
                                                                                                                                   "cannot index object"
                                                                                                                                   k269)))))
                                                                                                                           k267)))))
                                                                                                                   k265)))))
                                                                                                           k264))
                                                                                                        "d"
                                                                                                        k263))
                                                                                                     rv271
                                                                                                     (lambda (rv262)
                                                                                                       ((lambda (e64
                                                                                                                 k254)
                                                                                                          ((lambda (i63
                                                                                                                    k255)
                                                                                                             ((lambda (k256)
                                                                                                                ((cps
                                                                                                                  py-list?)
                                                                                                                 e64
                                                                                                                 (lambda (rv257)
                                                                                                                   (if rv257
                                                                                                                     ((cps
                                                                                                                       py-list-ref)
                                                                                                                      e64
                                                                                                                      i63
                                                                                                                      k256)
                                                                                                                     ((lambda (k258)
                                                                                                                        ((cps
                                                                                                                          tuple?)
                                                                                                                         e64
                                                                                                                         (lambda (rv259)
                                                                                                                           (if rv259
                                                                                                                             ((cps
                                                                                                                               tuple-ref)
                                                                                                                              e64
                                                                                                                              i63
                                                                                                                              k258)
                                                                                                                             ((lambda (k260)
                                                                                                                                ((cps
                                                                                                                                  dict?)
                                                                                                                                 e64
                                                                                                                                 (lambda (rv261)
                                                                                                                                   (if rv261
                                                                                                                                     ((cps
                                                                                                                                       dict-ref)
                                                                                                                                      e64
                                                                                                                                      i63
                                                                                                                                      k260)
                                                                                                                                     (error
                                                                                                                                      "cannot index object"
                                                                                                                                      k260)))))
                                                                                                                              k258)))))
                                                                                                                      k256)))))
                                                                                                              k255))
                                                                                                           0
                                                                                                           k254))
                                                                                                        rv262
                                                                                                        (lambda (rv253)
                                                                                                          ((lambda (e66
                                                                                                                    k245)
                                                                                                             ((lambda (i65
                                                                                                                       k246)
                                                                                                                ((lambda (k247)
                                                                                                                   ((cps
                                                                                                                     py-list?)
                                                                                                                    e66
                                                                                                                    (lambda (rv248)
                                                                                                                      (if rv248
                                                                                                                        ((cps
                                                                                                                          py-list-ref)
                                                                                                                         e66
                                                                                                                         i65
                                                                                                                         k247)
                                                                                                                        ((lambda (k249)
                                                                                                                           ((cps
                                                                                                                             tuple?)
                                                                                                                            e66
                                                                                                                            (lambda (rv250)
                                                                                                                              (if rv250
                                                                                                                                ((cps
                                                                                                                                  tuple-ref)
                                                                                                                                 e66
                                                                                                                                 i65
                                                                                                                                 k249)
                                                                                                                                ((lambda (k251)
                                                                                                                                   ((cps
                                                                                                                                     dict?)
                                                                                                                                    e66
                                                                                                                                    (lambda (rv252)
                                                                                                                                      (if rv252
                                                                                                                                        ((cps
                                                                                                                                          dict-ref)
                                                                                                                                         e66
                                                                                                                                         i65
                                                                                                                                         k251)
                                                                                                                                        (error
                                                                                                                                         "cannot index object"
                                                                                                                                         k251)))))
                                                                                                                                 k249)))))
                                                                                                                         k247)))))
                                                                                                                 k246))
                                                                                                              "e"
                                                                                                              k245))
                                                                                                           rv253
                                                                                                           (lambda (rv244)
                                                                                                             ((lambda (b68
                                                                                                                       k209)
                                                                                                                ((lambda (i67
                                                                                                                          k210)
                                                                                                                   ((lambda (k211)
                                                                                                                      ((cps
                                                                                                                        tuple?)
                                                                                                                       b68
                                                                                                                       (lambda (rv212)
                                                                                                                         (if rv212
                                                                                                                           ((lambda (e70
                                                                                                                                     k214)
                                                                                                                              ((lambda (i69
                                                                                                                                        k215)
                                                                                                                                 ((lambda (k216)
                                                                                                                                    ((cps
                                                                                                                                      py-list?)
                                                                                                                                     e70
                                                                                                                                     (lambda (rv217)
                                                                                                                                       (if rv217
                                                                                                                                         ((cps
                                                                                                                                           py-list-ref)
                                                                                                                                          e70
                                                                                                                                          i69
                                                                                                                                          k216)
                                                                                                                                         ((lambda (k218)
                                                                                                                                            ((cps
                                                                                                                                              tuple?)
                                                                                                                                             e70
                                                                                                                                             (lambda (rv219)
                                                                                                                                               (if rv219
                                                                                                                                                 ((cps
                                                                                                                                                   tuple-ref)
                                                                                                                                                  e70
                                                                                                                                                  i69
                                                                                                                                                  k218)
                                                                                                                                                 ((lambda (k220)
                                                                                                                                                    ((cps
                                                                                                                                                      dict?)
                                                                                                                                                     e70
                                                                                                                                                     (lambda (rv221)
                                                                                                                                                       (if rv221
                                                                                                                                                         ((cps
                                                                                                                                                           dict-ref)
                                                                                                                                                          e70
                                                                                                                                                          i69
                                                                                                                                                          k220)
                                                                                                                                                         (error
                                                                                                                                                          "cannot index object"
                                                                                                                                                          k220)))))
                                                                                                                                                  k218)))))
                                                                                                                                          k216)))))
                                                                                                                                  k215))
                                                                                                                               1
                                                                                                                               k214))
                                                                                                                            t38
                                                                                                                            (lambda (rv213)
                                                                                                                              ((cps
                                                                                                                                tuple-set!)
                                                                                                                               b68
                                                                                                                               i67
                                                                                                                               rv213
                                                                                                                               k211)))
                                                                                                                           ((lambda (k222)
                                                                                                                              ((cps
                                                                                                                                py-list?)
                                                                                                                               b68
                                                                                                                               (lambda (rv223)
                                                                                                                                 (if rv223
                                                                                                                                   ((lambda (e72
                                                                                                                                             k225)
                                                                                                                                      ((lambda (i71
                                                                                                                                                k226)
                                                                                                                                         ((lambda (k227)
                                                                                                                                            ((cps
                                                                                                                                              py-list?)
                                                                                                                                             e72
                                                                                                                                             (lambda (rv228)
                                                                                                                                               (if rv228
                                                                                                                                                 ((cps
                                                                                                                                                   py-list-ref)
                                                                                                                                                  e72
                                                                                                                                                  i71
                                                                                                                                                  k227)
                                                                                                                                                 ((lambda (k229)
                                                                                                                                                    ((cps
                                                                                                                                                      tuple?)
                                                                                                                                                     e72
                                                                                                                                                     (lambda (rv230)
                                                                                                                                                       (if rv230
                                                                                                                                                         ((cps
                                                                                                                                                           tuple-ref)
                                                                                                                                                          e72
                                                                                                                                                          i71
                                                                                                                                                          k229)
                                                                                                                                                         ((lambda (k231)
                                                                                                                                                            ((cps
                                                                                                                                                              dict?)
                                                                                                                                                             e72
                                                                                                                                                             (lambda (rv232)
                                                                                                                                                               (if rv232
                                                                                                                                                                 ((cps
                                                                                                                                                                   dict-ref)
                                                                                                                                                                  e72
                                                                                                                                                                  i71
                                                                                                                                                                  k231)
                                                                                                                                                                 (error
                                                                                                                                                                  "cannot index object"
                                                                                                                                                                  k231)))))
                                                                                                                                                          k229)))))
                                                                                                                                                  k227)))))
                                                                                                                                          k226))
                                                                                                                                       1
                                                                                                                                       k225))
                                                                                                                                    t38
                                                                                                                                    (lambda (rv224)
                                                                                                                                      ((cps
                                                                                                                                        py-list-set!)
                                                                                                                                       b68
                                                                                                                                       i67
                                                                                                                                       rv224
                                                                                                                                       k222)))
                                                                                                                                   ((lambda (k233)
                                                                                                                                      ((cps
                                                                                                                                        dict?)
                                                                                                                                       b68
                                                                                                                                       (lambda (rv234)
                                                                                                                                         (if rv234
                                                                                                                                           ((lambda (e74
                                                                                                                                                     k236)
                                                                                                                                              ((lambda (i73
                                                                                                                                                        k237)
                                                                                                                                                 ((lambda (k238)
                                                                                                                                                    ((cps
                                                                                                                                                      py-list?)
                                                                                                                                                     e74
                                                                                                                                                     (lambda (rv239)
                                                                                                                                                       (if rv239
                                                                                                                                                         ((cps
                                                                                                                                                           py-list-ref)
                                                                                                                                                          e74
                                                                                                                                                          i73
                                                                                                                                                          k238)
                                                                                                                                                         ((lambda (k240)
                                                                                                                                                            ((cps
                                                                                                                                                              tuple?)
                                                                                                                                                             e74
                                                                                                                                                             (lambda (rv241)
                                                                                                                                                               (if rv241
                                                                                                                                                                 ((cps
                                                                                                                                                                   tuple-ref)
                                                                                                                                                                  e74
                                                                                                                                                                  i73
                                                                                                                                                                  k240)
                                                                                                                                                                 ((lambda (k242)
                                                                                                                                                                    ((cps
                                                                                                                                                                      dict?)
                                                                                                                                                                     e74
                                                                                                                                                                     (lambda (rv243)
                                                                                                                                                                       (if rv243
                                                                                                                                                                         ((cps
                                                                                                                                                                           dict-ref)
                                                                                                                                                                          e74
                                                                                                                                                                          i73
                                                                                                                                                                          k242)
                                                                                                                                                                         (error
                                                                                                                                                                          "cannot index object"
                                                                                                                                                                          k242)))))
                                                                                                                                                                  k240)))))
                                                                                                                                                          k238)))))
                                                                                                                                                  k237))
                                                                                                                                               1
                                                                                                                                               k236))
                                                                                                                                            t38
                                                                                                                                            (lambda (rv235)
                                                                                                                                              ((cps
                                                                                                                                                dict-set!)
                                                                                                                                               b68
                                                                                                                                               i67
                                                                                                                                               rv235
                                                                                                                                               k233)))
                                                                                                                                           (k233
                                                                                                                                            (void))))))
                                                                                                                                    k222)))))
                                                                                                                            k211)))))
                                                                                                                    k210))
                                                                                                                 1
                                                                                                                 k209))
                                                                                                              rv244
                                                                                                              (lambda (rv127)
                                                                                                                ((lambda (e76
                                                                                                                          k201)
                                                                                                                   ((lambda (i75
                                                                                                                             k202)
                                                                                                                      ((lambda (k203)
                                                                                                                         ((cps
                                                                                                                           py-list?)
                                                                                                                          e76
                                                                                                                          (lambda (rv204)
                                                                                                                            (if rv204
                                                                                                                              ((cps
                                                                                                                                py-list-ref)
                                                                                                                               e76
                                                                                                                               i75
                                                                                                                               k203)
                                                                                                                              ((lambda (k205)
                                                                                                                                 ((cps
                                                                                                                                   tuple?)
                                                                                                                                  e76
                                                                                                                                  (lambda (rv206)
                                                                                                                                    (if rv206
                                                                                                                                      ((cps
                                                                                                                                        tuple-ref)
                                                                                                                                       e76
                                                                                                                                       i75
                                                                                                                                       k205)
                                                                                                                                      ((lambda (k207)
                                                                                                                                         ((cps
                                                                                                                                           dict?)
                                                                                                                                          e76
                                                                                                                                          (lambda (rv208)
                                                                                                                                            (if rv208
                                                                                                                                              ((cps
                                                                                                                                                dict-ref)
                                                                                                                                               e76
                                                                                                                                               i75
                                                                                                                                               k207)
                                                                                                                                              (error
                                                                                                                                               "cannot index object"
                                                                                                                                               k207)))))
                                                                                                                                       k205)))))
                                                                                                                               k203)))))
                                                                                                                       k202))
                                                                                                                    2
                                                                                                                    k201))
                                                                                                                 t38
                                                                                                                 (lambda (rv128)
                                                                                                                   (set-then!
                                                                                                                    g$c
                                                                                                                    rv128
                                                                                                                    ((lambda (e78
                                                                                                                              k193)
                                                                                                                       ((lambda (i77
                                                                                                                                 k194)
                                                                                                                          ((lambda (k195)
                                                                                                                             ((cps
                                                                                                                               py-list?)
                                                                                                                              e78
                                                                                                                              (lambda (rv196)
                                                                                                                                (if rv196
                                                                                                                                  ((cps
                                                                                                                                    py-list-ref)
                                                                                                                                   e78
                                                                                                                                   i77
                                                                                                                                   k195)
                                                                                                                                  ((lambda (k197)
                                                                                                                                     ((cps
                                                                                                                                       tuple?)
                                                                                                                                      e78
                                                                                                                                      (lambda (rv198)
                                                                                                                                        (if rv198
                                                                                                                                          ((cps
                                                                                                                                            tuple-ref)
                                                                                                                                           e78
                                                                                                                                           i77
                                                                                                                                           k197)
                                                                                                                                          ((lambda (k199)
                                                                                                                                             ((cps
                                                                                                                                               dict?)
                                                                                                                                              e78
                                                                                                                                              (lambda (rv200)
                                                                                                                                                (if rv200
                                                                                                                                                  ((cps
                                                                                                                                                    dict-ref)
                                                                                                                                                   e78
                                                                                                                                                   i77
                                                                                                                                                   k199)
                                                                                                                                                  (error
                                                                                                                                                   "cannot index object"
                                                                                                                                                   k199)))))
                                                                                                                                           k197)))))
                                                                                                                                   k195)))))
                                                                                                                           k194))
                                                                                                                        "a"
                                                                                                                        k193))
                                                                                                                     g$d
                                                                                                                     (lambda (rv192)
                                                                                                                       ((lambda (e80
                                                                                                                                 k184)
                                                                                                                          ((lambda (i79
                                                                                                                                    k185)
                                                                                                                             ((lambda (k186)
                                                                                                                                ((cps
                                                                                                                                  py-list?)
                                                                                                                                 e80
                                                                                                                                 (lambda (rv187)
                                                                                                                                   (if rv187
                                                                                                                                     ((cps
                                                                                                                                       py-list-ref)
                                                                                                                                      e80
                                                                                                                                      i79
                                                                                                                                      k186)
                                                                                                                                     ((lambda (k188)
                                                                                                                                        ((cps
                                                                                                                                          tuple?)
                                                                                                                                         e80
                                                                                                                                         (lambda (rv189)
                                                                                                                                           (if rv189
                                                                                                                                             ((cps
                                                                                                                                               tuple-ref)
                                                                                                                                              e80
                                                                                                                                              i79
                                                                                                                                              k188)
                                                                                                                                             ((lambda (k190)
                                                                                                                                                ((cps
                                                                                                                                                  dict?)
                                                                                                                                                 e80
                                                                                                                                                 (lambda (rv191)
                                                                                                                                                   (if rv191
                                                                                                                                                     ((cps
                                                                                                                                                       dict-ref)
                                                                                                                                                      e80
                                                                                                                                                      i79
                                                                                                                                                      k190)
                                                                                                                                                     (error
                                                                                                                                                      "cannot index object"
                                                                                                                                                      k190)))))
                                                                                                                                              k188)))))
                                                                                                                                      k186)))))
                                                                                                                              k185))
                                                                                                                           "b"
                                                                                                                           k184))
                                                                                                                        rv192
                                                                                                                        (lambda (rv183)
                                                                                                                          ((lambda (b82
                                                                                                                                    k148)
                                                                                                                             ((lambda (i81
                                                                                                                                       k149)
                                                                                                                                ((lambda (k150)
                                                                                                                                   ((cps
                                                                                                                                     tuple?)
                                                                                                                                    b82
                                                                                                                                    (lambda (rv151)
                                                                                                                                      (if rv151
                                                                                                                                        ((lambda (e84
                                                                                                                                                  k153)
                                                                                                                                           ((lambda (i83
                                                                                                                                                     k154)
                                                                                                                                              ((lambda (k155)
                                                                                                                                                 ((cps
                                                                                                                                                   py-list?)
                                                                                                                                                  e84
                                                                                                                                                  (lambda (rv156)
                                                                                                                                                    (if rv156
                                                                                                                                                      ((cps
                                                                                                                                                        py-list-ref)
                                                                                                                                                       e84
                                                                                                                                                       i83
                                                                                                                                                       k155)
                                                                                                                                                      ((lambda (k157)
                                                                                                                                                         ((cps
                                                                                                                                                           tuple?)
                                                                                                                                                          e84
                                                                                                                                                          (lambda (rv158)
                                                                                                                                                            (if rv158
                                                                                                                                                              ((cps
                                                                                                                                                                tuple-ref)
                                                                                                                                                               e84
                                                                                                                                                               i83
                                                                                                                                                               k157)
                                                                                                                                                              ((lambda (k159)
                                                                                                                                                                 ((cps
                                                                                                                                                                   dict?)
                                                                                                                                                                  e84
                                                                                                                                                                  (lambda (rv160)
                                                                                                                                                                    (if rv160
                                                                                                                                                                      ((cps
                                                                                                                                                                        dict-ref)
                                                                                                                                                                       e84
                                                                                                                                                                       i83
                                                                                                                                                                       k159)
                                                                                                                                                                      (error
                                                                                                                                                                       "cannot index object"
                                                                                                                                                                       k159)))))
                                                                                                                                                               k157)))))
                                                                                                                                                       k155)))))
                                                                                                                                               k154))
                                                                                                                                            3
                                                                                                                                            k153))
                                                                                                                                         t38
                                                                                                                                         (lambda (rv152)
                                                                                                                                           ((cps
                                                                                                                                             tuple-set!)
                                                                                                                                            b82
                                                                                                                                            i81
                                                                                                                                            rv152
                                                                                                                                            k150)))
                                                                                                                                        ((lambda (k161)
                                                                                                                                           ((cps
                                                                                                                                             py-list?)
                                                                                                                                            b82
                                                                                                                                            (lambda (rv162)
                                                                                                                                              (if rv162
                                                                                                                                                ((lambda (e86
                                                                                                                                                          k164)
                                                                                                                                                   ((lambda (i85
                                                                                                                                                             k165)
                                                                                                                                                      ((lambda (k166)
                                                                                                                                                         ((cps
                                                                                                                                                           py-list?)
                                                                                                                                                          e86
                                                                                                                                                          (lambda (rv167)
                                                                                                                                                            (if rv167
                                                                                                                                                              ((cps
                                                                                                                                                                py-list-ref)
                                                                                                                                                               e86
                                                                                                                                                               i85
                                                                                                                                                               k166)
                                                                                                                                                              ((lambda (k168)
                                                                                                                                                                 ((cps
                                                                                                                                                                   tuple?)
                                                                                                                                                                  e86
                                                                                                                                                                  (lambda (rv169)
                                                                                                                                                                    (if rv169
                                                                                                                                                                      ((cps
                                                                                                                                                                        tuple-ref)
                                                                                                                                                                       e86
                                                                                                                                                                       i85
                                                                                                                                                                       k168)
                                                                                                                                                                      ((lambda (k170)
                                                                                                                                                                         ((cps
                                                                                                                                                                           dict?)
                                                                                                                                                                          e86
                                                                                                                                                                          (lambda (rv171)
                                                                                                                                                                            (if rv171
                                                                                                                                                                              ((cps
                                                                                                                                                                                dict-ref)
                                                                                                                                                                               e86
                                                                                                                                                                               i85
                                                                                                                                                                               k170)
                                                                                                                                                                              (error
                                                                                                                                                                               "cannot index object"
                                                                                                                                                                               k170)))))
                                                                                                                                                                       k168)))))
                                                                                                                                                               k166)))))
                                                                                                                                                       k165))
                                                                                                                                                    3
                                                                                                                                                    k164))
                                                                                                                                                 t38
                                                                                                                                                 (lambda (rv163)
                                                                                                                                                   ((cps
                                                                                                                                                     py-list-set!)
                                                                                                                                                    b82
                                                                                                                                                    i81
                                                                                                                                                    rv163
                                                                                                                                                    k161)))
                                                                                                                                                ((lambda (k172)
                                                                                                                                                   ((cps
                                                                                                                                                     dict?)
                                                                                                                                                    b82
                                                                                                                                                    (lambda (rv173)
                                                                                                                                                      (if rv173
                                                                                                                                                        ((lambda (e88
                                                                                                                                                                  k175)
                                                                                                                                                           ((lambda (i87
                                                                                                                                                                     k176)
                                                                                                                                                              ((lambda (k177)
                                                                                                                                                                 ((cps
                                                                                                                                                                   py-list?)
                                                                                                                                                                  e88
                                                                                                                                                                  (lambda (rv178)
                                                                                                                                                                    (if rv178
                                                                                                                                                                      ((cps
                                                                                                                                                                        py-list-ref)
                                                                                                                                                                       e88
                                                                                                                                                                       i87
                                                                                                                                                                       k177)
                                                                                                                                                                      ((lambda (k179)
                                                                                                                                                                         ((cps
                                                                                                                                                                           tuple?)
                                                                                                                                                                          e88
                                                                                                                                                                          (lambda (rv180)
                                                                                                                                                                            (if rv180
                                                                                                                                                                              ((cps
                                                                                                                                                                                tuple-ref)
                                                                                                                                                                               e88
                                                                                                                                                                               i87
                                                                                                                                                                               k179)
                                                                                                                                                                              ((lambda (k181)
                                                                                                                                                                                 ((cps
                                                                                                                                                                                   dict?)
                                                                                                                                                                                  e88
                                                                                                                                                                                  (lambda (rv182)
                                                                                                                                                                                    (if rv182
                                                                                                                                                                                      ((cps
                                                                                                                                                                                        dict-ref)
                                                                                                                                                                                       e88
                                                                                                                                                                                       i87
                                                                                                                                                                                       k181)
                                                                                                                                                                                      (error
                                                                                                                                                                                       "cannot index object"
                                                                                                                                                                                       k181)))))
                                                                                                                                                                               k179)))))
                                                                                                                                                                       k177)))))
                                                                                                                                                               k176))
                                                                                                                                                            3
                                                                                                                                                            k175))
                                                                                                                                                         t38
                                                                                                                                                         (lambda (rv174)
                                                                                                                                                           ((cps
                                                                                                                                                             dict-set!)
                                                                                                                                                            b82
                                                                                                                                                            i81
                                                                                                                                                            rv174
                                                                                                                                                            k172)))
                                                                                                                                                        (k172
                                                                                                                                                         (void))))))
                                                                                                                                                 k161)))))
                                                                                                                                         k150)))))
                                                                                                                                 k149))
                                                                                                                              0
                                                                                                                              k148))
                                                                                                                           rv183
                                                                                                                           (lambda (rv129)
                                                                                                                             ((lambda (e90
                                                                                                                                       k140)
                                                                                                                                ((lambda (i89
                                                                                                                                          k141)
                                                                                                                                   ((lambda (k142)
                                                                                                                                      ((cps
                                                                                                                                        py-list?)
                                                                                                                                       e90
                                                                                                                                       (lambda (rv143)
                                                                                                                                         (if rv143
                                                                                                                                           ((cps
                                                                                                                                             py-list-ref)
                                                                                                                                            e90
                                                                                                                                            i89
                                                                                                                                            k142)
                                                                                                                                           ((lambda (k144)
                                                                                                                                              ((cps
                                                                                                                                                tuple?)
                                                                                                                                               e90
                                                                                                                                               (lambda (rv145)
                                                                                                                                                 (if rv145
                                                                                                                                                   ((cps
                                                                                                                                                     tuple-ref)
                                                                                                                                                    e90
                                                                                                                                                    i89
                                                                                                                                                    k144)
                                                                                                                                                   ((lambda (k146)
                                                                                                                                                      ((cps
                                                                                                                                                        dict?)
                                                                                                                                                       e90
                                                                                                                                                       (lambda (rv147)
                                                                                                                                                         (if rv147
                                                                                                                                                           ((cps
                                                                                                                                                             dict-ref)
                                                                                                                                                            e90
                                                                                                                                                            i89
                                                                                                                                                            k146)
                                                                                                                                                           (error
                                                                                                                                                            "cannot index object"
                                                                                                                                                            k146)))))
                                                                                                                                                    k144)))))
                                                                                                                                            k142)))))
                                                                                                                                    k141))
                                                                                                                                 4
                                                                                                                                 k140))
                                                                                                                              t38
                                                                                                                              (lambda (rv130)
                                                                                                                                (set-then!
                                                                                                                                 g$c
                                                                                                                                 rv130
                                                                                                                                 ((lambda (e92
                                                                                                                                           k132)
                                                                                                                                    ((lambda (i91
                                                                                                                                              k133)
                                                                                                                                       ((lambda (k134)
                                                                                                                                          ((cps
                                                                                                                                            py-list?)
                                                                                                                                           e92
                                                                                                                                           (lambda (rv135)
                                                                                                                                             (if rv135
                                                                                                                                               ((cps
                                                                                                                                                 py-list-ref)
                                                                                                                                                e92
                                                                                                                                                i91
                                                                                                                                                k134)
                                                                                                                                               ((lambda (k136)
                                                                                                                                                  ((cps
                                                                                                                                                    tuple?)
                                                                                                                                                   e92
                                                                                                                                                   (lambda (rv137)
                                                                                                                                                     (if rv137
                                                                                                                                                       ((cps
                                                                                                                                                         tuple-ref)
                                                                                                                                                        e92
                                                                                                                                                        i91
                                                                                                                                                        k136)
                                                                                                                                                       ((lambda (k138)
                                                                                                                                                          ((cps
                                                                                                                                                            dict?)
                                                                                                                                                           e92
                                                                                                                                                           (lambda (rv139)
                                                                                                                                                             (if rv139
                                                                                                                                                               ((cps
                                                                                                                                                                 dict-ref)
                                                                                                                                                                e92
                                                                                                                                                                i91
                                                                                                                                                                k138)
                                                                                                                                                               (error
                                                                                                                                                                "cannot index object"
                                                                                                                                                                k138)))))
                                                                                                                                                        k136)))))
                                                                                                                                                k134)))))
                                                                                                                                        k133))
                                                                                                                                     5
                                                                                                                                     k132))
                                                                                                                                  t38
                                                                                                                                  (lambda (rv131)
                                                                                                                                    (set-then!
                                                                                                                                     e
                                                                                                                                     rv131
                                                                                                                                     (k125
                                                                                                                                      (void)))))))))))))))))))))))))))))))))))))))))))
                                                                         rv342
                                                                         (lambda (rv21)
                                                                           ((cps
                                                                             py-print)
                                                                            a
                                                                            (lambda (rv22)
                                                                              ((lambda (e94
                                                                                        k117)
                                                                                 ((lambda (i93
                                                                                           k118)
                                                                                    ((lambda (k119)
                                                                                       ((cps
                                                                                         py-list?)
                                                                                        e94
                                                                                        (lambda (rv120)
                                                                                          (if rv120
                                                                                            ((cps
                                                                                              py-list-ref)
                                                                                             e94
                                                                                             i93
                                                                                             k119)
                                                                                            ((lambda (k121)
                                                                                               ((cps
                                                                                                 tuple?)
                                                                                                e94
                                                                                                (lambda (rv122)
                                                                                                  (if rv122
                                                                                                    ((cps
                                                                                                      tuple-ref)
                                                                                                     e94
                                                                                                     i93
                                                                                                     k121)
                                                                                                    ((lambda (k123)
                                                                                                       ((cps
                                                                                                         dict?)
                                                                                                        e94
                                                                                                        (lambda (rv124)
                                                                                                          (if rv124
                                                                                                            ((cps
                                                                                                              dict-ref)
                                                                                                             e94
                                                                                                             i93
                                                                                                             k123)
                                                                                                            (error
                                                                                                             "cannot index object"
                                                                                                             k123)))))
                                                                                                     k121)))))
                                                                                             k119)))))
                                                                                     k118))
                                                                                  0
                                                                                  k117))
                                                                               g$b
                                                                               (lambda (rv116)
                                                                                 ((lambda (e96
                                                                                           k108)
                                                                                    ((lambda (i95
                                                                                              k109)
                                                                                       ((lambda (k110)
                                                                                          ((cps
                                                                                            py-list?)
                                                                                           e96
                                                                                           (lambda (rv111)
                                                                                             (if rv111
                                                                                               ((cps
                                                                                                 py-list-ref)
                                                                                                e96
                                                                                                i95
                                                                                                k110)
                                                                                               ((lambda (k112)
                                                                                                  ((cps
                                                                                                    tuple?)
                                                                                                   e96
                                                                                                   (lambda (rv113)
                                                                                                     (if rv113
                                                                                                       ((cps
                                                                                                         tuple-ref)
                                                                                                        e96
                                                                                                        i95
                                                                                                        k112)
                                                                                                       ((lambda (k114)
                                                                                                          ((cps
                                                                                                            dict?)
                                                                                                           e96
                                                                                                           (lambda (rv115)
                                                                                                             (if rv115
                                                                                                               ((cps
                                                                                                                 dict-ref)
                                                                                                                e96
                                                                                                                i95
                                                                                                                k114)
                                                                                                               (error
                                                                                                                "cannot index object"
                                                                                                                k114)))))
                                                                                                        k112)))))
                                                                                                k110)))))
                                                                                        k109))
                                                                                     "a"
                                                                                     k108))
                                                                                  rv116
                                                                                  (lambda (rv107)
                                                                                    ((lambda (e98
                                                                                              k99)
                                                                                       ((lambda (i97
                                                                                                 k100)
                                                                                          ((lambda (k101)
                                                                                             ((cps
                                                                                               py-list?)
                                                                                              e98
                                                                                              (lambda (rv102)
                                                                                                (if rv102
                                                                                                  ((cps
                                                                                                    py-list-ref)
                                                                                                   e98
                                                                                                   i97
                                                                                                   k101)
                                                                                                  ((lambda (k103)
                                                                                                     ((cps
                                                                                                       tuple?)
                                                                                                      e98
                                                                                                      (lambda (rv104)
                                                                                                        (if rv104
                                                                                                          ((cps
                                                                                                            tuple-ref)
                                                                                                           e98
                                                                                                           i97
                                                                                                           k103)
                                                                                                          ((lambda (k105)
                                                                                                             ((cps
                                                                                                               dict?)
                                                                                                              e98
                                                                                                              (lambda (rv106)
                                                                                                                (if rv106
                                                                                                                  ((cps
                                                                                                                    dict-ref)
                                                                                                                   e98
                                                                                                                   i97
                                                                                                                   k105)
                                                                                                                  (error
                                                                                                                   "cannot index object"
                                                                                                                   k105)))))
                                                                                                           k103)))))
                                                                                                   k101)))))
                                                                                           k100))
                                                                                        0
                                                                                        k99))
                                                                                     rv107
                                                                                     (lambda (rv98)
                                                                                       ((lambda (e100
                                                                                                 k90)
                                                                                          ((lambda (i99
                                                                                                    k91)
                                                                                             ((lambda (k92)
                                                                                                ((cps
                                                                                                  py-list?)
                                                                                                 e100
                                                                                                 (lambda (rv93)
                                                                                                   (if rv93
                                                                                                     ((cps
                                                                                                       py-list-ref)
                                                                                                      e100
                                                                                                      i99
                                                                                                      k92)
                                                                                                     ((lambda (k94)
                                                                                                        ((cps
                                                                                                          tuple?)
                                                                                                         e100
                                                                                                         (lambda (rv95)
                                                                                                           (if rv95
                                                                                                             ((cps
                                                                                                               tuple-ref)
                                                                                                              e100
                                                                                                              i99
                                                                                                              k94)
                                                                                                             ((lambda (k96)
                                                                                                                ((cps
                                                                                                                  dict?)
                                                                                                                 e100
                                                                                                                 (lambda (rv97)
                                                                                                                   (if rv97
                                                                                                                     ((cps
                                                                                                                       dict-ref)
                                                                                                                      e100
                                                                                                                      i99
                                                                                                                      k96)
                                                                                                                     (error
                                                                                                                      "cannot index object"
                                                                                                                      k96)))))
                                                                                                              k94)))))
                                                                                                      k92)))))
                                                                                              k91))
                                                                                           "b"
                                                                                           k90))
                                                                                        rv98
                                                                                        (lambda (rv89)
                                                                                          ((lambda (e102
                                                                                                    k81)
                                                                                             ((lambda (i101
                                                                                                       k82)
                                                                                                ((lambda (k83)
                                                                                                   ((cps
                                                                                                     py-list?)
                                                                                                    e102
                                                                                                    (lambda (rv84)
                                                                                                      (if rv84
                                                                                                        ((cps
                                                                                                          py-list-ref)
                                                                                                         e102
                                                                                                         i101
                                                                                                         k83)
                                                                                                        ((lambda (k85)
                                                                                                           ((cps
                                                                                                             tuple?)
                                                                                                            e102
                                                                                                            (lambda (rv86)
                                                                                                              (if rv86
                                                                                                                ((cps
                                                                                                                  tuple-ref)
                                                                                                                 e102
                                                                                                                 i101
                                                                                                                 k85)
                                                                                                                ((lambda (k87)
                                                                                                                   ((cps
                                                                                                                     dict?)
                                                                                                                    e102
                                                                                                                    (lambda (rv88)
                                                                                                                      (if rv88
                                                                                                                        ((cps
                                                                                                                          dict-ref)
                                                                                                                         e102
                                                                                                                         i101
                                                                                                                         k87)
                                                                                                                        (error
                                                                                                                         "cannot index object"
                                                                                                                         k87)))))
                                                                                                                 k85)))))
                                                                                                         k83)))))
                                                                                                 k82))
                                                                                              0
                                                                                              k81))
                                                                                           rv89
                                                                                           (lambda (rv80)
                                                                                             ((lambda (e104
                                                                                                       k72)
                                                                                                ((lambda (i103
                                                                                                          k73)
                                                                                                   ((lambda (k74)
                                                                                                      ((cps
                                                                                                        py-list?)
                                                                                                       e104
                                                                                                       (lambda (rv75)
                                                                                                         (if rv75
                                                                                                           ((cps
                                                                                                             py-list-ref)
                                                                                                            e104
                                                                                                            i103
                                                                                                            k74)
                                                                                                           ((lambda (k76)
                                                                                                              ((cps
                                                                                                                tuple?)
                                                                                                               e104
                                                                                                               (lambda (rv77)
                                                                                                                 (if rv77
                                                                                                                   ((cps
                                                                                                                     tuple-ref)
                                                                                                                    e104
                                                                                                                    i103
                                                                                                                    k76)
                                                                                                                   ((lambda (k78)
                                                                                                                      ((cps
                                                                                                                        dict?)
                                                                                                                       e104
                                                                                                                       (lambda (rv79)
                                                                                                                         (if rv79
                                                                                                                           ((cps
                                                                                                                             dict-ref)
                                                                                                                            e104
                                                                                                                            i103
                                                                                                                            k78)
                                                                                                                           (error
                                                                                                                            "cannot index object"
                                                                                                                            k78)))))
                                                                                                                    k76)))))
                                                                                                            k74)))))
                                                                                                    k73))
                                                                                                 "c"
                                                                                                 k72))
                                                                                              rv80
                                                                                              (lambda (rv71)
                                                                                                ((lambda (e106
                                                                                                          k63)
                                                                                                   ((lambda (i105
                                                                                                             k64)
                                                                                                      ((lambda (k65)
                                                                                                         ((cps
                                                                                                           py-list?)
                                                                                                          e106
                                                                                                          (lambda (rv66)
                                                                                                            (if rv66
                                                                                                              ((cps
                                                                                                                py-list-ref)
                                                                                                               e106
                                                                                                               i105
                                                                                                               k65)
                                                                                                              ((lambda (k67)
                                                                                                                 ((cps
                                                                                                                   tuple?)
                                                                                                                  e106
                                                                                                                  (lambda (rv68)
                                                                                                                    (if rv68
                                                                                                                      ((cps
                                                                                                                        tuple-ref)
                                                                                                                       e106
                                                                                                                       i105
                                                                                                                       k67)
                                                                                                                      ((lambda (k69)
                                                                                                                         ((cps
                                                                                                                           dict?)
                                                                                                                          e106
                                                                                                                          (lambda (rv70)
                                                                                                                            (if rv70
                                                                                                                              ((cps
                                                                                                                                dict-ref)
                                                                                                                               e106
                                                                                                                               i105
                                                                                                                               k69)
                                                                                                                              (error
                                                                                                                               "cannot index object"
                                                                                                                               k69)))))
                                                                                                                       k67)))))
                                                                                                               k65)))))
                                                                                                       k64))
                                                                                                    0
                                                                                                    k63))
                                                                                                 rv71
                                                                                                 (lambda (rv62)
                                                                                                   ((lambda (e108
                                                                                                             k54)
                                                                                                      ((lambda (i107
                                                                                                                k55)
                                                                                                         ((lambda (k56)
                                                                                                            ((cps
                                                                                                              py-list?)
                                                                                                             e108
                                                                                                             (lambda (rv57)
                                                                                                               (if rv57
                                                                                                                 ((cps
                                                                                                                   py-list-ref)
                                                                                                                  e108
                                                                                                                  i107
                                                                                                                  k56)
                                                                                                                 ((lambda (k58)
                                                                                                                    ((cps
                                                                                                                      tuple?)
                                                                                                                     e108
                                                                                                                     (lambda (rv59)
                                                                                                                       (if rv59
                                                                                                                         ((cps
                                                                                                                           tuple-ref)
                                                                                                                          e108
                                                                                                                          i107
                                                                                                                          k58)
                                                                                                                         ((lambda (k60)
                                                                                                                            ((cps
                                                                                                                              dict?)
                                                                                                                             e108
                                                                                                                             (lambda (rv61)
                                                                                                                               (if rv61
                                                                                                                                 ((cps
                                                                                                                                   dict-ref)
                                                                                                                                  e108
                                                                                                                                  i107
                                                                                                                                  k60)
                                                                                                                                 (error
                                                                                                                                  "cannot index object"
                                                                                                                                  k60)))))
                                                                                                                          k58)))))
                                                                                                                  k56)))))
                                                                                                          k55))
                                                                                                       "d"
                                                                                                       k54))
                                                                                                    rv62
                                                                                                    (lambda (rv53)
                                                                                                      ((lambda (e110
                                                                                                                k45)
                                                                                                         ((lambda (i109
                                                                                                                   k46)
                                                                                                            ((lambda (k47)
                                                                                                               ((cps
                                                                                                                 py-list?)
                                                                                                                e110
                                                                                                                (lambda (rv48)
                                                                                                                  (if rv48
                                                                                                                    ((cps
                                                                                                                      py-list-ref)
                                                                                                                     e110
                                                                                                                     i109
                                                                                                                     k47)
                                                                                                                    ((lambda (k49)
                                                                                                                       ((cps
                                                                                                                         tuple?)
                                                                                                                        e110
                                                                                                                        (lambda (rv50)
                                                                                                                          (if rv50
                                                                                                                            ((cps
                                                                                                                              tuple-ref)
                                                                                                                             e110
                                                                                                                             i109
                                                                                                                             k49)
                                                                                                                            ((lambda (k51)
                                                                                                                               ((cps
                                                                                                                                 dict?)
                                                                                                                                e110
                                                                                                                                (lambda (rv52)
                                                                                                                                  (if rv52
                                                                                                                                    ((cps
                                                                                                                                      dict-ref)
                                                                                                                                     e110
                                                                                                                                     i109
                                                                                                                                     k51)
                                                                                                                                    (error
                                                                                                                                     "cannot index object"
                                                                                                                                     k51)))))
                                                                                                                             k49)))))
                                                                                                                     k47)))))
                                                                                                             k46))
                                                                                                          0
                                                                                                          k45))
                                                                                                       rv53
                                                                                                       (lambda (rv44)
                                                                                                         ((lambda (e112
                                                                                                                   k36)
                                                                                                            ((lambda (i111
                                                                                                                      k37)
                                                                                                               ((lambda (k38)
                                                                                                                  ((cps
                                                                                                                    py-list?)
                                                                                                                   e112
                                                                                                                   (lambda (rv39)
                                                                                                                     (if rv39
                                                                                                                       ((cps
                                                                                                                         py-list-ref)
                                                                                                                        e112
                                                                                                                        i111
                                                                                                                        k38)
                                                                                                                       ((lambda (k40)
                                                                                                                          ((cps
                                                                                                                            tuple?)
                                                                                                                           e112
                                                                                                                           (lambda (rv41)
                                                                                                                             (if rv41
                                                                                                                               ((cps
                                                                                                                                 tuple-ref)
                                                                                                                                e112
                                                                                                                                i111
                                                                                                                                k40)
                                                                                                                               ((lambda (k42)
                                                                                                                                  ((cps
                                                                                                                                    dict?)
                                                                                                                                   e112
                                                                                                                                   (lambda (rv43)
                                                                                                                                     (if rv43
                                                                                                                                       ((cps
                                                                                                                                         dict-ref)
                                                                                                                                        e112
                                                                                                                                        i111
                                                                                                                                        k42)
                                                                                                                                       (error
                                                                                                                                        "cannot index object"
                                                                                                                                        k42)))))
                                                                                                                                k40)))))
                                                                                                                        k38)))))
                                                                                                                k37))
                                                                                                             "e"
                                                                                                             k36))
                                                                                                          rv44
                                                                                                          (lambda (rv35)
                                                                                                            ((lambda (e114
                                                                                                                      k27)
                                                                                                               ((lambda (i113
                                                                                                                         k28)
                                                                                                                  ((lambda (k29)
                                                                                                                     ((cps
                                                                                                                       py-list?)
                                                                                                                      e114
                                                                                                                      (lambda (rv30)
                                                                                                                        (if rv30
                                                                                                                          ((cps
                                                                                                                            py-list-ref)
                                                                                                                           e114
                                                                                                                           i113
                                                                                                                           k29)
                                                                                                                          ((lambda (k31)
                                                                                                                             ((cps
                                                                                                                               tuple?)
                                                                                                                              e114
                                                                                                                              (lambda (rv32)
                                                                                                                                (if rv32
                                                                                                                                  ((cps
                                                                                                                                    tuple-ref)
                                                                                                                                   e114
                                                                                                                                   i113
                                                                                                                                   k31)
                                                                                                                                  ((lambda (k33)
                                                                                                                                     ((cps
                                                                                                                                       dict?)
                                                                                                                                      e114
                                                                                                                                      (lambda (rv34)
                                                                                                                                        (if rv34
                                                                                                                                          ((cps
                                                                                                                                            dict-ref)
                                                                                                                                           e114
                                                                                                                                           i113
                                                                                                                                           k33)
                                                                                                                                          (error
                                                                                                                                           "cannot index object"
                                                                                                                                           k33)))))
                                                                                                                                   k31)))))
                                                                                                                           k29)))))
                                                                                                                   k28))
                                                                                                                1
                                                                                                                k27))
                                                                                                             rv35
                                                                                                             (lambda (rv26)
                                                                                                               ((cps
                                                                                                                 py-print)
                                                                                                                rv26
                                                                                                                (lambda (rv23)
                                                                                                                  ((cps
                                                                                                                    py-print)
                                                                                                                   g$c
                                                                                                                   (lambda (rv24)
                                                                                                                     ((cps
                                                                                                                       py-print)
                                                                                                                      e
                                                                                                                      (lambda (rv25)
                                                                                                                        (return
                                                                                                                         a
                                                                                                                         k19))))))))))))))))))))))))))))))))))
                                                                      (tuple
                                                                       "A"
                                                                       "b"
                                                                       "c"
                                                                       rv343
                                                                       "new c"
                                                                       1))))))))))))))))))))))))))))))))))))))
               (void)
               k18))
            k17))
         (g$func
          g$a
          (lambda (rv471)
            (set-then!
             g$result
             rv471
             ((cps py-print)
              g$a
              (lambda (rv472)
                ((lambda (e116 k493)
                   ((lambda (i115 k494)
                      ((lambda (k495)
                         ((cps py-list?)
                          e116
                          (lambda (rv496)
                            (if rv496
                              ((cps py-list-ref) e116 i115 k495)
                              ((lambda (k497)
                                 ((cps tuple?)
                                  e116
                                  (lambda (rv498)
                                    (if rv498
                                      ((cps tuple-ref) e116 i115 k497)
                                      ((lambda (k499)
                                         ((cps dict?)
                                          e116
                                          (lambda (rv500)
                                            (if rv500
                                              ((cps dict-ref) e116 i115 k499)
                                              (error
                                               "cannot index object"
                                               k499)))))
                                       k497)))))
                               k495)))))
                       k494))
                    "a"
                    k493))
                 g$d
                 (lambda (rv492)
                   ((lambda (e118 k484)
                      ((lambda (i117 k485)
                         ((lambda (k486)
                            ((cps py-list?)
                             e118
                             (lambda (rv487)
                               (if rv487
                                 ((cps py-list-ref) e118 i117 k486)
                                 ((lambda (k488)
                                    ((cps tuple?)
                                     e118
                                     (lambda (rv489)
                                       (if rv489
                                         ((cps tuple-ref) e118 i117 k488)
                                         ((lambda (k490)
                                            ((cps dict?)
                                             e118
                                             (lambda (rv491)
                                               (if rv491
                                                 ((cps dict-ref)
                                                  e118
                                                  i117
                                                  k490)
                                                 (error
                                                  "cannot index object"
                                                  k490)))))
                                          k488)))))
                                  k486)))))
                          k485))
                       "b"
                       k484))
                    rv492
                    (lambda (rv483)
                      ((lambda (e120 k475)
                         ((lambda (i119 k476)
                            ((lambda (k477)
                               ((cps py-list?)
                                e120
                                (lambda (rv478)
                                  (if rv478
                                    ((cps py-list-ref) e120 i119 k477)
                                    ((lambda (k479)
                                       ((cps tuple?)
                                        e120
                                        (lambda (rv480)
                                          (if rv480
                                            ((cps tuple-ref) e120 i119 k479)
                                            ((lambda (k481)
                                               ((cps dict?)
                                                e120
                                                (lambda (rv482)
                                                  (if rv482
                                                    ((cps dict-ref)
                                                     e120
                                                     i119
                                                     k481)
                                                    (error
                                                     "cannot index object"
                                                     k481)))))
                                             k479)))))
                                     k477)))))
                             k476))
                          0
                          k475))
                       rv483
                       (lambda (rv474)
                         ((cps py-print)
                          rv474
                          (lambda (rv473)
                            ((cps py-print) g$result $halt)))))))))))))))))))
   (py-list*
    (dict
     ("a"
      (py-list*
       (dict
        ("b"
         (py-list*
          (dict
           ("c"
            (py-list*
             (dict
              ("d" (py-list* (dict ("e" (py-list* 1 2 3 4)))))))))))))))))))
