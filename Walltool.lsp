(lib::lsp "MyFan.lsp")
(setfunhelp "c:Cshxq" "Tssd" "cshxq")
 
(defun c:Cshxq (/ an1 an2 ang ang1 ang2 c1 c2 cen e e1 e1l e2 e2l el en en_l enl fl1 
                fl2 flag i zoom devs la lay lay1 lay2 lay3 p p0 p1 p10 p11 p2 p3 p4 pl 
                pmid pp10 pp11 qi r r1 r2 s s1 s2 s3 vct vsz s4 ss wallwid zh pts emk 
                emka emkar emkarc del int mid ints pp len surb ptl jion zom txy
               ) 
  (defun zom (pt / p1 p2 x1 x2 y1 y2 xy xmin xmax ymin ymax ptzoom ptmin ptmax) 
    (setq p1    (@wcs (getvar "viewctr"))
          x1    (car p1)
          y1    (cadr p1)
          y2    (/ (getvar "viewsize") 2)
          p1    (@wcs (getvar "vsmin"))
          p2    (@wcs (getvar "vsmax"))
          p2    (mapcar '- p2 p1)
          xy    (/ (car p2) (cadr p2))
          x2    (* y2 xy)
          xmin  (- x1 x2)
          xmax  (+ x1 x2)
          ymin  (- y1 y2)
          ymax  (+ y1 y2)
          ptmin (@ucs (list xmin ymin))
          ptmax (@ucs (list xmax ymax))
    )
    (setq pt (@wcs pt))
    (if (<= (car pt) xmin) 
      (setq ptmin  (list (car pt) (cadr ptmin) 0)
            ptzoom 1
      )
    )
    (if (<= (cadr pt) ymin) 
      (setq ptmin  (list (car ptmin) (cadr pt) 0)
            ptzoom 1
      )
    )
    (if (>= (car pt) xmax) 
      (setq ptmax  (list (car pt) (cadr ptmax) 0)
            ptzoom 1
      )
    )
    (if (>= (cadr pt) ymax) 
      (setq ptmax  (list (car ptmax) (cadr pt) 0)
            ptzoom 1
      )
    )
    (if (= ptzoom 1) 
      (progn 
        (setq zoom  1
              ptmin (polar ptmin (* 1.25 pi) (* 2 &sp))
              ptmax (polar ptmax (* 0.25 pi) (* 2 &sp))
        )
        (command ".zoom" "w" ptmin ptmax)
      )
    )
  )

  (defun pts (pt / e l p0 p1 p2 p3 s) 
    (zom pt)
    (setq p0 (polar pt 
                    (* 0.25 pi)
                    &sp
             )
          p1 (polar pt 
                    (* 0.75 pi)
                    &sp
             )
          p2 (polar pt 
                    (* 1.25 pi)
                    &sp
             )
          p3 (polar pt 
                    (* 1.75 pi)
                    &sp
             )
          s  (ssget "cp" 
                    (list p0 p1 p2 p3)
                    (list (cons 0 "LINE,ARC") 
                          (cons 8 la)
                    )
             )
    )
    (if s 
      (progn (setq l (sslength s)) 
             (while (> l 0) 
               (setq e (ssname s (1- l))
                     l (1- l)
               )
               (if (ssmemb e ss) 
                 (ssdel e s)
               )
             )
      )
      (setq s (ssadd))
    )
    s
  )

  (defun emk (p0 p1 enl / el) 
    (setq el (subst (cons 10 (@wcs p0)) 
                    (assoc 10 enl)
                    enl
             )
          el (subst (cons 11 (@wcs p1)) 
                    (assoc 11 el)
                    el
             )
    )
    (entmake el)
  )

  (defun emka (qi zh enl / el) 
    (setq el (subst (cons 50 (@wcs qi)) 
                    (assoc 50 enl)
                    enl
             )
          el (subst (cons 51 (@wcs zh)) 
                    (assoc 51 el)
                    el
             )
    )
    (entmake el)
  )

  (defun emkar (qi zh an enl / dan da1 da2 el) 
    (setq dan (- zh qi)
          da1 (- zh an)
          da2 (- qi an)
          da3 (- an qi)
    )
    (if (< dan 0) 
      (setq dan (+ dan (* 2 pi)))
    )
    (if (< da1 0) 
      (setq da1 (+ da1 (* 2 pi)))
    )
    (if (< da2 0) 
      (setq da2 (+ da2 (* 2 pi)))
    )
    (if (< da3 0) 
      (setq da3 (+ da3 (* 2 pi)))
    )
    (if (> dan da1) 
      (if (> da1 da3) 
        (setq el (subst (cons 50 (@wcs an)) 
                        (assoc 50 enl)
                        enl
                 )
              el (subst (cons 51 (@wcs zh)) 
                        (assoc 51 el)
                        el
                 )
        )
        (setq el (subst (cons 50 (@wcs qi)) 
                        (assoc 50 enl)
                        enl
                 )
              el (subst (cons 51 (@wcs an)) 
                        (assoc 51 el)
                        el
                 )
        )
      )
      (setq el (subst (cons 50 (@wcs an)) 
                      (assoc 50 enl)
                      enl
               )
            el (subst (cons 51 (@wcs zh)) 
                      (assoc 51 el)
                      el
               )
      )
    )
    (entmake el)
  )

  (defun emkarc (qi1 zh1 qi2 zh2 enl / el da1 da2) 
    (setq da1 (- zh2 qi1)
          da2 (- zh1 qi2)
    )
    (if (< da1 0) 
      (setq da1 (+ da1 (* 2 pi)))
    )
    (if (< da2 0) 
      (setq da2 (+ da2 (* 2 pi)))
    )
    (if (< da1 da2) 
      (setq el (subst (cons 50 (@wcs qi1)) 
                      (assoc 50 enl)
                      enl
               )
            el (subst (cons 51 (@wcs zh2)) 
                      (assoc 51 el)
                      el
               )
      )
      (setq el (subst (cons 50 (@wcs qi2)) 
                      (assoc 50 enl)
                      enl
               )
            el (subst (cons 51 (@wcs zh1)) 
                      (assoc 51 el)
                      el
               )
      )
    )
    (entmake el)
  )

  (defun del (l / i) 
    (setq i 0)
    (while (< i (length l)) 
      (entdel (nth i l))
      (setq i (1+ i))
    )
  )

  (defun txy (/ po1 po2 po3 po4 dpo1 dpo2 dpo3 dpo4 apo) 
    (setq po1  (polar c1 
                      (&DRAG e1 50)
                      r1
               )
          po2  (polar c1 
                      (&DRAG e1 51)
                      r1
               )
          po3  (polar c1 
                      (&DRAG e2 50)
                      r1
               )
          po4  (polar c1 
                      (&DRAG e2 51)
                      r1
               )
          dpo1 (distance po1 po3)
          dpo2 (distance po1 po4)
          dpo3 (distance po2 po3)
          dpo4 (distance po2 po4)
    )
    (cond 
      ((= dpo1 (min dpo1 dpo2 dpo3 dpo4))
       (setq apo (angle c1 (&N2S po1 po3)))
       (emka 
         apo
         (&DRAG e1 51)
         e1l
       )
       (emka 
         apo
         (&DRAG e2 51)
         e2l
       )
       (@line 
         (polar c1 apo r1)
         (polar c1 apo r2)
         (cdr (assoc 8 e1l))
       )
      )
      ((= dpo2 (min dpo1 dpo2 dpo3 dpo4))
       (setq apo (angle c1 (&N2S po1 po4)))
       (emka 
         apo
         (&DRAG e1 51)
         e1l
       )
       (emka 
         (&DRAG e2 50)
         apo
         e2l
       )
       (@line 
         (polar c1 apo r1)
         (polar c1 apo r2)
         (cdr (assoc 8 e1l))
       )
      )
      ((= dpo3 (min dpo1 dpo2 dpo3 dpo4))
       (setq apo (angle c1 (&N2S po1 po3)))
       (emka 
         (&DRAG e2 50)
         apo
         e1l
       )
       (emka 
         apo
         (&DRAG e2 51)
         e2l
       )
       (@line 
         (polar c1 apo r1)
         (polar c1 apo r2)
         (cdr (assoc 8 e1l))
       )
      )
      ((= dpo4 (min dpo1 dpo2 dpo3 dpo4))
       (setq apo (angle c1 (&N2S po1 po3)))
       (emka 
         (&DRAG e1 50)
         apo
         e1l
       )
       (emka 
         (&DRAG e2 50)
         apo
         e2l
       )
       (@line 
         (polar c1 apo r1)
         (polar c1 apo r2)
         (cdr (assoc 8 e1l))
       )
      )
    )
  )

  (defun int (p0 p1 p2 p3 l1 l2 / p pp pp1 pp3 p4 p5 p6 p7 dis a1 a2) 
    (setq dis (* 2 (distance p1 p3))
          p4  (polar p1 
                     (+ (angle p0 p1) 
                        _pi2
                     )
                     dis
              )
          p5  (polar p1 
                     (- (angle p0 p1) 
                        _pi2
                     )
                     dis
              )
          p6  (polar p3 
                     (+ (angle p2 p3) 
                        _pi2
                     )
                     dis
              )
          p7  (polar p3 
                     (- (angle p2 p3) 
                        _pi2
                     )
                     dis
              )
          p   (inters p0 p1 p6 p7 nil)
          pp  (inters p2 p3 p4 p5 nil)
          a?