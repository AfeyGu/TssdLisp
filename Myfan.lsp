(setq _pi2 (* pi 0.5)
      _2pi (+ pi pi)
      &sp  (&SWAP T)
      &sc  (/ (float (car &sp)) (cadr &sp))
      &sp  (cadr &sp)
)

(defun @wcs (p) 
  (if (listp p) 
    (trans p 1 0)
    (+ p (angle (trans '(0 0 0) 1 0) (trans '(1 0 0) 1 0)))
  )
)

(defun @ucs (p) 
  (if (listp p) 
    (trans p 0 1)
    (+ p (angle (trans '(0 0 0) 0 1) (trans '(1 0 0) 0 1)))
  )
)

(defun @u2dcs (p) 
  (trans p 1 2)
)

(defun @ran1 (a / b mn mx) 
  (if a 
    (progn 
      (setq mn (apply 'min a)
            mn (1- mn)
      )
      (while (< mn (setq mx (apply 'max a))) 
        (setq b (cons mx b)
              a (subst mn mx a)
        )
      )
      b
    )
  )
)

(defun @ran2 (a / b c mn mx) 
  (if a 
    (progn 
      (setq mn (apply 'min a)
            mn (1- mn)
      )
      (while (< mn (setq mx (apply 'max a))) 
        (setq c a
              a (subst mn mx a)
        )
        (while (setq c (member mx c)) 
          (setq c (cdr c)
                b (cons mx b)
          )
        )
      )
      b
    )
  )
)

(defun @ran3 (a / b c d mn mx) 
  (if a 
    (progn 
      (setq c  (mapcar 'car a)
            mn (apply 'min c)
            mn (1- mn)
      )
      (while (< mn (setq mx (apply 'max c))) 
        (setq c (subst mn mx c))
        (while (setq d (assoc mx a)) 
          (setq a (subst '(nil) d a)
                b (cons d b)
          )
        )
      )
      b
    )
  )
)

(defun @ran4 (a / b c d e mn mx) 
  (if a 
    (progn 
      (setq c  (mapcar 'car a)
            mn (apply 'min c)
            mn (1- mn)
      )
      (while (< mn (setq mx (apply 'max c))) 
        (setq e a
              c (subst mn mx c)
        )
        (while (setq d (assoc mx e)) 
          (setq e (cdr (member d e))
                b (cons d b)
          )
        )
      )
      b
    )
  )
)

(defun @ran5 (a / b x x1) 
  (if a 
    (progn 
      (while (setq x (car a)) 
        (setq x1 x
              a  (cdr a)
        )
        (foreach y a (if (> y x) (setq x y)))
        (setq a (subst x1 x a))
        (if (or (< x (car b)) (not b)) (setq b (cons x b)))
      )
      b
    )
  )
)

(defun @ran6 (a / b x x1) 
  (if a 
    (progn 
      (while (setq x (car a)) 
        (setq x1 x
              a  (cdr a)
        )
        (foreach y a (if (> (car y) (car x)) (setq x y)))
        (setq a (subst x1 x a))
        (if (or (< (car x) (caar b)) (not b)) (setq b (cons x b)))
      )
      b
    )
  )
)

(defun @setosm (os) 
  (if os 
    (setvar "osmode" os)
    (progn 
      (setq os (getvar "osmode"))
      (if (< 0 os 16384) (setvar "osmode" (+ 16384 os)))
    )
  )
)

(defun @zoome (c0 lp / p1 p2 q1 q2 tfo tfz x1 x2 xmax xmin xy y1 y2 ymax ymin) 
  (setq p1   (getvar "viewctr")
        x1   (car p1)
        y1   (cadr p1)
        y2   (/ (getvar "viewsize") 2)
        p1   (getvar "vsmin")
        p2   (getvar "vsmax")
        p2   (mapcar '- p2 p1)
        xy   (/ (car p2) (cadr p2))
        x2   (* y2 xy)
        xmin (- x1 x2)
        xmax (+ x1 x2)
        ymin (- y1 y2)
        ymax (+ y1 y2)
        tfz  (minusp c0)
        c0   (abs c0)
        q1   (@ucs (getvar "extmin"))
        q2   (@ucs (getvar "extmax"))
  )
  (if (null lp) (setq lp (list q1 q2)))
  (foreach p1 lp 
    (setq x1 (car p1)
          y1 (cadr p1)
    )
    (if (< x1 xmin) 
      (setq xmin (- x1 c0)
            tfo  T
      )
    )
    (if (> x1 xmax) 
      (setq xmax (+ x1 c0)
            tfo  T
      )
    )
    (if (< y1 ymin) 
      (setq ymin (- y1 c0)
            tfo  T
      )
    )
    (if (> y1 ymax) 
      (setq ymax (+ y1 c0)
            tfo  T
      )
    )
  )
  (if tfo 
    (progn 
      (if (null tfz) 
        (setq q1 (list xmin ymin)
              q2 (list xmax ymax)
        )
      )
      (command ".zoom" "w" q1 q2)
    )
  )
  tfo
)

(defun @memb (n l mm / i tf) 
  (setq tf nil)
  (while (and (null tf) (setq i (car l))) 
    (setq tf (equal n i mm)
          l  (cdr l)
    )
  )
  tf
)

(defun @pinl (p1 lp mm / px p2 x y y1 y2 tf tf1) 
  (setq x  (car p1)
        y  (cadr p1)
        px (list (+ 1e8 x) y)
        p2 (last lp)
  )
  (if (setq tf1 (minusp mm)) (setq mm (- mm)))
  (while lp 
    (setq p3 (car lp)
          lp (cdr lp)
          y1 (cadr p2)
          y2 (cadr p3)
    )
    (if (equal (distance p2 p3) (+ (distance p1 p2) (distance p1 p3)) mm) 
      (setq tf tf1
            lp nil
      )
      (cond 
        ((equal y1 y2 mm))
        ((equal y1 y mm)
         (if (and (> (car p2) x) (> y2 y)) (setq tf (not tf)))
        )
        ((equal y2 y mm)
         (if (and (> (car p3) x) (> y1 y)) (setq tf (not tf)))
        )
        ((inters p1 px p2 p3) (setq tf (not tf)))
      )
    )
    (setq p2 p3)
  )
  tf
)

(defun @rlay (la / li ll) 
  (if la 
    (progn 
      (setq li (tblnext "layer" t)
            ll "0"
            la (strcase la)
      )
      (while (setq li (tblnext "layer")) 
        (setq li (cdr (assoc 2 li)))
        (if 
          (not 
            (wcmatch (strcase li) 
                     la
            )
          )
          (setq ll (strcat ll "," li))
        )
      )
    )
  )
  ll
)

(defun @arpl (pc r a1 a2 / n an rw plt plt1) 
  (if (null a1) 
    (setq a1 0
          a2 _2pi
    )
    (progn 
      (setq a1 (- a1 0.01)
            a2 (+ a2 0.01)
      )
      (while (< a1 0) 
        (setq a1 (+ a1 _2pi))
      )
      (while (>= a1 _2pi) 
        (setq a1 (- a1 _2pi))
      )
      (while (< a2 0) 
        (setq a2 (+ a2 _2pi))
      )
      (while (>= a2 _2pi) 
        (setq a2 (- a2 _2pi))
      )
      (if (< a2 a1) 
        (setq a2 (+ a2 _2pi))
      )
    )
  )
  (setq n    (fix 
               (/ 
                 (- a2 a1)
                 (/ pi 8)
               )
             )
        n    (if (< n 2) 
               2
               n
             )
        an   (/ 
               (- a2 a1)
               n
               2
             )
        rw   (/ r (cos an))
        plt  (cons (polar pc a1 r) 
                   plt
             )
        plt1 plt
  )
  (repeat n 
    (setq a1   (+ a1 an)
          plt  (cons (polar pc a1 rw) 
                     plt
               )
          a1   (+ a1 an)
          pn   (polar pc a1 r)
          plt  (cons pn plt)
          plt1 (cons pn plt1)
    )
  )
  (append plt (cdr (reverse plt1)))
)

(defun @subst (l pos item / i) 
  (if 
    (and (listp (car l)) 
         (setq i (assoc pos l))
    )
    (subst (cons pos item) 
           i
           l
    )
    (progn (setq i -1) 
           (mapcar 
             '(lambda (x) 
                (setq i (1+ i))
                (if (= i pos) 
                  item
                  x
                )
              )
             l
           )
    )
  )
)

(defun @aditem (l pos item / n l1 l2 sl) 
  (setq l1 nil
        l2 l
        n  (car l2)
        sl (if item 
             (1- pos)
             pos
           )
  )
  (repeat sl 
    (setq l1 (cons n l1)
          l2 (cdr l2)
          n  (car l2)
    )
  )
  (if item 
    (append (reverse l1) 
            (list 
              (if (/= item "") 
                item
              )
            )
            l2
    )
    (append (reverse l1) 
            (cdr l2)
    )
  )
)

(defun @gdman (e70) 
  (setq e70 (rem (&DRAG e70 70) 
                 32
            )
  )
  (cond 
    ((= e70 0)
     (&DRAG 50)
    )
    ((= e70 1)
     (angle (&DRAG 13) 
            (&DRAG 14)
     )
    )
    (t nil)
  )
)

(defun @gpal (msg / a di dm e l na p0 p1 pa pb lay) 
  (initget "A")
  (setq msg (strcat "\n选取" 
                    (if msg msg "线、文字或尺寸")
                    "以确定角度或 [输入角度(A)]<水平>："
            )
        e   (&DOVR msg '((0 . "line,arc,circle,lwpolyline,*text,dimension")))
  )
  (cond 
    ((null e)
     (setq a 0)
    )
    ((atom e)
     (setq a (&OSNP "\n输入角度<水平>：" 1))
     (if a 
       (setq a a)
       (setq a 0)
     )
    )
    ((listp e)
     (setq p0  (cadr e)
           na  (&DRAG 0)
           lay (&DRAG 8)
     )
     (cond 
       ((= na "LINE")
        (setq a (angle (&DRAG 10) 
                       (&DRAG 11)
                )
        )
       )
       ((or (= na "ARC") 
            (= na "CIRCLE")
        )
        (setq a (- 
                  (angle (&DRAG 10) 
                         (cadr e)
                  )
                  (* 0.5 pi)
                )
        )
       )
       ((= na "DIMENSION")
        (setq a (@gdman (car e)))
       )
       ((wcmatch na "*TEXT")
        (setq a (&DRAG 50))
       )
       (t
        (setq l (@whpl 
                  (@pl2l 
                    (&DRAG 10)
                    (&DRAG 70)
                    (&DRAG 42)
                  )
                  p0
                )
              a (if 
                  (= (length l) 
                     2
                  )
                  (angle (car l) 
                         (cadr l)
                  )
                  (- 
                    (angle (car l) 
                           p0
                    )
                    _pi2
                  )
                )
        )
       )
     )
    )
  )
  (if (< a (- pi 1e-3)) 
    (setq a a)
    (setq a (- a pi))
  )
  (list a lay)
)

(defun @axno (e st / e1 e41 p0 p1 p2 x y a hzh) 
  (if (listp st) 
    (if 
      (= (car st) 
         "hzh88"
      )
      (setq st  (cadr st)
            hzh 8
      )
    )
  )
  (if 
    (= (setq en (&DRAG e 2)) 
       "TS_REINO"
    )
    (setq e41 (tblsearch "style" "tssd_label")
          x   1
    )
    (setq e41 (tblsearch "style" "tssd_axis")
          x   1
    )
  )
  (setq e41 (assoc 41 e41)
        p0  (&DRAG 10)
        x   (* 0.25 x (&DRAG 41))
        a   (&DRAG 50)
        e1  (entnext e)
        y   (* 0.5 (&DRAG e1 40))
        e   (entget e)
        e1  (entget e1)
  )
  (if (wcmatch en "TS_AXNO[12]") 
    (setq p0 (polar p0 
                    ((if (= en "TS_AXNO1") 
                       -
                       +
                     ) 
                      a
                      _pi2
                    )
                    (+ x x)
             )
          a  0
    )
  )
  (if 
    (< (cdr (assoc 41 e)) 
       0
    )
    (if (< a (* 1.5 pi)) 
      (setq a (+ a pi))
    )
    (if (>= a pi) 
      (setq a (- a pi))
    )
  )
  (if (= hzh 8) 
    (setq p2 (polar p0 
                    (* 1.5 pi)
                    y
             )
          p1 (polar p2 0 (- x))
          p2 (polar p2 0 x)
    )
    (setq p2 (polar p0 
                    (- a _pi2)
                    y
             )
          p1 (polar p2 a (- x))
          p2 (polar p2 a x)
    )
  )
  (if st 
    (setq e1 (subst (cons 1 st) 
                    (assoc 1 e1)
                    e1
             )
    )
    (setq st (&DRAG 1))
  )
  (if 
    (= (strlen st) 
       1
    )
    (setq e1 (subst '(72 . 4) 
                    (assoc 72 e1)
                    e1
             )
          e1 (if e41 
               (subst e41 
                      (assoc 41 e1)
                      e1
               )
               e1
             )
          e1 (subst (cons 10 p0) 
                    (assoc 10 e1)
                    e1
             )
          e1 (subst (cons 11 p0) 
                    (assoc 11 e1)
                    e1
             )
    )
    (setq e1 (subst '(72 . 5) 
                    (assoc 72 e1)
                    e1
             )
          e1 (subst (cons 10 p1) 
                    (assoc 10 e1)
                    e1
             )
          e1 (subst (cons 11 p2) 
                    (assoc 11 e1)
                    e1
             )
    )
  )
  (if (wcmatch en "TS_AXNO*") 
    (setq e1 (subst '(50 . 0) 
                    (assoc 50 e1)
                    e1
             )
    )
  )
  (if (= hzh 8) 
    (setq e  (subst (cons 50 0.0) 
                    (assoc 50 e)
                    e
             )
          e  (subst (cons 41 (* 4 x)) 
                    (assoc 41 e)
                    e
             )
          e1 (subst (cons 50 0.0) 
                    (assoc 50 e1)
                    e1
             )
    )
  )
  (entmod e1)
  (entmod e)
)

(defun @ang2p (p1 p2 / a) 
  (angtos (angle p1 p2) 
          0
          1
  )
)

(defun @tidy (a) 
  (while (> a (- _2pi 1e-6)) 
    (setq a (- a _2pi))
  )
  (while (minusp (+ a 1e-6)) 
    (setq a (+ a _2pi))
  )
  a
)

(defun @gpels (n) 
  (* n 
     (/ 
       (float (getvar "viewsize"))
       (cadr (getvar "screensize"))
     )
  )
)

(defun @htas (tf / ps) 
  (setq ps (getvar "pickstyle"))
  (if tf 
    (if (< ps 2) 
      (setvar "pickstyle" (+ ps 2))
    )
    (if (> ps 1) 
      (setvar "pickstyle" (- ps 2))
    )
  )
)

(defun @area (l1 / s x1 y1 x2 y2 p1) 
  (setq s  0.
        p1 (last l1)
        x1 (car p1)
        y1 (cadr p1)
  )
  (foreach p1 l1 
    (setq x2 (car p1)
          y2 (cadr p1)
          s  (- (+ s (* x1 y2)) 
                (* x2 y1)
             )
          x1 x2
          y1 y2
    )
  )
  s
)

(defun @ptoa (p1 p2 e42 / zf p0 r a a1 a2) 
  (setq zf  (> e42 0)
        e42 (abs e42)
        r   (distance p1 p2)
        a1  (angle p1 p2)
        a2  (* 4 (atan e42))
        r   (/ 
              (* r (1+ (* e42 e42)))
              e42
              4
            )
        a1  ((if zf + -) 
              a1
              (* 0.5 pi)
              (/ a2 -2)
            )
        p0  (polar p1 a1 r)
        a1  (+ pi a1)
  )
  (if zf 
    (setq a2 (+ a1 a2))
    (setq a  a1
          a1 (- a1 a2)
          a2 a
    )
  )
  (if (< a2 a1) 
    (setq a2 (+ a2 pi pi))
  )
  (list p0 r a1 a2)
)

(defun @pl2l (lp e42 l42 / l p1) 
  (if (= e42 1) 
    (setq lp (append lp (list (car lp))))
  )
  (setq p1 (car lp)
        lp (cdr lp)
  )
  (foreach p2 lp 
    (setq e42 (car l42)
          l42 (cdr l42)
          l   (cons 
                (if (zerop e42) 
                  (list p1 p2)
                  (@ptoa p1 p2 e42)
                )
                l
              )
          p1  p2
    )
  )
  l
)

(defun @whpl (ll p0 / p1 p2) 
  (setq lr (car ll)
        mx 1e6
  )
  (foreach l ll 
    (setq p1 (car l)
          p2 (cadr l)
          ds (if 
               (= (length l) 
                  2
               )
               (if 
                 (< (distance p1 p0) 
                    (distance p1 p2)
                 )
                 (abs (&MIDP p0 p1 p2))
                 mx
               )
               (abs 
                 (- (distance p0 p1) 
                    p2
                 )
               )
             )
    )
    (if (< ds mx) 
      (setq lr l
            mx ds
      )
    )
  )
  lr
)

(defun @upda (a a0) 
  (if 
    (< (eval a) 
       (- a0 1e-4)
    )
    (set a 
         (+ (eval a) 
            pi
            pi
         )
    )
  )
)

(defun @mgrp (ss msg / el ge gn) 
  (setq gn (cdr 
             (assoc -1 
                    (dictsearch (namedobjdict) 
                                "ACAD_GROUP"
                    )
             )
           )
        el (list '(0 . "GROUP") 
                 '(102 . "{ACAD_REACTORS")
                 (cons 330 gn)
                 '(102 . "}")
                 '(100 . "AcDbGroup")
                 (cons 300 msg)
                 '(70 . 1)
                 '(71 . 1)
           )
        ge (entmakex el)
  )
  (dictadd gn "Makeshift" ge)
  (setq gn (cdr 
             (assoc 3 
                    (member (cons 350 ge) 
                            (reverse 
                              (dictsearch (namedobjdict) 
                                          "ACAD_GROUP"
                              )
                            )
                    )
             )
           )
  )
  (command ".-group" "a" gn)
  (if (listp ss) 
    (foreach e ss (command e))
    (command ss)
  )
  (command "")
)

(defun @ssgf (p1 p2 / a d) 
  (setq a (+ _pi2 (angle p1 p2))
        d (cadr (&SWAP t))
  )
  (list (polar p1 a d) 
        (polar p2 a d)
        (polar p2 a (- d))
        (polar p1 a (- d))
        (polar p1 a d)
  )
)

(defun @line (p1 p2 lay / el p) 
  (setq el (list (cons 0 "LINE") 
                 (cons 8 lay)
                 (cons 10 (@wcs p1))
                 (cons 11 (@wcs p2))
           )
  )
  (entmake el)
)

(defun @linel (pl lay / el p1 p2) 
  (while 
    (setq p1 (car pl)
          pl (cdr pl)
    )
    (setq p2 (car pl))
    (if p2 
      (progn 
        (setq el (list (cons 0 "LINE") 
                       (cons 8 lay)
                       (cons 10 (@wcs p1))
                       (cons 11 (@wcs p2))
                 )
        )
        (entmake el)
      )
    )
  )
)

(defun @circle (cen r lay / el) 
  (setq el (list (cons 0 "CIRCLE") 
                 (cons 8 lay)
                 (cons 10 (@wcs cen))
                 (cons 40 r)
           )
  )
  (entmake el)
)

(defun @arc (cen r beg end lay / el) 
  (setq el (list (cons 0 "ARC") 
                 (cons 8 lay)
                 (cons 10 (@wcs cen))
                 (cons 40 r)
                 (cons 50 (@wcs beg))
                 (cons 51 (@wcs end))
           )
  )
  (entmake el)
)

(defun @pline (lp wd el) 
  (foreach p (reverse lp) 
    (setq el (cons (cons 10 (@wcs p)) 
                   el
             )
    )
  )
  (entmake 
    (append 
      (list '(0 . "LWPOLYLINE") 
            '(100 . "AcDbEntity")
            '(100 . "AcDbPolyline")
            (cons 43 wd)
            (cons 90 (length lp))
      )
      el
    )
  )
)

(defun @text (str pt lay hi ang mod wf / i72 i73 sty) 
  (setq sty (getvar "textstyle")
        pt  (@wcs pt)
        ang (@wcs ang)
        mod (if mod mod "")
        i72 (cond 
              ((= mod "M")
               4
              )
              ((wcmatch mod "*C*")
               1
              )
              ((wcmatch mod "*R*")
               2
              )
              (t 0)
            )
        i73 (cond 
              ((= mod "M")
               0
              )
              ((wcmatch mod "*B*")
               1
              )
              ((wcmatch mod "*M*")
               2
              )
              ((wcmatch mod "*T*")
               3
              )
              (t 0)
            )
  )
  (entmake 
    (list '(0 . "TEXT") 
          (cons 1 str)
          (cons 7 sty)
          (cons 8 lay)
          (cons 10 pt)
          (cons 11 pt)
          (cons 40 hi)
          (cons 41 wf)
          (cons 50 ang)
          (cons 72 i72)
          (cons 73 i73)
    )
  )
)

(defun @DimA (p13 p14 p10 el) 
  (if 
    (entmake 
      (append 
        (list '(0 . "DIMENSION") 
              '(100 . "AcDbEntity")
              '(100 . "AcDbDimension")
              (cons 10 p10)
              '(11 0. 0. 0.)
              '(70 . 33)
              '(100 . "AcDbAlignedDimension")
              (cons 13 p13)
              (cons 14 p14)
        )
        el
      )
    )
    (entlast)
  )
)

(defun @sget (lx lay cwp pl / ss p1 p2 ang pt an dis p p3 p4 pt pold flag xs zoom ptl) 
  (if 
    (and (setq p1 (&OSNP "\n点取选择窗口的第一点<退出>: ")) 
         (setq p2 (&OSNP p1 "\n点取选择窗口的第二点<退出>: "))
    )
    (progn (setq ang (angle p1 p2)) 
           (prompt "\n点取选择窗口的结束点: ")
           (while (not pt) 
             (setq p    (grread T 3)
                   p3   (cadr p)
                   i    0
                   pold p2
                   flag (car p)
             )
             (if (/= flag 5) 
               (if (/= flag 3) 
                 (setq p3 pold)
               )
             )
             (if 
               (> (distance p3 pold) 
                  &sp
               )
               (progn (redraw) 
                      (setq an  (angle p2 p3)
                            dis (distance p2 p3)
                            p4  (polar p1 an dis)
                            p   (car 
                                  (&ORDR 
                                    3
                                    p3
                                    (polar p3 
                                           (+ ang (* 0.5 pi))
                                           100
                                    )
                                    p1
                                    p2
                                  )
                                )
                      )
                      (if 
                        (and (> ang (* 0.5 pi)) 
                             (< ang (* 1.5 pi))
                        )
                        (if 
                          (and 
                            (> (angle p p3) 
                               pi
                            )
                            (< (angle p p3) 
                               (* 2 pi)
                            )
                          )
                          (setq xs 1)
                          (setq xs 0)
                        )
                        (if 
                          (and 
                            (> (angle p p3) 
                               0
                            )
                            (< (angle p p3) 
                               pi
                            )
                          )
                          (setq xs 1)
                          (setq xs 0)
                        )
                      )
                      (if (= cwp "wp") 
                        (setq xs 0)
                      )
                      (grdraw p1 p2 7 xs)
                      (grdraw p2 p3 7 xs)
                      (grdraw p3 p4 7 xs)
                      (grdraw p4 p1 7 xs)
               )
             )
             (if (= flag 3) 
               (setq pt p3)
             )
           )
           (redraw)
           (if (and p1 p2 p3 p4 (not pl)) 
             (progn 
               (if (= xs 1) 
                 (setq xs "cp")
                 (setq xs "wp")
               )
               (setq zoom (@zoome &sp (list p1 p2 p3 p4))
                     ptl  (list p1 p2 p3 p4)
                     ss   (ssget xs 
                                 ptl
                                 (list (cons 0 lx) 
                                       (cons 8 lay)
                                 )
                          )
               )
               (if zoom (command ".Zoom" "p"))
             )
             (if (and p1 p2 p3 p4) 
               (setq ptl (list p1 p2 p3 p4))
             )
           )
    )
  )
  (list ss ptl)
)

(defun @ssf (en lay flag / ss s enl fla len e fl) 
  (setq ss   (ssget "x" 
                    (list (cons 0 "LINE,ARC,CIRCLE") 
                          (cons 8 lay)
                    )
             )
        s    (ssadd)
        enl  (entget en)
        flag (strcase flag)
        fla  (strcase (cdr (assoc 0 enl)))
  )
  (if ss 
    (progn (setq len (sslength ss)) 
           (while (> len 0) 
             (setq len (1- len)
                   e   (ssname ss len)
                   fl  (strcase (&DRAG e 0))
             )
             (if (wcmatch flag fl) 
               (if (car (&ORDR 0 en e)) 
                 (ssadd e s)
               )
             )
           )
    )
  )
  (if 
    (> (sslength s) 
       0
    )
    s
  )
)

(defun @ssgetf (lay flag p0 p1 / a dl e len p2 ptl s ss tfz) 
  (if (null lay) 
    (setq lay "*")
  )
  (setq dl  (max (* 2 (cadr (&SWAP t))) 
                 (/ 
                   (getvar "ltscale")
                   5
                 )
            )
        a   (angle p0 p1)
        ptl (list 
              (polar p0 
                     (+ a _pi2)
                     dl
              )
              (polar p1 
                     (+ a _pi2)
                     dl
              )
              (polar p1 
                     (- a _pi2)
                     dl
              )
              (polar p0 
                     (- a _pi2)
                     dl
              )
            )
        s   (ssadd)
        tfz (@zoome -1 ptl)
        ss  (ssget "cp" 
                   ptl
                   (list (cons 0 flag) 
                         (cons 8 lay)
                   )
            )
  )
  (if ss 
    (progn (command ".line" p0 p1 "") 
           (setq len (sslength ss)
                 en  (entlast)
           )
           (while (> len 0) 
             (setq len (1- len)
                   e   (ssname ss len)
             )
             (if (&ORDR 0 en e) 
               (ssadd e s)
             )
           )
           (entdel en)
    )
  )
  (if tfz (command ".zoom" "p"))
  (if 
    (> (sslength s) 
       0
    )
    s
  )
)

(defun @sslay (msg flt / e laf lay si ss) 
  (if (setq ss (&DSTR msg (list (cons 0 flt)))) 
    (progn 
      (setq si  0
            laf ""
      )
      (while (setq e (ssname ss si)) 
        (setq si  (1+ si)
              lay (&DRAG e 8)
        )
        (if (not (wcmatch lay laf)) 
          (setq laf (if (= laf "") 
                      lay
                      (strcat laf "," lay)
                    )
          )
        )
      )
      (setq ss (ssget "x" 
                      (list (cons 8 laf) 
                            (cons 0 flt)
                      )
               )
      )
      (princ "\n选择要剔除的实体<无>: ")
      (setvar "highlight" 1)
      (command "select" ss "r" pause)
      (setq ss (ssget "p"))
    )
  )
  ss
)

(defun @findw (pl lay / sgetpt assl xin p1 p2 ss s e l i p pp ang no1 no2 ln n an1 
               an2 an3 an4 flag
              ) 

  (defun sgetpt (p / p1 p2 p3 p4 s flag) 
    (setq p1 (polar p 
                    (* 0.25 pi)
                    (* 0.02 &sp)
             )
          p2 (polar p 
                    (* 0.75 pi)
                    (* 0.02 &sp)
             )
          p3 (polar p 
                    (* 1.25 pi)
                    (* 0.02 &sp)
             )
          p4 (polar p 
                    (* 1.75 pi)
                    (* 0.02 &sp)
             )
          s  (ssget "cp" 
                    (list p1 p2 p3 p4)
                    (list (cons 8 lay))
             )
    )
    (if 
      (and s 
           (> (sslength s) 
              1
           )
      )
      (setq tf T)
      (setq tf nil)
    )
    tf
  )

  (defun xin (pl / l p pt) 
    (setq l  (length pl)
          pt (list 0 0)
    )
    (while (setq p (car pl)) 
      (setq pt (list 
                 (+ (car pt) 
                    (car p)
                 )
                 (+ (cadr pt) 
                    (cadr p)
                 )
               )
            pl (cdr pl)
      )
    )
    (setq pt (list 
               (/ 
                 (car pt)
                 l
               )
               (/ 
                 (cadr pt)
                 l
               )
               0
             )
    )
  )

  (defun assl (i l) 
    (cdr (assoc i (nth 4 l)))
  )
  (setq p  (xin pl)
        p1 (car pl)
        pl (append (cdr pl) 
                   (list p1)
           )
        ss (ssadd)
  )
  (while (setq p2 (car pl)) 
    (setq p1 (polar p1 
                    (angle p p1)
                    (* 0.05 &sp)
             )
          p2 (polar p2 
                    (angle p p2)
                    (* 0.05 &sp)
             )
    )
    (setq s  (ssget "f" 
                    (list p1 p2)
                    (list (cons 0 "LINE,ARC") 
                          (cons 8 lay)
                    )
             )
          pl (cdr pl)
          p1 p2
    )
    (if s 
      (while (setq e (ssname s 0)) 
        (if (ssmemb e ss) 
          (ssdel e ss)
          (ssadd e ss)
        )
        (ssdel e s)
      )
    )
  )
  (setq l  (sslength ss)
        i  0
        pl nil
  )
  (while (> l i) 
    (setq e  (ssname ss i)
          el (entget e)
          i  (1+ i)
    )
    (if (wcmatch "LINE" (strcase (cdr (assoc 0 el)))) 
      (progn 
        (setq p1 (&DRAG e 10)
              p2 (&DRAG e 11)
        )
        (if 
          (> (distance p p1) 
             (distance p p2)
          )
          (setq pp p1
                p1 p2
                p2 pp
          )
        )
        (setq ang (angle p p1)
              pl  (cons (list ang p1 p2 e el 0) 
                        pl
                  )
        )
      )
      (progn 
        (setq p1 (polar (&DRAG e 10) 
                        (&DRAG e 50)
                        (&DRAG e 40)
                 )
              p2 (polar (&DRAG e 10) 
                        (&DRAG e 51)
                        (&DRAG e 40)
                 )
        )
        (if 
          (> (distance p p1) 
             (distance p p2)
          )
          (setq pp p1
                p1 p2
                p2 pp
          )
        )
        (setq ang (angle p p1)
              pl  (cons (list ang p1 p2 e el 1) 
                        pl
                  )
        )
      )
    )
  )
  (setq pl (if pl (@ran4 pl))
        l  (length pl)
        i  0
  )
  (while (> l i) 
    (setq no1 (nth i pl)
          i   (1+ i)
          no2 (nth i pl)
    )
    (if (not no2) 
      (setq no2 (car pl))
    )
    (if 
      (and (not (sgetpt (cadr no1))) 
           (not (sgetpt (cadr no2)))
      )
      (progn 
        (setq p (&ORDR 
                  3
                  (nth 3 no1)
                  (nth 3 no2)
                )
        )
        (cond 
          (p
           (command ".Extend" 
                    (cadddr no1)
                    (cadddr no2)
                    ""
                    "E"
                    "E"
                    (list (cadddr no1) 
                          (cadr no1)
                    )
                    (list (cadddr no2) 
                          (cadr no2)
                    )
                    ""
           )
           (setq i    (1+ i)
                 flag T
           )
          )
          ((and (assl 11 no1) 
                (assl 11 no2)
           )
           (setq an1 (angle (cadr no1) 
                            (caddr no1)
                     )
                 an2 (angle (cadr no2) 
                            (caddr no2)
                     )
                 an3 (angle (cadr no1) 
                            (cadr no2)
                     )
           )
           (if (equal an1 _2pi 0.01) 
             (setq an1 0.0)
           )
           (if (equal an2 _2pi 0.01) 
             (setq an2 0.0)
           )
           (if (equal an3 _2pi 0.01) 
             (setq an3 0.0)
           )
           (setq nnn no1)
           (if 
             (and 
               (or (equal an1 an3 0.01) 
                   (equal an2 an3 0.01)
               )
               (equal 
                 (- (max an1 an2) 
                    (min an1 an2)
                 )
                 pi
                 0.01
               )
             )
             (progn 
               (setq el   (nth 4 no1)
                     el   (subst (cons 10 (@wcs (caddr no1))) 
                                 (assoc 10 el)
                                 el
                          )
                     el   (subst (cons 11 (@wcs (caddr no2))) 
                                 (assoc 11 el)
                                 el
                          )
                     flag T
                     i    (1+ i)
               )
               (entmod el)
               (entdel (cadddr no2))
             )
           )
          )
          ((and 
             (equal (assl 40 no1) 
                    (assl 40 no2)
                    0.01
             )
             (equal (assl 10 no1) 
                    (assl 10 no2)
                    0.01
             )
           )
           (setq an1 (assl 50 no1)
                 an2 (assl 51 no1)
                 an3 (assl 50 no2)
                 an4 (assl 51 no2)
           )
           (if 
             (< (abs (- an2 an3)) 
                (abs (- an1 an4))
             )
             (setq an2 an4)
             (setq an1 an3)
           )
           (setq el   (nth 4 no1)
                 el   (subst (cons 50 an1) 
                             (assoc 50 el)
                             el
                      )
                 el   (subst (cons 51 an2) 
                             (assoc 51 el)
                             el
                      )
                 flag T
                 i    (1+ i)
           )
           (entmod el)
           (entdel (cadddr no2))
          )
        )
      )
    )
  )
  flag
)

(defun @substr (str start len / ch) 
  (if (= len 1) 
    (if 
      (> (ascii (substr str start)) 
         159
      )
      (setq ch             (substr str start 2)
            Tsz_If_Unicode T
      )
      (setq ch             (substr str start 1)
            Tsz_If_Unicode nil
      )
    )
    (setq ch             (substr str start len)
          Tsz_If_Unicode nil
    )
  )
  ch
)

(defun @peronl (p0 p1 p2 / da db dx dy x0 x1 x2 y0 y1 y2) 
  (if 
    (or (equal p0 p1 1e-3) 
        (equal p0 p2 1e-3)
    )
    p0
    (progn 
      (setq x0 (car p0)
            x1 (car p1)
            x2 (car p2)
            y0 (cadr p0)
            y1 (cadr p1)
            y2 (cadr p2)
            dx (- x2 x1)
            dy (- y2 y1)
            da (+ (* dx dx) 
                  (* dy dy)
               )
            db (+ (* dx (- x0 x1)) 
                  (* dy (- y0 y1))
               )
            da (/ db da)
      )
      (list (+ x1 (* dx da)) 
            (+ y1 (* dy da))
      )
    )
  )
)

(defun @udtdft (e0 / e st st1) 
  (setq e (cdr (assoc -2 (tblsearch "block" (&DRAG e0 2)))))
  (while e 
    (if 
      (= (&DRAG e 0) 
         "MTEXT"
      )
      (setq st (last 
                 (&PLCN 
                   (&DRAG 1)
                   ";"
                 )
               )
            e  nil
      )
      (setq e (entnext e))
    )
  )
  (@Hlt e0 3)
  (setq st1 (getstring (strcat "\n输入尺寸标注值<" st ">: ")))
  (@Hlt e0 4)
  (if 
    (and (read st1) 
         (/= st st1)
    )
    (progn (command ".dim1" "new" st1 e0 "") 
           t
    )
  )
)

(defun #cls () 
  (set_tile "error" "")
)

(defun #isnum (n1 n2 / lab sg1 sg2 st1 st2 vl) 
  (#cls)
  (if n1 
    (if 
      (= (type n1) 
         'REAL
      )
      (setq sg1 >=
            st1 "≥"
      )
      (setq sg1 >
            st1 "＞"
      )
    )
  )
  (if n2 
    (if 
      (= (type n2) 
         'REAL
      )
      (setq sg2 <=
            st2 "≤"
      )
      (setq sg2 <
            st2 "＜"
      )
    )
  )
  (if (wcmatch $value "[.]*") 
    (setq $value (strcat "0" $value))
  )
  (if 
    (and 
      (= (setq vl (read $value)) 
         (atof $value)
      )
      (or 
        (and (null n1) 
             (null n2)
        )
        (and n1 
             (null n2)
             (sg1 vl n1)
        )
        (and n2 
             (null n1)
             (sg2 vl n2)
        )
        (and n1 
             n2
             (sg1 vl n1)
             (sg2 vl n2)
        )
      )
    )
    (set (read $key) 
         vl
    )
    (progn 
      (setq lab (get_attr $key "label")
            lab (if (= lab "") 
                  "**该项值"
                  (strcat "**[" lab "]")
                )
      )
      (set_tile "error" 
                (cond 
                  ((and (null n1) 
                        (null n2)
                   )
                   (strcat lab "应为实数值!")
                  )
                  ((and n1 (null n2))
                   (strcat lab 
                           "应"
                           st1
                           (&RTXT n1)
                           "!"
                   )
                  )
                  ((and n2 (null n1))
                   (strcat lab 
                           "应"
                           st2
                           (&RTXT n2)
                           "!"
                   )
                  )
                  ((and n1 n2)
                   (strcat lab 
                           "应在"
                           (&RTXT n1)
                           "～"
                           (&RTXT n2)
                           "之间!"
                   )
                  )
                )
      )
      (set_tile $key (&RTXT (eval (read $key))))
      nil
    )
  )
)

(defun #notsp (msg) 
  (or 
    (= (@substr msg 1 1) 
       "."
    )
    (read msg)
  )
)

(defun #dis_list (key pos) 
  (start_list key)
  (mapcar 'add_list (eval (read key)))
  (end_list)
  (mode_tile key 2)
  (set_tile key pos)
)

(defun #do_add (key new / l pos) 
  (setq l   (eval (read key))
        pos (1+ (atoi (get_tile key)))
        l   (@aditem l pos new)
  )
  (set (read key) 
       l
  )
  (#dis_list key (itoa pos))
)

(defun #do_del (key / l pos) 
  (if (setq l (eval (read key))) 
    (progn 
      (setq pos (atoi (get_tile key))
            l   (@aditem l pos nil)
      )
      (set (read key) 
           l
      )
      (setq pos (cond 
                  ((< pos 1)
                   0
                  )
                  ((= pos (length l))
                   (1- pos)
                  )
                  (T pos)
                )
      )
      (#dis_list key (itoa pos))
    )
  )
)

(defun #do_edit (key new / l pos) 
  (setq l   (eval (read key))
        pos (atoi (get_tile key))
        l   (@subst l pos new)
  )
  (set (read key) 
       l
  )
  (#dis_list key (itoa pos))
)

(defun #f_img (key img / x y) 
  (setq x (dimx_tile key)
        y (dimy_tile key)
  )
  (start_image key)
  (if (numberp img) 
    (fill_image 0 0 x y img)
    (slide_image 0 0 x y (strcat "acad(" img ")"))
  )
  (end_image)
)

(defun #dcldata (_#fn _#l _#tf / _#l1 _#x _#x1 _#tf1) 
  (setq _#tf1 (wcmatch _#fn "*`.*"))
  (if 
    (and 
      (if 
        (and (null _#tf1) 
             (null (wcmatch _#fn "*/*"))
        )
        (prompt (strcat "\n***[#dcldata]中[" _#fn "]的格式不对！"))
        T
      )
      (if 
        (= 
          (rem (length _#l) 
               2
          )
          1
        )
        (prompt "\n***[#dcldata]输入列表长度为单数！")
        T
      )
    )
    (progn 
      (if (and _#tf1 (null (wcmatch _#fn "*/*"))) 
        (setq _#fn (strcat (&SGET) 
                           "prg\\"
                           _#fn
                   )
        )
      )
      (while _#l 
        (if 
          (= (type (setq _#x (car _#l))) 
             'STR
          )
          (setq _#x  (strcase _#x t)
                _#x1 (if _#tf 
                       (cadr _#l)
                       (eval (read _#x))
                     )
                _#l1 (cons (cons _#x _#x1) 
                           _#l1
                     )
                _#l  (cddr _#l)
          )
          (progn (princ "**\n[") 
                 (princ _#x)
                 (princ "]索引项应该为字符型！")
                 (setq _#l nil)
          )
        )
      )
      (setq _#l  (reverse _#l1)
            _#l1 nil
      )
      (if _#tf 
        (progn 
          (if _#tf1 
            (if (setq _#fn (open _#fn "r")) 
              (progn (setq _#l1 (read (read-line _#fn))) 
                     (close _#fn)
              )
            )
            (setq _#l1 (read (&GPTS _#fn)))
          )
          (foreach _#x _#l 
            (if 
              (setq _#x1 (assoc (car _#x) 
                                _#l1
                         )
              )
              (setq _#l (subst _#x1 _#x _#l)
                    _#x _#x1
              )
            )
            (if (/= _#tf 0) 
              (set (read (car _#x)) 
                   (cdr _#x)
              )
            )
          )
        )
        (if _#tf1 
          (if (setq _#fn (open _#fn "w")) 
            (progn (prin1 _#l _#fn) 
                   (close _#fn)
            )
          )
          (progn (setq _#x (vl-prin1-to-string _#l)) 
                 (&GLAY _#fn _#x)
          )
        )
      )
      _#l
    )
  )
)

(defun @txt2lst (st0 / i j n ll sl so st1 su tf stll) 

  (defun stll (st) 
    (cons (strcat su so st so su) 
          ll
    )
  )
  (setq i  1
        sl (strlen st0)
        su ""
        so ""
  )
  (if (> sl 1) 
    (progn 
      (while (< i sl) 
        (setq st1 (strcase (substr st0 i)))
        (cond 
          ((> (ascii (substr st0 i 1)) 
              159
           )
           (setq ll (stll (substr st0 i 2))
                 i  (+ 2 i)
           )
          )
          ((or (setq n (wcmatch st1 "%%###*")) 
               (and 
                 (= (substr st1 1 1) 
                    "\\"
                 )
                 (wcmatch (substr st1 2) 
                          "U+[0-9A-F][0-9A-F][0-9A-F][0-9A-F]*"
                 )
               )
           )
           (setq tf nil)
           (cond 
             ((wcmatch st1 
                       (strcat (&FLD "KZZF1") 
                               "*"
                       )
              )
              (setq j  (vl-string-search (&FLD "KZZF2") 
                                         st1
                       )
                    tf t
              )
             )
             ((wcmatch st1 
                       (strcat (&FLD "KZZF3") 
                               "*"
                       )
              )
              (setq j  (vl-string-search (&FLD "KZZF4") 
                                         st1
                       )
                    tf t
              )
             )
             ((wcmatch st1 
                       (strcat (&FLD "KZZF5") 
                               "*"
                       )
              )
              (setq j (vl-string-search (&FLD "KZZF6") 
                                        st1
                      )
              )
             )
             ((wcmatch st1 
                       (strcat (&FLD "KZZF6") 
                               "*"
                       )
              )
              (setq j (vl-string-search (&FLD "KZZF5") 
                                        st1
                      )
              )
             )
             ((wcmatch st1 
                       (strcat (&FLD "YQ1") 
                               "*"
                       )
              )
              (setq j (vl-string-search (&FLD "YQ2") 
                                        st1
                      )
              )
             )
             ((wcmatch st1 
                       (strcat (&FLD "YQ3") 
                               "*"
                       )
              )
              (setq j (vl-string-search (&FLD "YQ4") 
                                        st1
                      )
              )
             )
             ((wcmatch st1 
                       (strcat (&FLD "YQ5") 
                               "*"
                       )
              )
              (setq j (vl-string-search (&FLD "YQ6") 
                                        st1
                      )
              )
             )
             (t (setq j -1))
           )
           (setq n (if n 5 7))
           (if (= j -1) 
             (setq ll (stll (substr st0 i n))
                   i  (+ n i)
             )
             (setq j  (if j 
                        (+ j n)
                        (- sl i -1)
                      )
                   ll (if (and tf ll) 
                        (cons 
                          (@mrg2str 
                            (car ll)
                            (strcat su 
                                    so
                                    (substr st0 i j)
                                    so
                                    su
                            )
                          )
                          (cdr ll)
                        )
                        (stll (substr st0 i j))
                      )
                   i  (+ j i)
             )
           )
          )
          ((wcmatch st1 "%%[%DPC]*")
           (setq ll (stll (substr st0 i 3))
                 i  (+ 3 i)
           )
          )
          ((wcmatch st1 "%%O*")
           (setq so (if (= so "") 
                      "%%O"
                      ""
                    )
                 i  (+ 3 i)
           )
          )
          ((wcmatch st1 "%%U*")
           (setq su (if (= su "") 
                      "%%U"
                      ""
                    )
                 i  (+ 3 i)
           )
          )
          (t
           (setq ll (stll (substr st0 i 1))
                 i  (1+ i)
           )
          )
        )
      )
      (if (= i sl) 
        (setq ll (stll (substr st0 sl 1)))
      )
      (setq ll (reverse ll))
    )
    (setq ll (list st0))
  )
  ll
)

(defun @mrg2str (st1 st2) 
  (setq si1 (strlen st1))
  (cond 
    ((and (wcmatch st1 "*%%O%%U") 
          (wcmatch st2 "%%U%%O*")
     )
     (strcat (substr st1 1 (- si1 6)) 
             (substr st2 7)
     )
    )
    ((and (wcmatch st1 "*%%O") 
          (wcmatch st2 "%%U%%O*")
     )
     (strcat (substr st1 1 (- si1 3)) 
             "%%U"
             (substr st2 7)
     )
    )
    ((and (wcmatch st1 "*%%O%%U") 
          (wcmatch st2 "%%O*")
     )
     (strcat (substr st1 1 (- si1 6)) 
             "%%U"
             (substr st2 4)
     )
    )
    ((or 
       (and (wcmatch st1 "*%%O") 
            (wcmatch st2 "%%O*")
       )
       (and (wcmatch st1 "*%%U") 
            (wcmatch st2 "%%U*")
       )
     )
     (strcat (substr st1 1 (- si1 3)) 
             (substr st2 4)
     )
    )
    ('T (strcat st1 st2))
  )
)

(defun @gstr (key msg nam eget / e en el na str regex _rplc) 

  (defun _rplc (sour targ) 
    (vlax-put-property regex "Pattern" sour)
    (vlax-invoke-method regex "Replace" str targ)
  )
  (setq nam  (strcase nam)
        eget (if eget nentsel entsel)
  )
  (while 
    (and msg 
         (progn (if key (initget key)) 
                t
         )
         (setq e (eget msg))
    )
    (if (listp e) 
      (if 
        (and (setq en (car (reverse (nth 3 e)))) 
             (setq el (entget en)
                   el (member (assoc 8 el) 
                              el
                      )
             )
             (= (strcase (cdr (assoc 100 el))) 
                "ACDBMINSERTBLOCK"
             )
             (wcmatch (cdr (assoc 2 el)) 
                      "`**"
             )
        )
        (setq e   (prompt "\n**不能编辑只读图形！！！")
              msg nil
        )
        (if 
          (wcmatch 
            (setq na (strcase 
                       (&DRAG 
                         (car e)
                         0
                       )
                     )
            )
            nam
          )
          (progn (setq msg nil) 
                 (cond 
                   ((wcmatch na "TEXT,ATTRIB")
                    (setq str (&DRAG 1))
                   )
                   ((= na "ATTDEF")
                    (setq str (&DRAG 2))
                   )
                   ((= na "MTEXT")
                    (setq regex (vlax-create-object "Vbscript.RegExp"))
                    (vlax-put-property regex "IgnoreCase" 0)
                    (vlax-put-property regex "Global" 1)
                    (setq str (vla-get-textstring (vlax-ename->vla-object (car e)))
                          str (_rplc "\\\\\\\\" (chr 1))
                          str (_rplc "\\\\{" (chr 2))
                          str (_rplc "\\\\}" (chr 3))
                          str (_rplc 
                                "\\\\S(.[^;]*)(\\^|#|/)(.[^;]*);""")
                          str (_rplc 
                                "\\\\(F|f|C|H|\T|Q|W|A|p)(.[^;]*);""")
                          str (_rplc 
                                "\\\\(L|l|O|o|~|P)""")
                          str (_rplc "\n" "")
                          str (_rplc 
                                "({|})""")
                          str (_rplc "\\x01" "\\")
                          str (_rplc "\\x02" "{")
                          str (_rplc "\\x03" "}")
                    )
                    (vlax-release-object regex)
                   )
                   ('T (setq e (prompt "\n(@gstr key msg **nam)参数出错！！！")))
                 )
                 (if (and e str) 
                   (setq e (append 
                             (list (&GENT str) 
                                   na
                             )
                             e
                           )
                   )
                 )
          )
          (princ "\n**没有选中指定实体，重新选择...")
        )
      )
      (setq msg nil)
    )
  )
  e
)

(defun @rtxt (str / i len lnk st1 stn) 
  (if (wcmatch str "*[dDfFLHCI@-][1-9]*") 
    (progn 
      (setq lnk (&GSYS "钢筋间距符号")
            lnk (if (= lnk "1") 
                  "-"
                  "@"
                )
            i   1
            len (strlen str)
            stn ""
      )
      (while (<= i len) 
        (setq st1 (substr str i))
        (cond 
          ((> (ascii st1) 
              159
           )
           (setq st1 (substr st1 1 2)
                 i   (1+ i)
           )
          )
          ((and 
             (= (substr st1 1 1) 
                "\\"
             )
             (wcmatch (strcase (substr st1 2)) 
                      "U+[0-9A-F][0-9A-F][0-9A-F][0-9A-F]*"
             )
           )
           (setq st1 (substr st1 1 7)
                 i   (+ i 6)
           )
          )
          ((wcmatch st1 "[dDfFLHCI@-][1-9]*")
           (setq st1 (substr st1 1 1)
                 st1 (cond 
                       ((= st1 "d")
                        (&FLD "GJFH1")
                       )
                       ((= st1 "D")
                        (&FLD "GJFH2")
                       )
                       ((= st1 "f")
                        (&FLD "GJFH3")
                       )
                       ((= st1 "F")
                        (&FLD "GJFH4")
                       )
                       ((= st1 "L")
                        (&FLD "XG2")
                       )
                       ((= st1 "H")
                        (&FLD "XG4")
                       )
                       ((= st1 "C")
                        (&FLD "XG5")
                       )
                       ((= st1 "I")
                        (&FLD "XG3")
                       )
                       (T lnk)
                     )
           )
          )
          (T (setq st1 (substr st1 1 1)))
        )
        (setq stn (strcat stn st1)
              i   (1+ i)
        )
      )
      (setq str stn)
    )
  )
  str
)

(defun @run_exe (st) 
  (if 
    (> (&UTXT) 
       -1
    )
    (if (setq st (findfile st)) 
      (startapp st)
      (princ "\n**系统错误，无法找到相应资源!")
    )
  )
  (&TSTY)
)

(defun @tbox (e / ax ay dx dy el l p1 p2 p3 p4) 
  (setq el (entget e)
        l  (textbox el)
        dx (- (caadr l) 
              (caar l)
           )
        dy (- (cadadr l) 
              (cadar l)
           )
        p1 (&DRAG e 10)
        ax (&DRAG 50)
        ay (+ ax _pi2)
        p2 (polar p1 ax dx)
        p3 (polar p2 ay dy)
        p4 (polar p1 ay dy)
  )
  (list p1 p4 p3 p2 p1)
)

(defun @ofpl (l d / p1 p2 p3 p4 p ll tfc oft) 

  (defun oft (p1 p0 p2 d / a1 a2 p4) 
    (setq a1 (+ _pi2 (angle p1 p0))
          a2 (+ _pi2 (angle p0 p2))
          p1 (polar p1 a1 d)
          p4 (polar p0 a1 d)
          p0 (polar p0 a2 d)
          p2 (polar p2 a2 d)
          p0 (inters p1 p4 p0 p2 nil)
    )
  )
  (setq p1  (car l)
        p2  (cadr l)
        p3  (cadr (reverse l))
        p4  (last l)
        tfc (equal p1 p4 (@gpels 1))
        p   (if tfc 
              (oft p3 p1 p2 d)
              (polar p1 
                     (+ (angle p1 p2) 
                        _pi2
                     )
                     d
              )
            )
        ll  (cons p ll)
  )
  (foreach p3 (cddr l) 
    (setq ll (cons (oft p1 p2 p3 d) 
                   ll
             )
          p1 p2
          p2 p3
    )
  )
  (setq ll (cons 
             (if tfc 
               (last ll)
               (polar p4 
                      (+ (angle p3 p4) 
                         _pi2
                      )
                      d
               )
             )
             ll
           )
  )
  (reverse ll)
)

(defun @GetGrps (e / dxf el grp l ll) 
  (foreach dxf (entget e) 
    (if 
      (and 
        (= (car dxf) 
           330
        )
        (setq el (entget (cdr dxf)))
        (= "GROUP" (strcase (cdr (assoc 0 el))))
      )
      (progn (setq l nil) 
             (foreach grp el 
               (if 
                 (= (car grp) 
                    340
                 )
                 (setq l (cons (cdr grp) 
                               l
                         )
                 )
               )
             )
             (setq ll (cons 
                        (cons (cdr dxf) 
                              l
                        )
                        ll
                      )
             )
      )
    )
  )
  ll
)

(defun @Hlt (e no / l1) 
  (if 
    (and 
      (= (type e) 
         'ENAME
      )
      (member no '(1 2 3 4))
    )
    (progn (redraw e no) 
           (cond 
             ((= no 3)
              (if (listp le@Hlt) 
                (if (not (member e le@Hlt)) 
                  (setq le@Hlt (cons e le@Hlt))
                )
                (setq le@Hlt (list e))
              )
             )
             ((= no 4)
              (if 
                (and (listp le@Hlt) 
                     (setq l1 (member e le@Hlt))
                )
                (setq le@Hlt (append (reverse (cdr (member e (reverse le@Hlt)))) 
                                     (cdr l1)
                             )
                )
              )
             )
           )
           T
    )
    (princ "\n**【@Hlt(e no)】函数调用参数错误！")
  )
)

(defun @eUpd (e lu / el i l l1) 
  (foreach l (entget e) 
    (if 
      (setq i  (car l)
            l1 (cdr (assoc i lu))
      )
      (setq l  (cons i (car l1))
            lu (subst (cons i (cdr l1)) 
                      (cons i l1)
                      lu
               )
      )
    )
    (setq el (cons l el))
  )
  (entmod (reverse el))
)

(defun ss::del (ss / e si) 
  (setq si -1)
  (while 
    (setq si (1+ si)
          e  (ssname ss si)
    )
    (entdel e)
  )
)
