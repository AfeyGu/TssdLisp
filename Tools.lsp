(lib::lsp "MyFan.lsp")
(setfunhelp "c:Bbl" "Tssd" "bbl")
(Defun c:Bbl (/ newp shdcl upaxl updim upgrp uppln uptxt _sc _sd e ea eax eg en l lab 
              lac lac1 lax p0 sc sd ss ssx sx
             ) 
  (Defun newp (p p0) 
    (polar p0 (angle p0 p) (* sx (distance p p0)))
  )
  (defun shdcl (/ #flash #ret scn sco sdn sdo tgc tgd) 
    (defun #flash () 
      (mode_tile "sdo" (- 1 (atoi tgd)))
      (mode_tile "sdn" (- 1 (atoi tgd)))
      (mode_tile "txd" (- 1 (atoi tgd)))
      (mode_tile "sco" (- 1 (atoi tgc)))
      (mode_tile "scn" (- 1 (atoi tgc)))
      (mode_tile "txc" (- 1 (atoi tgc)))
    )
    (defun #ret (/) 
      (if 
        (or 
          (= tgc tgd "0")
          (and (= tgd "1") (= sdo sdn))
          (and (= tgc "1") (= sco scn))
        )
        (set_tile "error" "**请先改变对话框中的比例！")
        (progn 
          (&GLAY "Tssd/Dcl_Myj/chscl" (strcat tgd "\t" tgc))
          (if (= tgd "0") 
            (setq sdn -1
                  sdo -1
            )
          )
          (if (= tgc "0") 
            (setq scn -1
                  sco -1
            )
          )
          (done_dialog 1)
        )
      )
    )
    (if 
      (not 
        (and 
          (setq tgc (&GPTS "Tssd/Dcl_Myj/chscl"))
          (/= tgc "")
          (setq tgc (&PLCN tgc "\t"))
          (= (length tgc) 2)
        )
      )
      (setq tgc '("1" "0"))
    )
    (setq tgd (car tgc)
          tgc (cadr tgc)
    )
    (if (new_dialog "chscl" (lib::dcl "Tools")) 
      (progn 
        (&SWAP T)
        (setq sdo (fix (/ &sp &sc))
              sdn sdo
              sco &sp
              scn sco
        )
        (foreach x '("sdo" "sdn" "sco" "scn") 
          (set_tile x (itoa (eval (read x))))
          (action_tile x "(#isnum 0 10000)")
        )
        (set_tile "tgc" tgc)
        (set_tile "tgd" tgd)
        (#flash)
        (action_tile "tgc" "(setq tgc $value) (#flash)")
        (action_tile "tgd" "(setq tgd $value) (#flash)")
        (action_tile "help" "(help \"Tssd\" \"bbl\")")
        (action_tile "accept" "(#ret)")
        (if (= (start_dialog) 1) 
          (list (/ sdn 1. sdo) (/ scn 1. sco) sdn scn)
        )
      )
    )
  )
  (defun updim (/ e3 el p10 p11 p13 p13n p14 p14n p15 p15n sd1 sc1) 
    (setq el  (entget e)
          e3  (&DRAG 3)
          p10 (&DRAG 10)
          p11 (&DRAG 11)
          p13 (&DRAG 13)
          p14 (&DRAG 14)
    )
    (cond 
      ((wcmatch e3 "TSSD*")
       (setq e3 (&PLCN e3 "_"))
       (&GVAL 
         (if (minusp _sd) (atoi (cadr e3)) _sd)
         (if (minusp _sc) (atoi (caddr e3)) _sc)
       )
       (setq el (subst (cons 3 (getvar "dimstyle")) (assoc 3 el) el))
       (if (= sx 1) 
         (setq p13n p13
               p14n p14
         )
         (setq p13n (newp p13 p0)
               p14n (newp p14 p0)
               el   (subst (cons 13 (@wcs p13n)) (assoc 13 el) el)
               el   (subst (cons 14 (@wcs p14n)) (assoc 14 el) el)
         )
       )
       (if (< (distance p13 p10) (distance p14 p10)) 
         (setq p10 (polar p13n (angle p13 p10) (* sc (distance p13 p10))))
         (setq p10 (polar p14n (angle p14 p10) (* sc (distance p14 p10))))
       )
       (setq p13 (&N2S p13 p14)
             p14 (&N2S p13n p14n)
             p11 (polar p14 (angle p13 p11) (* sc (distance p13 p11)))
             el  (subst (cons 10 (@wcs p10)) (assoc 10 el) el)
             el  (subst (cons 11 (@wcs p11)) (assoc 11 el) el)
       )
       (entmod el)
      )
      ((wcmatch e3 "TSAN*")
       (setq e3 (&PLCN e3 "_"))
       (&GVAL 
         (if (minusp _sd) (atoi (cadr e3)) _sd)
         (if (minusp _sc) (atoi (caddr e3)) _sc)
         t
       )
       (setq el  (subst (cons 3 (getvar "dimstyle")) (assoc 3 el) el)
             p15 (&DRAG 15)
       )
       (if (= (logand (&DRAG 70) 5) 5) 
         (progn 
           (if (= sx 1) 
             (setq p15n p15)
             (setq p13n (newp p13 p0)
                   p14n (newp p14 p0)
                   p15n (newp p15 p0)
                   el   (subst (cons 13 (@wcs p13n)) (assoc 13 el) el)
                   el   (subst (cons 14 (@wcs p14n)) (assoc 14 el) el)
                   el   (subst (cons 15 (@wcs p15n)) (assoc 15 el) el)
             )
           )
           (setq p10 (polar 
                       p15n
                       (angle p15 p10)
                       (- 
                         (* sx (distance p15 p13))
                         (* sc (- (distance p15 p13) (distance p15 p10)))
                       )
                     )
                 p11 (polar 
                       p15n
                       (angle p15 p11)
                       (- 
                         (* sx (distance p15 p13))
                         (* sc (- (distance p15 p13) (distance p15 p11)))
                       )
                     )
                 el  (subst (cons 10 (@wcs p10)) (assoc 10 el) el)
                 el  (subst (cons 11 (@wcs p11)) (assoc 11 el) el)
           )
         )
         (if (/= sx 1) 
           (setq p13 (&N2S p10 p15)
                 p10 (newp p10 p0)
                 p15 (newp p15 p0)
                 p14 (&N2S p10 p15)
                 p11 (polar p14 (angle p13 p11) (* sc (distance p13 p11)))
                 el  (subst (cons 10 (@wcs p10)) (assoc 10 el) el)
                 el  (subst (cons 11 (@wcs p11)) (assoc 11 el) el)
                 el  (subst (cons 15 (@wcs p15)) (assoc 15 el) el)
           )
         )
       )
       (entmod el)
      )
      ((/= sx 1) (ssadd e ssx))
    )
  )
  (defun upaxl (/ e1 p10) 
    (setq p10 (&DRAG 10)
          e1  (entnext e)
    )
    (if 
      (and 
        (= (&DRAG e1 0) "INSERT")
        (wcmatch (strcase (&DRAG 2)) "TS_AXNO*")
      )
      (progn 
        (ssdel e1 ss)
        (if (member e1 eax) (setq eax (subst 'nil e1 eax)))
      )
      (setq e1 nil)
    )
    (if (/= sx 1) 
      (progn 
        (command ".move" e)
        (if e1 (command e1))
        (command "" p10 (setq p10 (newp p10 p0)))
      )
    )
    (if (/= sc 1) 
      (progn 
        (command ".scale" e)
        (if e1 (command e1))
        (command "" p10 sc)
      )
    )
  )
  (defun uptxt (p10) 
    (if (/= sx 1) 
      (command ".move" e "" p10 (setq p10 (newp p10 p0)))
    )
    (if (/= sc 1) (command ".scale" e "" p10 sc))
  )
  (defun uppln (/ el i p10 x xi) 
    (defun reinbf (e / sp r l rn ln ll lln p10l h42 pt10l enl) 
      (defun modewg (e el p10l pt10l / flag enl no p1 p2) 
        (if 
          (and 
            (equal (distance (car p10l) (cadr p10l)) l (* 0.5 l))
            (or 
              (equal (cadr h42) 1.0 0.001)
              (equal (cadr h42) -1.0 0.001)
            )
            (equal 
              (distance (cadr p10l) (caddr p10l))
              (* 2 r)
              (* 1.1 &sp (&GSYS "板筋宽度"))
            )
          )
          (progn 
            (setq flag 2
                  p2   (polar 
                         (caddr pt10l)
                         (angle (caddr pt10l) (cadr pt10l))
                         (* 2 rn)
                       )
                  p1   (polar p2 (angle (cadr pt10l) (car pt10l)) ln)
                  el   (subst (cons 10 p1) (assoc 10 el) el)
            )
            (while (>= flag 1) 
              (setq no (car el)
                    el (cdr el)
              )
              (if (= (car no) 10) (setq flag (1- flag)))
              (if (= flag 0) 
                (setq enl  (append enl (list (cons 10 p2)))
                      flag -1
                )
                (setq enl (append enl (list no)))
              )
            )
            (setq enl (append enl el))
          )
          (if 
            (and 
              (equal (distance (car p10l) (cadr p10l)) ll (* 0.5 l))
              (equal 
                (abs (&MIDP (car p10l) (cadr p10l) (caddr p10l)))
                (/ ll (sqrt 2.0))
                (* 0.01 _sc)
              )
            )
            (setq p1  (polar 
                        (cadr pt10l)
                        (angle (cadr pt10l) (car pt10l))
                        lln
                      )
                  enl (subst (cons 10 p1) (assoc 10 el) el)
            )
          )
        )
        enl
      )
      (if (= _sc -1) 
        (setq sp  &sp
              sca &sp
        )
        (setq sp  (/ _sc sc)
              sca _sc
        )
      )
      (setq r    (&GSYS "圆钩半径")
            l    (&GSYS "圆钩长度")
            ll   (&GSYS "直斜钩长")
            rn   (* sca r)
            r    (* sp r)
            ln   (* sca l)
            l    (* sp l)
            lln  (* sca ll)
            ll   (* sp ll)
            p10l (&DRAG e 10)
            h42  (&DRAG 42)
      )
      (command ".scale" e "" p0 sx)
      (setq pt10l (&DRAG e 10))
      (if (> (length p10l) 2) 
        (progn 
          (if (setq enl (modewg e (entget e) p10l pt10l)) 
            (entmod enl)
          )
          (if 
            (setq enl (reverse 
                        (modewg 
                          e
                          (reverse (entget e))
                          (reverse p10l)
                          (reverse pt10l)
                        )
                      )
            )
            (entmod enl)
          )
        )
      )
    )
    (setq p10 (&DRAG 10))
    (if 
      (and 
        (member (&DRAG 42) '((1.0 1.0) (-1.0 -1.0)))
        (equal (distance (car p10) (cadr p10)) (&DRAG 43) 1.0)
      )
      (progn 
        (setq p10 (&N2S (car p10) (cadr p10)))
        (if (/= sx 1) 
          (command ".move" e "" p10 (setq p10 (newp p10 p0)))
        )
        (if (/= sc 1) (command ".scale" e "" p10 sc))
      )
      (progn 
        (if (/= sx 1) (reinbf e))
        (if (/= sd 1) 
          (progn 
            (foreach x (entget e) 
              (if 
                (and 
                  (member (setq i (car x)) '(40 41 43))
                  (not (zerop (setq xi (cdr x))))
                )
                (if (= i 10) 
                  (setq x (cons i (@wcs (* xi sd))))
                  (setq x (cons i (* xi sd)))
                )
              )
              (setq el (cons x el))
            )
            (entmod (reverse el))
          )
        )
      )
    )
  )
  (defun upgrp (/ e en p10 ss1 tf) 
    (setq ss1 (ssadd))
    (foreach x eg 
      (if (= (car x) 340) 
        (progn 
          (setq e  (cdr x)
                en (&DRAG e 0)
          )
          (ssdel e ss)
          (cond 
            ((= en "DIMENSION") (updim) (setq tf t))
            ((and (= en "LWPOLYLINE") (> (&DRAG 43) 0))
             (uppln)
             (setq tf t)
            )
            (t
             (ssadd e ss1)
             (if (null p10) (setq p10 (&DRAG 10)))
             (if (wcmatch en "*TEXT,INSERT") (setq tf t))
            )
          )
        )
      )
    )
    (if (> (sslength ss1) 0) 
      (if tf 
        (progn 
          (if (/= sx 1) 
            (command ".move" ss1 "" p10 (setq p10 (newp p10 p0)))
          )
          (if (/= sc 1) (command ".scale" ss1 "" p10 sc))
        )
        (if (/= sx 1) (command ".scale" ss1 "" p0 sx))
      )
    )
  )
  (if 
    (and 
      (> (&UTXT) -1)
      (setq l (shdcl))
      (setq sd  (car l)
            sc  (cadr l)
            _sd (caddr l)
            _sc (cadddr l)
            sx  (/ sc sd)
      )
      (setq ss (&DSTR "\n选择要改变比例的实体<退出>: "))
      (setq p0 (&OSNP "\n点取比例缩放基点<退出>: "))
    )
    (progn 
      (setq lax  (&LJIG "平面轴线编号")
            lac  (&LJIG "平面柱子")
            lac1 (&LJIG "平面柱子虚线")
            lab  (&LJIG "图框")
            ssx  (ssadd)
      )
      (while (setq e (ssname ss 0)) 
        (setq en (&DRAG e 0)
              eg (&DRAG 330)
              ea (&DRAG 8)
        )
        (ssdel e ss)
        (cond 
          ((and 
             eg
             (setq eg (entget eg))
             (= (cdr (assoc 0 eg)) "GROUP")
           )
           (upgrp)
          )
          ((and (= ea lax) (= en "LINE")) (upaxl))
          ((= en "DIMENSION") (updim))
          ((and 
             (= en "INSERT")
             (wcmatch (strcase (&DRAG 2)) "TS_AXNO*")
           )
           (setq eax (cons e eax))
          )
          ((or 
             (= en "TEXT")
             (= en "MTEXT")
             (and 
               (= en "INSERT")
               (wcmatch 
                 (strcase (&DRAG 2))
                 "REIN_INDEX_BLOCK,COMPASS,TS_IDX*,TS_REINO*"
               )
             )
           )
           (uptxt (&DRAG 10))
          )
          ((and (= en "LWPOLYLINE") (/= ea lac) (/= ea lac1))
           (uppln)
          )
          ((= ea lab))
          ((/= sx 1) (ssadd e ssx))
        )
      )
      (foreach e eax (if e (uptxt (&DRAG e 10))))
      (if (> (sslength ssx) 0) 
        (command ".scale" ssx "" p0 sx)
      )
    )
  )
  (&TSTY)
)
(defun join2ln (la / 2pi a01 a02 a11 a12 e0 e1 la1 la2 mm mma msg ofr p01 p02 p1 p11 
                p12 p2 r0 r1 tf0 tf1
               ) 
  (if (< (&UTXT) 0) (exit))
  (setq ofr (getvar "filletrad")
        mm  0.1
        mma 0.01
        2pi (+ pi pi)
        msg (strcat 
              "\n选取第一根要连接的"
              (if (= la "*") "直线或圆弧" "梁线")
              "<退出>："
            )
  )
  (while 
    (and 
      (setq e0 (&DOVR msg (list '(0 . "line,arc") (cons 8 la))))
      (@Hlt (car e0) 3)
      (setq la1 (&DRAG (car e0) 8)
            la1 (cond 
                  ((= la "*") la1)
                  ((wcmatch la1 (setq la2 (&LJIG "平面主梁,平面主梁实线"))) la2)
                  ((wcmatch la1 (setq la2 (&LJIG "平面次梁,平面次梁实线"))) la2)
                  ((wcmatch la1 (setq la2 (&LJIG "平面过梁,平面过梁实线"))) la2)
                  ((wcmatch la1 (setq la2 (&LJIG "平面连梁,平面连梁实线"))) la2)
                )
            e1  (&DOVR 
                  "\t第二根<退出>："
                  (list '(0 . "line,arc") (cons 8 la1))
                )
      )
    )
    (if (not (equal (car e0) (car e1))) 
      (progn 
        (if 
          (setq p01 (&DRAG (car e0) 10)
                tf0 (= "LINE" (&DRAG 0))
          )
          (setq p02 (&DRAG 11))
          (setq r0  (&DRAG 40)
                a01 (&DRAG 50)
                a02 (&DRAG 51)
                a02 (if (< a02 a01) (+ a02 2pi) a02)
          )
        )
        (if 
          (setq p11 (&DRAG (car e1) 10)
                tf1 (= "LINE" (&DRAG 0))
          )
          (setq p12 (&DRAG 11))
          (setq r1  (&DRAG 40)
                a11 (&DRAG 50)
                a12 (&DRAG 51)
                a12 (if (< a12 a11) (+ a12 2pi) a12)
          )
        )
        (cond 
          ((and tf0 tf1)
           (if (equal (sin (- (angle p01 p02) (angle p11 p12))) 0 mma) 
             (if (equal (&MIDP p01 p11 p12) 0 mm) 
               (progn 
                 (setq r0 0
                       r1 0
                 )
                 (mapcar 
                   '(lambda (p1 p2) 
                      (if (> (setq r1 (distance p1 p2)) r0) 
                        (setq r0  r1
                              p01 p1
                              p02 p2
                        )
                      )
                    )
                   (list p01 p01 p01 p02 p02 p11)
                   (list p02 p11 p12 p11 p12 p12)
                 )
                 (setq e0 (entget (car e0))
                       e0 (subst (cons 10 (@wcs p01)) (assoc 10 e0) e0)
                       e0 (subst (cons 11 (@wcs p02)) (assoc 11 e0) e0)
                 )
                 (entdel (car e1))
                 (entmod e0)
               )
               (progn (princ "\n***两直线平行!") (@Hlt (car e0) 4))
             )
             (command ".fillet" "r" 0 ".fillet" e0 e1)
           )
          )
          ((or tf0 tf1) (command ".fillet" "r" 0 ".fillet" e0 e1))
          ((equal p01 p11 mm)
           (if (equal r0 r1 mm) 
             (progn 
               (setq r0 (min a01 a02 a11 a12)
                     r1 (max a01 a02 a11 a12)
                     e0 (entget (car e0))
                     e0 (subst (cons 50 (@wcs r0)) (assoc 50 e0) e0)
                     e0 (subst (cons 51 (@wcs r1)) (assoc 51 e0) e0)
               )
               (entdel (car e1))
               (entmod e0)
             )
             (progn (princ "\n**两圆弧共圆心, 但半径不同！") (@Hlt (car e0) 4))
           )
          )
          (t (command ".fillet" "r" 0 ".fillet" e0 e1))
        )
      )
      (progn (princ "\n**不能选取同一个实体！") (@Hlt (car e0) 4))
    )
  )
  (if e0 (@Hlt (car e0) 4))
  (setvar "filletrad" ofr)
  (&TSTY)
)
(setfunhelp "c:Ljxd" "Tssd" "LJXD")
(defun c:Ljxd () (join2ln "*"))
(setfunhelp "c:LJLX" "Tssd" "LJLX")
(defun C:Ljlx () 
  (join2ln 
    (&LJIG "平面主梁,平面主梁实线,平面次梁,平面次梁实线,平面过梁,平面过梁实线,平面连梁,平面连梁实线")
  )
)
(defun DrawPdx (p1 p6 / a d p2 p3 p4 p5 sc) 
  (if (and p1 p6) 
    (progn 
      (&LJIG "细线符号" T)
      (setvar "plinewid" 0)
      (if (< (distance p1 p6) (* 4 &sp)) 
        (setq sc (* 0.002 (distance p1 p6)))
        (setq sc (* 0.01 &sp))
      )
      (setq d  (* 0.5 (distance p1 p6))
            a  (angle p1 p6)
            p2 (polar p1 a (- d (* sc 130)))
            p5 (polar p6 a (- (* sc 130) d))
            p3 (polar (polar p2 a (* sc 70)) (+ a _pi2) (* sc 220))
            p4 (polar (polar p5 a (* sc -70)) (+ a _pi2) (* sc -220))
            p1 (polar p1 a (* sc -280))
            p6 (polar p6 a (* sc 280))
      )
      (command ".pline" p1 p2 p3 p4 p5 p6 "")
    )
  )
)
(setfunhelp "c:Pdx" "Tssd" "pdx")
(defun c:Pdx (/ e p1 p2) 
  (if (> (&UTXT) -1) 
    (progn 
      (if (setq p1 (&OSNP "\n点取剖断线的起点<选直线>: ")) 
        (setq p2 (&OSNP p1 "\t终点<退出>: "))
        (if (setq e (&DOVR "\n选取剖断线的基线<退出>: " '((0 . "LINE")))) 
          (progn 
            (setq p1 (&DRAG 10)
                  p2 (&DRAG 11)
            )
            (entdel (car e))
          )
        )
      )
      (if (and p1 p2) (DrawPdx p1 p2))
    )
  )
  (&TSTY)
)
(setfunhelp "c:ljfh" "Tssd" "ljfh")
(defun c:ljfh (/ a p1 p2 pa pb sc) 
  (if 
    (and 
      (> (&UTXT) -1)
      (setq p1 (&OSNP "\n点取剖断线的起点<退出>: "))
      (setq p2 (&OSNP p1 "\t终点<退出>: "))
    )
    (progn 
      (setq sc (* 1. &sp)
            a  (+ (angle p1 p2) _pi2)
            pa (polar p1 a sc)
            pb (polar p2 a sc)
      )
      (DrawPdx pa pb)
      (setq pa (polar p1 a (- sc))
            pb (polar p2 a (- sc))
      )
      (DrawPdx pa pb)
    )
  )
  (&TSTY)
)
(setfunhelp "c:Jddd" "Tssd" "JDDD")
(defun c:Jddd (/ *error* ints getcon en ent pt ptl ss del p0 p1 p2 pint pintl en_l) 
  (defun *error* (msg) 
    (if 
      (not 
        (or 
          (= msg "Function cancelled")
          (= msg "quit/exit abort")
        )
      )
      (if del (ss::del del))
    )
    (&TSTY)
  )
  (defun inst (s en / l i co sdel e e1 p pl ar) 
    (setq l    (sslength s)
          l    (1- l)
          co   (getvar "cecolor")
          sdel (ssadd)
    )
    (setvar "cecolor" "2")
    (while (setq e (ssname s l)) 
      (setq pl (&ORDR e en))
      (while (setq p (car pl)) 
        (setq pl (cdr pl))
        (command 
          ".circle"
          p
          (* 0.006 (getvar "viewsize"))
          ".circle"
          p
          (* 0.01 (getvar "viewsize"))
        )
      )
      (setq l (1- l))
    )
    (while (entnext ent) 
      (ssadd (setq ent (entnext ent)) sdel)
    )
    (setvar "cecolor" co)
    (if (> (sslength sdel) 0) sdel nil)
  )
  (defun getcon (en / flag p10 p11 ptl pc r qi zh ang p0 p1 p2 p3 px py) 
    (setq flag (strcase (&DRAG en 0)))
    (if (wcmatch flag "LINE") 
      (setq p10 (&DRAG 10)
            p11 (&DRAG 11)
            ang (angle p10 p11)
            ptl (list 
                  (polar 
                    p10
                    (+ ang (* 0.5 pi))
                    (* 0.01 (getvar "viewsize"))
                  )
                  (polar 
                    p10
                    (- ang (* 0.5 pi))
                    (* 0.01 (getvar "viewsize"))
                  )
                  (polar 
                    p11
                    (- ang (* 0.5 pi))
                    (* 0.01 (getvar "viewsize"))
                  )
                  (polar 
                    p11
                    (+ ang (* 0.5 pi))
                    (* 0.01 (getvar "viewsize"))
                  )
                )
      )
      (if (wcmatch flag "ARC") 
        (progn 
          (setq pc  (&DRAG en 10)
                r   (&DRAG 40)
                qi  (&DRAG 50)
                zh  (&DRAG 51)
                p0  (polar pc qi r)
                p3  (polar pc zh r)
                ang (- zh qi)
          )
          (if (< ang 0) (setq ang (+ ang pi pi)))
          (setq ang (+ qi (* 0.5 ang))
                p1  (polar 
                      (polar pc ang r)
                      (angle p3 p0)
                      (* 0.5 (distance p0 p3))
                    )
                p2  (polar p1 (angle p0 p3) (distance p0 p3))
                ptl (list p0 p1 p2 p3)
          )
        )
        (if (wcmatch flag "LWPOLYLINE") 
          (progn 
            (setq p10 (&DRAG en 10))
            (while (setq p0 (car p10)) 
              (setq p10 (cdr p10)
                    px  (cons (car p0) px)
                    py  (cons (cadr p0) py)
              )
            )
            (setq px  (@ran1 px)
                  py  (@ran1 py)
                  ptl (list 
                        (list (car px) (car py))
                        (list (last px) (car py))
                        (list (last px) (last py))
                        (list (car px) (last py))
                      )
            )
          )
        )
      )
    )
    ptl
  )
  (if (< (&UTXT) 0) (exit))
  (if (setq en (entsel "\n选取需要打断的实体<退出>: ")) 
    (progn 
      (setq pt   (cadr en)
            en   (car en)
            ptl  (getcon en)
            en_l (cons en en_l)
      )
      (if ptl 
        (if (setq ss (ssget "cp" ptl '((0 . "line,lwpolyline,arc")))) 
          (progn 
            (setq ent (entlast)
                  del (inst ss en)
            )
            (while (setq p0 (&OSNP "\n在实体上选取要打断的点<退出>: ")) 
              (setq p1 (polar p0 (* 0.25 pi) (* 0.006 (getvar "viewsize")))
                    p2 (polar p0 (* 1.25 pi) (* 0.006 (getvar "viewsize")))
              )
              (if (setq ss (ssget "c" p1 p2 '((0 . "line,lwpolyline,arc")))) 
                (while (setq ent (ssname ss 0)) 
                  (ssdel ent ss)
                  (setq l (length en_l))
                  (while (> l 0) 
                    (setq l     (1- l)
                          en    (nth l en_l)
                          pintl (&ORDR en ent)
                    )
                    (while (setq pint (car pintl)) 
                      (setq pintl (cdr pintl))
                      (if (< (distance pint p0) (* 0.0065 (getvar "viewsize"))) 
                        (progn 
                          (command ".Break" en pint pint)
                          (setq en_l (cons (entlast) en_l)
                                l    0
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
            (ss::del del)
          )
        )
      )
    )
  )
  (&TSTY)
)
(setfunhelp "c:Jdjc" "Tssd" "JDjc")
(defun c:Jdjc () 
  (if (> (&UTXT) -1) 
    (progn 
      (command "trim" "" "E" "N")
      (while (/= (getvar "cmdactive") 0) 
        (princ "\r选择要剪裁的实体<退出>: ")
        (command pause)
      )
    )
  )
  (&TSTY)
)
(setfunhelp "c:Hjt" "Tssd" "HJT")
(defun c:Hjt (/ p1 p2 p3 asz blk) 
  (if 
    (and 
      (> (&UTXT) -1)
      (setq p1 (&OSNP "\n点取箭头的第一点<退出>: "))
      (setq p2 (&OSNP p1 "\t第二点<退出>: \n"))
    )
    (progn 
      (&LJIG "细线符号" t)
      (setq asz (getvar "Dimasz"))
      (setvar "Dimasz" (* 2 &sp))
      (if (< (distance p1 p2) (* 4.2 &sp)) 
        (setq p2 (polar p1 (angle p1 p2) (* 4.2 &sp)))
      )
      (command ".Leader" p1 p2)
      (grdraw p1 p2 7)
      (while (setq p3 (&OSNP p2 "\r下一点<结束>: ")) 
        (command p3)
        (grdraw p2 p3 7)
        (setq p2 p3)
      )
      (command "A" "" "N")
      (setvar "Dimasz" asz)
    )
  )
  (&TSTY)
)
(setfunhelp "c:Lxfzh" "Tssd" "LXFZH")
(Defun c:Lxfzh (/ e ky p) 
  (if (> (&UTXT) -1) 
    (progn 
      (setq ky (&OSNP "\n输入复制距离<通过方式>: " 0)
            ky (if ky ky "T")
      )
      (while 
        (setq e (&DOVR 
                  "\n选择要连续复制的实体<退出>: \n"
                  '((0 . "line,arc,circle,lwpolyline,text,mtext,insert"))
                )
        )
        (setq e (car e))
        (@Hlt e 3)
        (while 
          (if (= ky "T") 
            (progn 
              (initget 128)
              (setq p (&OSNP "\r点取复制目标点或 输入复制距离<结束>: "))
            )
            (progn 
              (initget "T" 128)
              (setq p (&OSNP 
                        (strcat "\r当前距离=" (&RTXT ky) ", 点取方向或输距离 [通过方式(T)]<结束>: ")
                      )
              )
            )
          )
          (cond 
            ((listp p)
             (@Hlt e 4)
             (command ".offset" ky e p "")
             (setq e (entlast))
             (@Hlt e 3)
            )
            ((= (read p) (atof p))
             (setq ky (atof p))
             (princ "\n")
            )
            ((= p "T") (setq ky p) (princ "\n"))
            (t (prompt "\n**输入错误, 再试一次！\n"))
          )
        )
        (@Hlt e 4)
      )
    )
  )
  (&TSTY)
)
(setfunhelp "c:Txyh" "Tssd" "TXYH")
(defun c:Txyh (/ e si ss st chone mode_dim) 
  (defun chone (e tf / e1 i ix) 
    (setq e (entget e))
    (foreach x e 
      (setq i  (car x)
            ix (cdr x)
      )
      (cond 
        ((and tf (= i 8))
         (setq e1 (cons '(62 . 7) (cons x e1)))
        )
        ((and tf (= i 62)) nil)
        ((and (null tf) (< 9 i 20) (cddr ix))
         (setq e1 (cons (list i (car ix) (cadr ix) 0.0) e1))
        )
        (t (setq e1 (cons x e1)))
      )
    )
    (setq e1 (reverse e1))
    (if (not (equal e e1)) (entmod e1))
  )
  (defun mode_dim (/ flag diml) 
    (setq flag T)
    (while (setq diml (tblnext "dimstyle" flag)) 
      (setq flag nil)
      (if 
        (and 
          (wcmatch "ARCHTICK" (strcase (cdr (assoc 5 diml))))
          (not 
            (or 
              (wcmatch (strcase (cdr (assoc 2 diml))) "TSSD*")
              (wcmatch (strcase (cdr (assoc 2 diml))) "TSSD*")
            )
          )
          (not (equal (cdr (assoc 41 diml)) &sp (* 0.01 &sp)))
        )
        (command 
          "dim"
          "dimasz"
          &sp
          "save"
          (cdr (assoc 2 diml))
          "y"
          "exit"
        )
      )
    )
  )
  (if (and (> (&UTXT) -1) (setq ss (ssget "x"))) 
    (progn 
      (command 
        ".Insert"
        "_ArchTick=_Tsdbk1"
        (list 0. 0. 0.)
        1
        1
        0
      )
      (entdel (entlast))
      (repeat 3 
        (command ".purge" "all" "*")
        (while (not (zerop (getvar "cmdactive"))) 
          (command "y")
        )
      )
      (mode_dim)
      (setq tf nil
            si 0
            st (strcat "\r图中共有" (itoa (sslength ss)) "个实体, 已经处理")
      )
      (while (setq e (ssname ss si)) 
        (setq si (1+ si))
        (if (zerop (rem si 50)) 
          (princ (strcat st "：" (itoa si)))
        )
        (chone e tf)
        (while (and (setq e (entnext e)) (= (&DRAG e 0) "ATTRIB")) 
          (chone e tf)
        )
      )
      (if (> si 0) (princ (strcat st "完毕！    \n")))
      (setq si 0
            e  (tblnext "block" t)
            st "\r正在处理图块: "
      )
      (while e 
        (if (wcmatch (cdr (assoc 2 e)) "~[*]*") 
          (progn 
            (setq e (cdr (assoc -2 e)))
            (while e 
              (chone e tf)
              (setq e  (entnext e)
                    si (1+ si)
              )
              (if (zerop (rem si 50)) (princ (strcat st (itoa si))))
            )
          )
        )
        (setq e (tblnext "block"))
      )
      (if (> si 0) (princ "\r图块处理完毕！   \n"))
      (if tf (command ".zoom" "e" ".zoom" "p"))
    )
  )
  (command ".regen")
  (&TSTY)
)
(setfunhelp "c:Qtds" "Tssd" "QTDS")
(defun c:Qtds (/ e si ss st chone) 
  (defun chone (e tf / e1 i ix) 
    (setq e (entget e))
    (foreach x e 
      (setq i  (car x)
            ix (cdr x)
      )
      (cond 
        ((and tf (= i 8))
         (setq e1 (cons '(62 . 7) (cons x e1)))
        )
        ((and tf (= i 62)) nil)
        ((and (null tf) (< 9 i 20) (cddr ix))
         (setq e1 (cons (list i (car ix) (cadr ix) 0.0) e1))
        )
        (t (setq e1 (cons x e1)))
      )
    )
    (setq e1 (reverse e1))
    (if (not (equal e e1)) (entmod e1))
  )
  (if (and (> (&UTXT) -1) (setq ss (ssget "x"))) 
    (progn 
      (setq tf T
            si 0
            st (strcat "\r图中共有" (itoa (sslength ss)) "个实体, 已经处理")
      )
      (while (setq e (ssname ss si)) 
        (setq si (1+ si))
        (if (zerop (rem si 50)) 
          (princ (strcat st "：" (itoa si)))
        )
        (chone e tf)
        (while (and (setq e (entnext e)) (= (&DRAG e 0) "ATTRIB")) 
          (chone e tf)
        )
      )
      (if (> si 0) (princ (strcat st "完毕！    \n")))
      (setq si 0
            e  (tblnext "block" t)
            st "\r正在处理图块: "
      )
      (while e 
        (if (wcmatch (cdr (assoc 2 e)) "~[*]*") 
          (progn 
            (setq e (cdr (assoc -2 e)))
            (while e 
              (chone e tf)
              (setq e  (entnext e)
                    si (1+ si)
              )
              (if (zerop (rem si 50)) (princ (strcat st (itoa si))))
            )
          )
        )
        (setq e (tblnext "block"))
      )
      (if (> si 0) (princ "\r图块处理完毕！   \n"))
      (if tf 
        (command ".regen")
        (command ".zoom" "e" ".zoom" "p")
      )
    )
  )
  (&TSTY)
)
(setfunhelp "c:Xchsht" "Tssd" "XCHSHT")
(defun c:Xchsht (/ ss smx e el el0 n n1 n2 i i1 stl st1 st2 name la p ln ln2 le le1 
                 lp 3dface attdef arc circle dimension insert line lwpolyline point 
                 polyline solid text trace
                ) 
  (if 
    (and 
      (> (&UTXT) -1)
      (if (setq ss (&DSTR "\n选择要进行重叠处理的实体<全部>: ")) 
        T
        (setq ss (ssget "x"))
      )
    )
    (progn 
      (setq st1 (strcat "\r已搜索到实体 ")
            st2 (strcat "\r已处理实体 ")
            n   0
            n1  0
            n2  0
            i   0
            smx (sslength ss)
            ln  '(3dface attdef arc circle dimension insert line lwpolyline point 
                  solid text trace
                 )
      )
      (while (< n smx) 
        (setq e    (ssname ss n)
              n    (1+ n)
              la   (&DRAG e 8)
              name (&DRAG 0)
              p    (cond 
                     ((= "POLYLINE" name) (&DRAG (entnext e) 10))
                     ((= "LWPOLYLINE" name) (car (&DRAG 10)))
                     (t (&DRAG 10))
                   )
              name (read name)
              la   (strcat la (rtos (car p) 2 0) (rtos (cadr p) 2 0))
              le   (assoc la (eval name))
        )
        (set 
          name
          (if le 
            (subst (append le (list e)) le (eval name))
            (cons (list la e) (eval name))
          )
        )
        (if (= 127 (logand 127 n)) 
          (princ (strcat st1 (itoa n)))
        )
      )
      (princ (strcat st1 (itoa n) "\n"))
      (foreach ln1 ln 
        (setq ln2 (eval ln1))
        (foreach le ln2 
          (setq le (cdr le))
          (while (setq e (car le)) 
            (if 
              (setq n1 (1+ n1)
                    le (cdr le)
              )
              (progn 
                (setq el0 (entget e)
                      el0 (member (assoc 10 el0) el0)
                      le1 nil
                )
                (foreach e le 
                  (setq el (entget e)
                        el (member (assoc 10 el) el)
                  )
                  (if (equal el el0) 
                    (progn (entdel e) 
                           (setq n1 (1+ n1)
                                 n2 (1+ n2)
                           )
                    )
                    (setq le1 (cons e le1))
                  )
                )
                (setq le le1)
              )
            )
          )
          (if (> i 100) 
            (progn (princ (strcat st2 (itoa n1))) (setq i 0))
            (setq i (1+ i))
          )
        )
      )
      (foreach le polyline 
        (setq le (cdr le))
        (while (setq e (car le)) 
          (if 
            (setq n1 (1+ n1)
                  le (cdr le)
            )
            (progn 
              (setq lp  nil
                    le1 nil
              )
              (while 
                (setq e (entnext e)
                      p (&DRAG e 10)
                )
                (setq lp (cons p lp))
              )
              (setq lp (reverse lp))
              (foreach e le 
                (setq i1 0)
                (while 
                  (and 
                    (setq e (entnext e)
                          p (&DRAG e 10)
                    )
                    (equal p (nth i1 lp))
                  )
                  (setq i1 (1+ i1))
                )
                (if p 
                  (setq le1 (cons e le1))
                  (progn (entdel e) 
                         (setq n1 (1+ n1)
                               n2 (1+ n2)
                         )
                  )
                )
              )
              (setq le le1)
            )
          )
        )
        (if (> i 100) 
          (progn (princ (strcat st2 (itoa n1))) (setq i 0))
          (setq i (1+ i))
        )
      )
      (princ (strcat st2 (itoa n1)))
      (princ 
        (strcat "\n已消去实体 " (itoa n2) ", 还剩 " (itoa (- n n2)))
      )
      (redraw)
    )
  )
  (&TSTY)
)
(setfunhelp "c:Tchgs" "Tssd" "TCHGS")
(defun c:Tchgs (/ col e el si ss) 
  (if 
    (and 
      (> (&UTXT) -1)
      (setq ss (&DSTR "\n选择要编辑的填充<退出>: " '((0 . "hatch"))))
      (setq col (acad_colordlg 256))
    )
    (progn 
      (setq si  0
            col (cons 62 col)
      )
      (while (setq e (ssname ss si)) 
        (setq si (1+ si)
              el (entget e)
              el (if (&DRAG e 62) 
                   (subst col (assoc 62 el) el)
                   (append el (list col))
                 )
        )
        (entmod el)
      )
    )
  )
  (&TSTY)
)
(setfunhelp "c:Syfh" "Tssd" "SYFH")
(defun c:Syfh (/ shdcl insidx str what) 
  (defun chsh_dcl (/ vslide dia no) 
    (defun vslide (no / x y i img name) 
      (setq i 1)
      (while (< i 7) 
        (setq img  (strcat "img" (itoa i))
              name (strcat "Tssd(syfh" (itoa i) ")")
              x    (dimx_tile img)
              y    (dimy_tile img)
              i    (1+ i)
        )
        (start_image img)
        (fill_image 0 0 x y -2)
        (slide_image 0 0 x (- y 20) name)
        (end_image)
      )
      (mode_tile (strcat "img" (itoa no)) 4)
      (if (or (= no 5) (= no 6)) 
        (progn 
          (mode_tile "xs" 0)
          (mode_tile "xx" 0)
          (set_tile "txt" "索引图号:")
        )
        (progn 
          (mode_tile "xs" 1)
          (mode_tile "xx" 1)
          (set_tile "txt" "大样图号:")
        )
      )
      (if (or (= no 1) (= no 3)) 
        (progn (mode_tile "dy" 1) (set_tile "txt" ""))
        (mode_tile "dy" 0)
      )
      (if 
        (or 
          (= "" (get_tile "dy"))
          (= " " (get_tile "dy"))
          (= "  " (get_tile "dy"))
        )
        (set_tile "dy" "-")
      )
      (if 
        (or 
          (= "" (get_tile "jd"))
          (= " " (get_tile "jd"))
          (= "  " (get_tile "jd"))
          (= "-" (get_tile "jd"))
        )
        (set_tile "jd" (cadr str))
      )
      (setq str (list 
                  (itoa no)
                  (get_tile "jd")
                  (get_tile "dy")
                  (get_tile "xs")
                  (get_tile "xx")
                )
      )
    )
    (setq what 2
          dia  (lib::dcl "Tools")
          str  (&GPTS "Tssd/Dcl_Hzh/Syfh")
    )
    (if (or (not str) (= str "")) 
      (setq str (list "1" "1" "10" "" ""))
      (setq str (&PLCN str "\t"))
    )
    (while (> what 0) 
      (if (not (new_dialog "Syfh" dia)) (exit))
      (mapcar 'set_tile '("id" "dy" "xs" "xx") (cdr str))
      (vslide (setq no (atoi (car str))))
      (action_tile "img1" "(setq no 1)(vslide no)")
      (action_tile "img2" "(setq no 2)(vslide no)")
      (action_tile "img3" "(setq no 3)(vslide no)")
      (action_tile "img4" "(setq no 4)(vslide no)")
      (action_tile "img5" "(setq no 5)(vslide no)")
      (action_tile "img6" "(setq no 6)(vslide no)")
      (action_tile 
        "accept"
        "(vslide no)(done_dialog 0)(setq what -8)"
      )
      (action_tile "cancel" "(done_dialog 0)(setq what -2)")
      (action_tile "help" "(help \"Tssd\" \"Syfh\")")
      (start_dialog)
    )
    (unload_dialog dia)
  )
  (defun insidx (/ ot p1 p2 p3 sgrp th en) 
    (&GLAY 
      "Tssd/Dcl_Hzh/Syfh"
      (strcat 
        (car str)
        "\t"
        (cadr str)
        "\t"
        (caddr str)
        "\t"
        (cadddr str)
        "\t"
        (nth 4 str)
      )
    )
    (setq ot   (getvar "orthomode")
          sgrp (ssadd)
    )
    (setvar "orthomode" 0)
    (&LJIG "细线符号" t)
    (if (or (= "5" (car str)) (= "6" (car str))) 
      (progn 
        (if 
          (and 
            (setq p1 (&OSNP "\n点取引出线基点<退出>: "))
            (setq p2 (&OSNP p1 "\t转折点<退出>: "))
          )
          (progn 
            (setvar "orthomode" 1)
            (command ".line" p1 p2 "")
            (ssadd (setq en (entlast)) sgrp)
            (if (= "6" (car str)) 
              (progn 
                (if 
                  (and 
                    (> (angle p1 p2) (* 0.5 pi))
                    (< (angle p1 p2) (* 1.5 pi))
                  )
                  (setq ang (- (angle p1 p2) (* 0.5 pi)))
                  (setq ang (+ (angle p1 p2) (* 0.5 pi)))
                )
                (command 
                  ".Pline"
                  (polar p1 ang (* 1.2 &sp))
                  "w"
                  (* &sp (&GSYS "多义线宽度"))
                  ""
                  (polar 
                    (polar p1 (angle p1 p2) (* 5 &sp))
                    ang
                    (* 1.2 &sp)
                  )
                  ""
                )
              )
            )
            (ssadd (setq en (entlast)) sgrp)
            (if (setq p3 (&OSNP p2 "\t点取引出线的终点<结束>: ")) 
              (progn 
                (setq p1 p2
                      p2 p3
                )
                (command ".line" p1 p2 "")
              )
            )
            (setq th  (&INTS "标号文字")
                  ang (/ (* 180 (angle p2 p1)) pi)
            )
            (if (and (>= ang 90) (< ang 180)) 
              (setq ang (+ ang 180))
              (if (and (>= ang 180) (<= ang 270)) 
                (setq ang (- ang 180))
              )
            )
            (if 
              (and 
                (> (angle p1 p2) (* 0.5 pi))
                (< (angle p1 p2) (* 1.5 pi))
              )
              (progn 
                (if 
                  (not 
                    (or 
                      (= (nth 3 str) "")
                      (= (nth 3 str) " ")
                      (= (nth 3 str) "  ")
                    )
                  )
                  (command 
                    ".Text"
                    (polar 
                      (polar p2 (angle p2 p1) (/ th 3))
                      (- (angle p1 p2) (* 0.5 pi))
                      (/ th 3)
                    )
                    th
                    ang
                    (nth 3 str)
                  )
                )
                (if 
                  (not 
                    (or 
                      (= (nth 4 str) "")
                      (= (nth 4 str) " ")
                      (= (nth 4 str) "  ")
                    )
                  )
                  (command 
                    ".Text"
                    "J"
                    "TL"
                    (polar 
                      (polar p2 (angle p2 p1) (/ th 3))
                      (+ (angle p1 p2) (* 0.5 pi))
                      (/ th 3)
                    )
                    th
                    ang
                    (nth 4 str)
                  )
                )
              )
              (progn 
                (if 
                  (not 
                    (or 
                      (= (nth 3 str) "")
                      (= (nth 3 str) " ")
                      (= (nth 3 str) "  ")
                    )
                  )
                  (command 
                    ".Text"
                    "J"
                    "BR"
                    (polar 
                      (polar p2 (angle p2 p1) (/ th 3))
                      (+ (angle p1 p2) (* 0.5 pi))
                      (/ th 3)
                    )
                    th
                    ang
                    (nth 3 str)
                  )
                )
                (if 
                  (not 
                    (or 
                      (= (nth 4 str) "")
                      (= (nth 4 str) " ")
                      (= (nth 4 str) "  ")
                    )
                  )
                  (command 
                    ".Text"
                    "J"
                    "TR"
                    (polar 
                      (polar p2 (angle p2 p1) (/ th 3))
                      (- (angle p1 p2) (* 0.5 pi))
                      (/ th 3)
                    )
                    th
                    ang
                    (nth 4 str)
                  )
                )
              )
            )
            (&FIND 
              3
              p2
              (angle p1 p2)
              0
              "细线符号"
              (cadr str)
              (caddr str)
            )
            (while (setq en (entnext en)) (ssadd en sgrp))
            (&DGAR sgrp)
          )
        )
      )
      (if (setq p1 (&OSNP "\n点取索引号的位置<退出>: ")) 
        (progn 
          (if (= "1" (car str)) 
            (&FIND 4 p1 -1 0 "细线符号" (cadr str))
          )
          (if (= "3" (car str)) 
            (&FIND 5 p1 -1 0 "细线符号" (cadr str))
          )
          (if (= "2" (car str)) 
            (&FIND 6 p1 -1 0 "细线符号" (cadr str) (caddr str))
          )
          (if (= "4" (car str)) 
            (&FIND 7 p1 -1 0 "细线符号" (cadr str) (caddr str))
          )
        )
      )
    )
    (setvar "orthomode" ot)
  )
  (if (< (&UTXT) 0) (exit))
  (chsh_dcl)
  (if (= what -8) (insidx))
  (&TSTY)
)
(setfunhelp "c:Mjhzh" "Tssd" "MJHZH")
(Defun c:Mjhzh (/ l insbury shdcl) 
  (defun shdcl (/ #chk #shimg dc dl dw dx dy du dd h0 w0) 
    (defun #shimg (/ mx my sc x0 x1 x2 y0 y1 y2) 
      (start_image "img")
      (fill_image 0 0 w0 h0 -2)
      (setq sc (max dl dw)
            sc (/ w0 3. sc (+ 2 (/ (max (abs dx) (abs dy)) sc 4)))
            x0 (fix (+ (* w0 0.5) (* dx sc)))
            y0 (fix (- (* h0 0.5) (* dy sc)))
            mx (fix (* dl sc 0.5))
            x1 (- x0 mx)
            x2 (+ x0 mx)
            my (fix (* dw sc 0.5))
            y1 (- y0 my)
            y2 (+ y0 my)
      )
      (if (= tf "0") 
        (progn 
          (vector_image x1 y1 x2 y1 2)
          (vector_image x1 y2 x2 y2 2)
          (vector_image x1 y1 x1 y2 2)
          (vector_image x2 y1 x2 y2 2)
        )
        (fill_image x1 y1 (+ mx mx) (+ my my) 2)
      )
      (vector_image (- x0 5) (- y0 5) (+ x0 5) (+ y0 5) 3)
      (vector_image (- x0 5) (+ y0 5) (+ x0 5) (- y0 5) 3)
      (vector_image (/ w0 2) 0 (/ w0 2) h0 1)
      (vector_image 0 (/ h0 2) w0 (/ h0 2) 1)
      (if (= tt "1") 
        (progn 
          (vector_image (- x1 10) y0 (+ x2 10) y0 7)
          (vector_image x0 (- y1 10) x0 (+ y2 10) 7)
        )
      )
      (if (or (read du) (read dd)) 
        (progn 
          (vector_image x0 (+ y1 10) (+ x2 10) (- y1 10) 6)
          (vector_image 
            (+ x2 10)
            (- y1 10)
            (+ x2 40)
            (- y1 10)
            6
          )
        )
      )
      (if (read du) 
        (vector_image 
          (+ x2 15)
          (- y1 15)
          (+ x2 35)
          (- y1 15)
          7
        )
      )
      (if (read dd) 
        (vector_image (+ x2 15) (- y1 5) (+ x2 35) (- y1 5) 7)
      )
      (end_image)
    )
    (defun #chk () 
      (cond 
        ((or (= $key "dl") (= $key "dw"))
         (if (#isnum 0 10000.0) (#shimg))
        )
        ((or (= $key "dx") (= $key "dy"))
         (if (#isnum -10000.0 10000.0) 
           (progn 
             (#shimg)
             (set_tile (strcat "s" (@substr $key 2 1)) $value)
           )
         )
        )
        ((= $key "du") (setq du $value) (#shimg))
        ((= $key "dd") (setq dd $value) (#shimg))
        (t (#isnum 0 1000.0))
      )
    )
    (if (new_dialog "bury" (lib::dcl "Tools")) 
      (progn 
        (if 
          (not 
            (and 
              (setq dc (&GPTS "Tssd/Dcl_Myj/bury"))
              (/= dc "")
              (setq dc (&PLCN dc "\t"))
              (= (length dc) 8)
            )
          )
          (setq dc '("0" "1" "300" "300" "0" "0" "\"\"" "\"\""))
        )
        (mapcar 'set '(tt tf dl dw dx dy du dd) dc)
        (setq du (read du)
              dd (read dd)
        )
        (mapcar 'set_tile 
                '("tt" "tf" "dl" "dw" "dx" "dy" "du" "dd")
                (list tt tf dl dw dx dy du dd)
        )
        (set_tile "sx" dx)
        (set_tile "sy" dy)
        (setq dc (car (&SWAP t))
              dl (atoi dl)
              dw (atoi dw)
              dx (atoi dx)
              dy (atoi dy)
              w0 (dimx_tile "img")
              h0 (dimy_tile "img")
        )
        (set_tile "dc" (&RTXT dc))
        (#shimg)
        (action_tile "tt" "(setq tt $value) (#shimg)")
        (action_tile "tf" "(setq tf $value) (#shimg)")
        (foreach x '("dc" "dl" "dw" "dx" "dy" "du" "dd") 
          (action_tile x "(#chk)")
        )
        (action_tile 
          "sx"
          "(set_tile \"dx\" $value) (setq dx (atoi $value)) (#shimg)"
        )
        (action_tile 
          "sy"
          "(set_tile \"dy\" $value) (setq dy (atoi $value)) (#shimg)"
        )
        (action_tile "help" "(help \"Tssd\" \"MJHZH\")")
        (if (= (start_dialog) 1) 
          (progn 
            (&GLAY 
              "Tssd/Dcl_Myj/bury"
              (strcat 
                tt
                "\t"
                tf
                "\t"
                (&RTXT dl)
                "\t"
                (&RTXT dw)
                "\t"
                (&RTXT dx)
                "\t"
                (&RTXT dy)
                "\t\""
                du
                "\"\t\""
                dd
                "\""
              )
            )
            (list (= tt "1") (= tf "1") dc dl dw dx dy du dd)
          )
        )
      )
    )
  )
  (defun insbury (tt tf dc dl dw dx dy du dd / ad eh sgrp p0 p1 ss th tl ty x xc y yc) 
    (&LJIG "预埋件" t)
    (setq dc (/ &sp dc 1.)
          dl (* 0.5 dl dc)
          dw (* 0.5 dw dc)
          p0 (getvar "viewctr")
          xc (+ (car p0) (* dx dc))
          yc (+ (cadr p0) (* dy dc))
          ss (ssadd)
    )
    (setvar "plinewid" 0)
    (command 
      ".pline"
      (list (- xc dl) (- yc dw) 0)
      (list (+ xc dl) (- yc dw) 0)
      (list (+ xc dl) (+ yc dw) 0)
      (list (- xc dl) (+ yc dw) 0)
      "c"
    )
    (ssadd (entlast) ss)
    (if tf 
      (progn 
        (&LJIG "填充" t)
        (@htas t)
        (command ".bhatch" "p" "s" "s" (entlast) "" "")
        (@htas nil)
        (ssadd (setq eh (entlast)) ss)
        (&LJIG "预埋件" t)
      )
    )
    (if tt 
      (progn 
        (setq tt (* 1.5 &sp))
        (command 
          ".line"
          (list (- xc dl tt) yc 0)
          (list (+ xc dl tt) yc 0)
          ""
        )
        (ssadd (entlast) ss)
        (command 
          ".line"
          (list xc (- yc dw tt) 0)
          (list xc (+ yc dw tt) 0)
          ""
        )
        (ssadd (entlast) ss)
      )
    )
    (while 
      (progn 
        (initget "Base")
        (= (&END "\n点取埋件定位点或 [改变基点(B)]<退出>：" ss p0) "Base")
      )
      (while (/= 5 (car (setq p1 (grread T)))))
      (setq p0 (cadr p1)
            p1 (&OSNP p0 "\n点取埋件基点<不变>: ")
      )
      (if p1 (setq p0 p1))
    )
    (if (entget (ssname ss 0)) 
      (progn 
        (setq p0   (&DRAG (ssname ss 0) 10)
              p0   (&N2S (car p0) (caddr p0))
              p1   p0
              xc   (car p0)
              yc   (cadr p0)
              th   (&INTS "标号文字")
              tl   0
              sgrp (ssadd)
        )
        (if 
          (and 
            (entget (ssname ss 0))
            (or (read du) (read dd))
            (setq p1 (&OSNP p1 "\n点取埋件文字的引出点<退出>: "))
          )
          (progn 
            (setq x (car p1)
                  y (cadr p1)
            )
            (if (> (caadr (grread 5)) xc) 
              (setq ad +
                    ty "l"
              )
              (setq ad -
                    ty "r"
              )
            )
            (if (read du) 
              (progn 
                (&LJIG "预埋件编号" t)
                (command 
                  ".text"
                  (strcat "b" ty)
                  (list (ad x th) (+ y (/ th 3.)) 0)
                  th
                  0
                  du
                )
                (setq du (textbox (entget (entlast)))
                      tl (max tl (- (caadr du) (caar du)))
                )
                (ssadd (entlast) sgrp)
              )
            )
            (&LJIG "预埋件文字" t)
            (if (read dd) 
              (progn 
                (command 
                  ".text"
                  (strcat "t" ty)
                  (list (ad x th) (+ y (/ th -3.)) 0)
                  th
                  0
                  dd
                )
                (setq dd (textbox (entget (entlast)))
                      tl (max tl (- (caadr dd) (caar dd)))
                )
                (ssadd (entlast) sgrp)
              )
            )
            (setq yc (if (> y yc) 
                       (+ yc dw (* -0.5 dw))
                       (- yc dw (* -0.5 dw))
                     )
            )
            (command ".line" p1 (list (ad x (* 1.2 th) tl) y) "")
            (ssadd (entlast) sgrp)
            (command ".line" (list xc yc) p1 "")
            (ssadd (entlast) sgrp)
            (&DGAR sgrp)
          )
          (if (null p1) (ss::del ss))
        )
      )
    )
  )
  (if (and (> (&UTXT) -1) (setq l (shdcl))) 
    (apply 'insbury l)
  )
  (&TSTY)
)
(defun Bzbd\input (new-inputp / n n0) 
  (setq n0 (&GPTS "Tssd/Lsp_Myj/Bzbd")
        n0 (if (member n0 '("-4" "-3" "-2" "-1" "0" "1" "2" "3" "4")) 
             (atoi n0)
             4
           )
  )
  (princ 
    (strcat 
      "\n当前坐标单位为"
      (if (< n0 1) "毫米" "米")
      (if (zerop n0) 
        "。"
        (strcat ", 保留" (itoa (abs n0)) "位小数。")
      )
    )
  )
  (if new-inputp 
    (progn 
      (while 
        (and 
          (setq n (getint 
                    (strcat 
                      "\n输入新的单位及小数位数, 1~4=米有小数位 /0=毫米无小数 /-1~-4=毫米有小数位<"
                      (itoa n0)
                      ">: "
                    )
                  )
          )
          (> (abs n) 4)
        )
        (princ "\n**输入值非法, 应在-4~4之间, 请重新输入！")
      )
      (if (and n (/= n n0)) 
        (&GLAY "Tssd/Lsp_Myj/Bzbd" (itoa n))
      )
    )
  )
  (if n n n0)
)
(setfunhelp "c:Bzbd" "Tssd" "BZBD")
(defun c:Bzbd (/ dd du hi lt n p1 p2 sse st tx ty x x2 y y2 zd) 
  (if (> (&UTXT) -1) 
    (progn 
      (setq n  (Bzbd\input nil)
            lt (&LJIG "细线符号" t)
            hi (&INTS "标号文字")
      )
      (while 
        (and 
          (progn 
            (initget "Q")
            (setq p1 (&OSNP "\n点取坐标点位置或 [修改单位和精度(Q)]<退出>: "))
          )
          (or (= p1 "Q") (setq p2 (&OSNP p1 "\n点取引出点位置<退出>: ")))
        )
        (if (= p1 "Q") 
          (setq n (Bzbd\input t))
          (progn 
            (setq x   (car p1)
                  y   (cadr p1)
                  x2  (car p2)
                  y2  (cadr p2)
                  sse (ssadd)
            )
            (if (> x2 x) 
              (setq zd +
                    st "L"
              )
              (setq zd -
                    st "R"
              )
            )
            (setq tx y
                  ty x
            )
            (if (> n 0) 
              (setq tx (/ tx 1000)
                    ty (/ ty 1000)
              )
            )
            (setq tx (strcat "X=" (rtos tx 2 (abs n)))
                  ty (strcat "Y=" (rtos ty 2 (abs n)))
            )
            (command ".line" p1 p2 "")
            (ssadd (entlast) sse)
            (command 
              ".line"
              (list (- x &sp) y)
              (list (+ x &sp) y)
              ""
            )
            (ssadd (entlast) sse)
            (command 
              ".line"
              (list x (- y &sp))
              (list x (+ y &sp))
              ""
            )
            (ssadd (entlast) sse)
            (command 
              ".text"
              (strcat "B" st)
              (list (zd x2 hi) (+ y2 (/ hi 3)))
              hi
              0
              tx
            )
            (entmod (@subst (entget (entlast)) 8 lt))
            (setq du (textbox (entget (entlast)))
                  du (- (caadr du) (caar du))
            )
            (ssadd (entlast) sse)
            (command 
              ".text"
              (strcat "T" st)
              (list (zd x2 hi) (- y2 (/ hi 3)))
              hi
              0
              ty
            )
            (entmod (@subst (entget (entlast)) 8 lt))
            (setq dd (textbox (entget (entlast)))
                  dd (- (caadr dd) (caar dd))
            )
            (ssadd (entlast) sse)
            (command 
              ".line"
              p2
              (list (zd x2 hi hi (max du dd)) y2)
              ""
            )
            (ssadd (entlast) sse)
            (&DGAR sse)
          )
        )
      )
    )
  )
  (&TSTY)
)
(setfunhelp "c:Zhbzh" "Tssd" "zhbzh")
(defun c:Zhbzh (/ draw csh bj) 
  (defun draw (na / pt ang ss flag zg) 
    (&GLAY "Tssd/Dcl_Hzh/Zbz" (&RTXT bj))
    (setq pt (&OSNP "\n选取指北针中心所在的位置<退出>: ")
          ss (ssadd)
    )
    (if pt 
      (progn 
        (&LJIG "细线符号" T)
        (command ".insert" na pt (* (/ bj 15.0) &sp) "" 0)
        (ssadd (entlast) ss)
        (initget 128)
        (setq ang (&END "\n选取指北针的角度方向(单位:度)<向上>: " ss pt 0))
        (setq flag (type ang))
        (if (= flag 'STR) 
          (progn 
            (ss::del ss)
            (if (= (read ang) (atof ang)) 
              (command 
                ".insert"
                na
                pt
                (* (/ bj 15.0) &sp)
                ""
                (atof ang)
              )
              (command ".insert" na pt (* (/ bj 15.0) &sp) "" 90)
            )
          )
          (if (not ang) 
            (command ".insert" na pt (* (/ bj 15.0) &sp) "" 90)
          )
        )
        (if (not ang) 
          (setq ang (* 0.5 pi))
          (if (listp ang) 
            (setq ang (angle pt ang))
            (setq ang (/ (* (atof ang) pi) 180))
          )
        )
        (setq zg (&INTS "标号文字")
              pt (polar 
                   pt
                   ang
                   (+ 
                     zg
                     (* (if (= (strcase na) "TS_ZBZ2") (* 0.5 bj) bj) &sp)
                   )
                 )
        )
        (command 
          ".text"
          "j"
          "c"
          pt
          (* 1.5 zg)
          (- (* 180 (/ ang pi)) 90)
          "北"
        )
      )
    )
  )
  (defun csh (/ vslide) 
    (defun vslide (name img / x y) 
      (setq x (dimx_tile img))
      (setq y (dimy_tile img))
      (start_image img)
      (fill_image 0 0 x y -2)
      (slide_image 0 0 x (- y 5) name)
      (end_image)
    )
    (set_tile "bj" bj)
    (setq bj (atof bj))
    (vslide "Tssd(zbz)" "img1")
    (vslide "Tssd(zbz1)" "img2")
    (vslide "Tssd(zbz2)" "img3")
  )
  (defun hdsj (no /) 
    (cond 
      ((= no 1) (setq na "Ts_zbz"))
      ((= no 2) (setq na "Ts_zbz1"))
      ((= no 3) (setq na "Ts_zbz2"))
    )
    (if (or (= $reason 4) (= no 0)) 
      (progn (setq what -8) (done_dialog 0))
    )
  )
  (if (< (&UTXT) 0) (exit))
  (setq bj (&GPTS "Tssd/Dcl_Hzh/Zbz"))
  (if (or (not bj) (= bj "")) (setq bj "24"))
  (setq what 2
        dia  (lib::dcl "Tools")
        x    0
        na   "Ts_zbz"
  )
  (while (> what 0) 
    (if (not (new_dialog "zbz" dia)) (exit))
    (csh)
    (action_tile "img1" "(hdsj 1)")
    (action_tile "img2" "(hdsj 2)")
    (action_tile "img3" "(hdsj 3)")
    (action_tile "bj" "(#isnum 0 100000)")
    (action_tile "accept" "(hdsj 0)")
    (action_tile "cancel" "(done_dialog 0)(setq what -2)")
    (action_tile "help" "(help \"Tssd\" \"zhbzh\")")
    (start_dialog)
  )
  (unload_dialog dia)
  (if (= what -8) (draw na))
  (&TSTY)
)
(setfunhelp "c:Pmpq" "Tssd" "pmpq")
(defun c:Pmpq (/ ptx_y ang ang1 ang2 i s len oldlen p p1 pt pt_l pt1 pt2 str wid zg) 
  (defun ptx_y (p p1 / pp) 
    (setq x (abs (- (nth 0 p) (nth 0 p1)))
          y (abs (- (nth 1 p) (nth 1 p1)))
    )
    (if (>= x y) 
      (setq pp (list (nth 0 p1) (nth 1 p) 0.0))
      (setq pp (list (nth 0 p) (nth 1 p1) 0.0))
    )
    pp
  )
  (if (< (&UTXT) 0) (exit))
  (setq s  nil
        s  (ssadd)
        pt (&OSNP "\n点取剖面剖切符号的第一点<退出>: ")
  )
  (if pt 
    (progn 
      (setq pt_l (append pt_l (list pt))
            ang1 nil
      )
      (setq pt1 (&OSNP pt "\n第二点<结束>: "))
      (while pt1 
        (setq pt1  (ptx_y pt pt1)
              ang2 (angle pt pt1)
        )
        (if (not ang) (setq ang (angle pt pt1)))
        (grdraw pt pt1 2)
        (if 
          (or 
            (equal ang2 ang1 0.001)
            (equal (+ ang2 pi) ang1 0.001)
            (equal (- ang2 pi) ang1 0.001)
          )
          (setq pt_l (reverse (cdr (reverse pt_l))))
        )
        (setq pt_l (append pt_l (list pt1))
              pt   pt1
              ang1 ang2
        )
        (setq pt1 (&OSNP pt "\t下一点<结束>: "))
      )
      (setq len    (length pt_l)
            oldlen len
      )
      (if (>= len 2) 
        (progn 
          (setq p  (nth 0 pt_l)
                p1 (nth (- len 1) pt_l)
          )
          (setq p (polar p (angle p p1) (* 0.5 (distance p p1))))
          (setq ang1 (&OSNP p "\n输入剖面剖视的方向<退出>: " 1))
          (if ang1 
            (if 
              (or 
                (equal ang ang1 0.01)
                (equal ang (+ ang1 pi) 0.01)
                (equal ang (- ang1 pi) 0.01)
              )
              (setq ang1 (&OSNP p "\n输入的方向无效, 重新输入剖面剖视的方向<退出>: " 1))
            )
          )
          (if ang1 
            (if 
              (or 
                (equal ang ang1 0.01)
                (equal ang (+ ang1 pi) 0.01)
                (equal ang (- ang1 pi) 0.01)
              )
              (progn 
                (setq ang1 nil)
                (princ "\n多次输入了无效的剖面剖视的方向, 程序自动退出!")
              )
            )
          )
          (if ang1 
            (progn 
              (if (or (equal ang 0 0.01) (equal ang pi 0.01)) 
                (if (and (> ang1 0) (< ang1 pi)) 
                  (setq ang1 (* 0.5 pi))
                  (setq ang1 (* 1.5 pi))
                )
                (if (and (> ang1 (* 0.5 pi)) (< ang1 (* 1.5 pi))) 
                  (setq ang1 pi)
                  (setq ang1 0)
                )
              )
              (setq str (getstring "\n输入剖面剖切符号<1>: "))
              (if (wcmatch str "") (setq str "1"))
              (initget 6)
              (setq zg (getreal "\n输入剖面剖切符号的字高<5>: "))
              (if (not zg) 
                (setq zg (* 5. &sp))
                (setq zg (* zg &sp))
              )
              (setq wid (* (&GSYS "多义线宽度") &sp)
                    i   0
              )
              (&INTS "标号文字" t)
              (&LJIG "粗线符号" T)
              (while (> len 0) 
                (setq p (nth i pt_l))
                (if (= i 0) 
                  (progn 
                    (command 
                      "pline"
                      (setq pt1 (polar p ang1 (* 5 &sp)))
                      "w"
                      wid
                      ""
                      p
                      (polar p ang (* 8 &sp))
                      ""
                    )
                    (cond 
                      ((equal ang1 0 0.01)
                       (command "text" (polar pt1 ang1 (* 1.5 &sp)) zg 0 str)
                      )
                      ((equal ang1 (* 0.5 pi) 0.01)
                       (command "text" (polar pt1 ang1 (* 1.5 &sp)) zg 0 str)
                      )
                      ((equal ang1 pi 0.01)
                       (command 
                         "text"
                         "J"
                         "BR"
                         (polar pt1 ang1 (* 1.5 &sp))
                         zg
                         0
                         str
                       )
                      )
                      ((equal ang1 (* 1.5 pi) 0.01)
                       (command 
                         "text"
                         "J"
                         "TL"
                         (polar pt1 ang1 (* 1.5 &sp))
                         zg
                         0
                         str
                       )
                      )
                    )
                  )
                )
                (if (> i 0) 
                  (if (= (- oldlen 1) i) 
                    (progn 
                      (command 
                        "pline"
                        (polar p (angle p (nth (- oldlen 2) pt_l)) (* 8 &sp))
                        "w"
                        wid
                        ""
                        p
                        (setq pt1 (polar p ang1 (* 5 &sp)))
                        ""
                      )
                      (cond 
                        ((equal ang1 0 0.01)
                         (command "text" (polar pt1 ang1 (* 1.5 &sp)) zg 0 str)
                        )
                        ((equal ang1 (* 0.5 pi) 0.01)
                         (command 
                           "text"
                           "J"
                           "BR"
                           (polar pt1 ang1 (* 1.5 &sp))
                           zg
                           0
                           str
                         )
                        )
                        ((equal ang1 pi 0.01)
                         (command 
                           "text"
                           "J"
                           "BR"
                           (polar pt1 ang1 (* 1.5 &sp))
                           zg
                           0
                           str
                         )
                        )
                        ((equal ang1 (* 1.5 pi) 0.01)
                         (command 
                           "text"
                           "J"
                           "TR"
                           (polar pt1 ang1 (* 1.5 &sp))
                           zg
                           0
                           str
                         )
                        )
                      )
                    )
                    (progn 
                      (command 
                        "pline"
                        (setq pt (polar p (angle p (nth (- i 1) pt_l)) (* 5 &sp)))
                        "w"
                        wid
                        ""
                        p
                        (setq pt2 (polar p (angle p (nth (+ i 1) pt_l)) (* 5 &sp)))
                        ""
                      )
                    )
                  )
                )
                (setq len (- len 1)
                      i   (+ i 1)
                )
              )
            )
          )
        )
      )
    )
  )
  (redraw)
  (&TSTY)
)
(setfunhelp "c:Jmpq" "Tssd" "jmpq")
(defun c:Jmpq (/ a ay hi no p1 p2 p3 pw q1 q2 zg) 
  (if 
    (and 
      (> (&UTXT) -1)
      (setq p1 (&OSNP "\n选取截面剖切符号的起点<退出>: "))
      (setq p2 (&OSNP p1 "\n选取截面剖切符号的终点<退出>: "))
      (setq p3 (&N2S p1 p2))
      (setq p3 (&OSNP p3 "\n选取截面剖切符号的方向<退出>: "))
    )
    (progn 
      (initget (+ 2 4))
      (setq no (getint "\n输入截面剖切号<1>: ")
            no (if no (itoa no) "1")
            hi (&INTS "标号文字" t)
            a  (angle p1 p2)
            pw (* &sp (&GSYS "多义线宽度"))
      )
      (initget 6)
      (setq zg (getreal "\n输入剖面剖切符号的字高<5>: "))
      (if (not zg) 
        (setq zg (* 5. &sp))
        (setq zg (* zg &sp))
      )
      (setq q1 (polar p1 a (* 8 &sp))
            q2 (polar p2 a (* 8 &sp))
      )
      (&LJIG "粗线符号" T)
      (setvar "plinewid" pw)
      (command ".pline" p1 q1 "" ".pline" p2 q2 "")
      (setq ay ((if (minusp (&MIDP p3 p1 p2)) + -) a _pi2)
            p1 (polar (&N2S p1 q1) ay (+ zg hi))
            p2 (polar (&N2S p2 q2) ay (+ zg hi) a (/ (* a 180) pi))
      )
      (command ".text" "m" p1 zg 0 no ".text" "m" p2 zg 0 no)
    )
  )
  (&TSTY)
)
(setfunhelp "c:Txjc" "Tssd" "txjc")
(defun c:Txjc (/ ints ssbreak flag pt1 pt2 pt_list oldpt1 len i ss en pt ent entl 
               pt_l an
              ) 
  (defun ssbreak (p1 p2 s / len e el flag pt1 pt2 cen r qi zh p pl ptl pt) 
    (defun brkpl (en1 en2 pt / pl1 pl2 flag xg) 
      (defun xg (e / el) 
        (setq el (entget e)
              el (subst (cons 70 0) (assoc 70 el) el)
              el (append el (list (assoc 10 el)))
        )
        (entmod el)
        (entupd e)
        e
      )
      (if (wcmatch (&DRAG en1 0) "LWPOLYLINE") 
        (if 
          (setq pl1  (&DRAG en1 10)
                flag (&DRAG en1 70)
          )
          (progn 
            (if (= flag 1) (xg en1))
            (if (not (@memb pt pl1 5)) (command ".break" en1 pt pt))
          )
        )
      )
      (if (wcmatch (&DRAG en2 0) "LWPOLYLINE") 
        (if 
          (setq pl2  (&DRAG en2 10)
                flag (&DRAG en2 70)
          )
          (progn 
            (if (= flag 1) (xg en2))
            (if (not (@memb pt pl2 5)) (command ".break" en2 pt pt))
          )
        )
      )
    )
    (setq len (sslength s))
    (while (> len 0) 
      (setq len  (1- len)
            e    (ssname s len)
            el   (entget e)
            flag (cdr (assoc 0 el))
      )
      (cond 
        ((wcmatch flag "LINE")
         (setq pt1 (&DRAG e 10)
               pt2 (&DRAG 11)
               pt  (inters p1 p2 pt1 pt2)
         )
         (if pt (command ".break" e pt pt))
        )
        ((wcmatch flag "ARC")
         (setq cen (&DRAG e 10)
               r   (&DRAG 40)
               qi  (&DRAG 50)
               zh  (&DRAG 51)
               pt  (&ORDR p1 p2 cen r qi zh)
         )
         (if pt 
           (progn 
             (command ".break" e (car pt) (car pt))
             (if (= (length pt) 2) 
               (command ".break" e (cadr pt) (cadr pt))
             )
           )
         )
        )
        ((wcmatch flag "CIRCLE")
         (setq cen (&DRAG e 10)
               r   (&DRAG 40)
               pt  (&ORDR p1 p2 cen r)
         )
         (if pt 
           (progn 
             (command 
               ".break"
               e
               (car pt)
               (polar (car pt) (+ (angle cen (car pt)) 0.001) r)
             )
             (if (= (length pt) 2) 
               (command 
                 ".break"
                 e
                 (cadr pt)
                 (polar (cadr pt) (+ (angle cen (cadr pt)) 0.0001) r)
               )
             )
           )
         )
        )
        ((wcmatch flag "LWPOLYLINE")
         (setq pl (@pl2l (&DRAG e 10) (&DRAG e 70) (&DRAG e 42)))
         (while (setq pt (car pl)) 
           (if (= (length pt) 2) 
             (progn 
               (setq p (inters p1 p2 (car pt) (cadr pt)))
               (if p (setq p (list p)))
             )
             (setq p (&ORDR p1 p2 (car pt) (cadr pt) (caddr pt) (cadddr pt)))
           )
           (if p (setq ptl (append ptl p)))
           (setq pl (cdr pl))
         )
         (while (setq pt (car ptl)) 
           (brkpl e (entlast) pt)
           (setq ptl (cdr ptl))
         )
        )
      )
    )
  )
  (if (< (&UTXT) 0) (exit))
  (setq pt1 (&OSNP "\n点取图形裁剪的第一个角点<退出>: "))
  (if pt1 
    (progn 
      (initget "M")
      (setq pt_list (append pt_list (list pt1))
            pt2     (&OSNP pt1 "\n另一角点或 [多边形剪裁(M)]<退出>: " T)
      )
      (if (= pt2 "M") 
        (progn 
          (terpri)
          (while pt1 
            (setq pt2 (&OSNP pt1 "\t下一角点<结束>:"))
            (if pt2 
              (progn 
                (setq pt_list (append pt_list (list pt2)))
                (grdraw pt1 pt2 2)
                (setq pt1 pt2)
              )
              (setq oldpt1 pt1
                    pt1    pt2
              )
            )
          )
        )
        (if pt2 
          (setq pt_list nil
                p1      (@wcs pt1)
                p2      (@wcs pt2)
                pt_list (append 
                          pt_list
                          (list 
                            pt1
                            (@ucs (list (car p1) (cadr p2)))
                            pt2
                            (@ucs (list (car p2) (cadr p1)))
                          )
                        )
          )
        )
      )
      (setq pt_list (append pt_list (list (car pt_list)))
            len     (length pt_list)
            i       0
      )
      (if (>= len 4) 
        (progn 
          (while (> len 1) 
            (setq pt1 (nth i pt_list)
                  pt2 (nth (+ i 1) pt_list)
                  len (1- len)
                  i   (1+ i)
                  an  (+ (* 0.5 pi) (angle pt1 pt2))
                  ss  (@ssgetf nil "LINE,ARC,CIRCLE,LWPOLYLINE" pt1 pt2)
            )
            (if ss (ssbreak pt1 pt2 ss))
          )
          (setq i   (fix (* 0.5 (setq len (length pt_list))))
                pt1 (car pt_list)
                pt2 (nth i pt_list)
                i   0
                pt  (polar pt1 (angle pt1 pt2) (* 0.5 (distance pt1 pt2)))
                i   1
                pt1 (nth 0 pt_list)
                ss  nil
          )
          (command "pline" pt1)
          (while (> len 1) 
            (setq pt1 (nth i pt_list)
                  len (- len 1)
                  i   (+ i 1)
            )
            (command pt1)
          )
          (command "")
          (setq ent (entlast))
          (command 
            "offset"
            (* 0.05 &sp)
            ent
            (polar 
              pt
              (angle pt (nth 0 pt_list))
              (* 1.5 (distance pt (nth 0 pt_list)))
            )
            ""
          )
          (entdel ent)
          (setq ent  (entlast)
                pt_l (&DRAG ent 10)
          )
          (entdel ent)
          (setq ss (ssget "wp" pt_l))
          (ss::del ss)
          (redraw)
        )
      )
    )
  )
  (&TSTY)
)
(setfunhelp "c:Jzhtchcl" "Tssd" "Jzhtchcl")
(defun c:Jzhtchcl (/ _lt dat dia hp nu txt what dcl) 
  (defun dcl (da / vslide csh getno slid gets gettxt sld_list) 
    (defun vslide (no fl / vs n img) 
      (defun vs (na img / x y) 
        (if (not na) (setq na ""))
        (if (wcmatch (strcase na) "GRAVEL1") (setq na "Gravel"))
        (setq x (dimx_tile img)
              y (dimy_tile img)
        )
        (start_image img)
        (fill_image 0 0 x y 0)
        (slide_image 
          -20
          -20
          (+ x 40)
          (+ y 20)
          (strcat "Tssd(" na ")")
        )
        (end_image)
      )
      (setq n    (rem no 4)
            img  (strcat "img" (itoa n))
            name (nth no sld_list)
      )
      (cond 
        ((= n 0)
         (vs (nth no sld_list) "img0")
         (vs (nth (+ no 1) sld_list) "img1")
         (vs (nth (+ no 2) sld_list) "img2")
         (vs (nth (+ no 3) sld_list) "img3")
        )
        ((= n 1)
         (vs (nth (- no 1) sld_list) "img0")
         (vs (nth no sld_list) "img1")
         (vs (nth (+ no 1) sld_list) "img2")
         (vs (nth (+ no 2) sld_list) "img3")
        )
        ((= n 2)
         (vs (nth (- no 2) sld_list) "img0")
         (vs (nth (- no 1) sld_list) "img1")
         (vs (nth no sld_list) "img2")
         (vs (nth (+ no 1) sld_list) "img3")
        )
        ((= n 3)
         (vs (nth (- no 3) sld_list) "img0")
         (vs (nth (- no 2) sld_list) "img1")
         (vs (nth (- no 1) sld_list) "img2")
         (vs (nth no sld_list) "img3")
        )
      )
      (if (= fl 0) (mode_tile img 4))
    )
    (defun csh (da / va) 
      (mapcar 'set '(no ed an) txt)
      (start_list "li")
      (mapcar 'add_list da)
      (end_list)
      (vslide no 0)
      (set_tile "li" (&RTXT no))
      (set_tile "ed" (&RTXT ed))
      (set_tile "an" (&RTXT an))
      (setq va (itoa (- 40 no)))
      (set_tile "sli" va)
    )
    (defun getno (fl / n va) 
      (mapcar 'mode_tile '("pp" "ob" "ed" "an") '(0 0 0 0))
      (if (/= fl 4) 
        (progn 
          (setq n  (/ no 4)
                no (+ fl (* n 4))
          )
          (set_tile "li" (itoa no))
        )
        (setq no (atoi (get_tile "li")))
      )
      (setq txt (list no ed an)
            va  (itoa (- 39 no))
      )
      (set_tile "sli" va)
      (if (< no 40) 
        (vslide no 0)
        (mapcar 'mode_tile '("pp" "ob" "ed" "an") '(1 1 1 1))
      )
    )
    (defun slid (/ va) 
      (mapcar 'mode_tile '("pp" "ob" "ed" "an") '(0 0 0 0))
      (setq va (get_tile "sli")
            no (- 39 (read va))
      )
      (vslide no 1)
      (if (> no 40) 
        (mapcar 'mode_tile '("pp" "ob" "ed" "an") '(1 1 1 1))
      )
    )
    (defun gets (no / en1 en2 in) 
      (while 
        (setq in (cond 
                   ((and (zerop nu) (zerop no)) (&OSNP "\n点取填充内部点<返回>: "))
                   ((zerop no)
                    (initget "Z")
                    (&OSNP "\n点取填充内部点或 [回退(Z)]<返回>: ")
                   )
                   ((zerop nu) (&DSTR "\n选择要填充的实体<返回>: "))
                   (t (&DSTR "\n选择要填充的实体或 [回退(Z)]<返回>: " "Z"))
                 )
        )
        (if (= in "Z") 
          (progn (setq nu (1- nu)) (command ".u"))
          (progn 
            (setq en1 (entlast))
            (command ".bhatch" "a" "i" "y" "s" "n" "r" "n" "a" "n" "" "p" name)
            (if (/= (strcase name) "SOLID") (command ed an))
            (if (listp in) (command in "") (command "s" in "" ""))
            (setq en2 (entlast))
            (if en1 
              (if (not (eq en1 en2)) 
                (setq nu (1+ nu))
                (command ".u")
              )
              (if en2 (setq nu (1+ nu)) (command ".u"))
            )
          )
        )
      )
    )
    (defun gettxt (f) 
      (setq txt (list no ed an))
      (if (= f 1) (#dcldata "Tssd/Dcl_HZH/Hatch" _lt nil))
    )
    (setq sld_list (list "solid" "Armored" "Gravel1" "Scree_stone" "Grit_stone" 
                         "Stone1" "Stone2" "Stone3" "Stone4" "Stone5" "Clay1" 
                         "Return_Ground" "Return_clinker" "Water" "Concrete" "Armoredw" 
                         "Second_Concrete" "Concrete_stone" "Asphaltum" "Concrete1" "Metal" 
                         "Brick1" "FireBrick" "Glass" "Veneer" "Stell_Concrete" "Fibrefill" 
                         "Hole_matetiel" "Latex" "Plastic" "FireProofing" "Asphaltum_Sand" 
                         "Stell1" "Turf" "Stone6" "Sand_bag" "Bundle_Wood" "Bamboo" 
                         "Bamboo1"
                   )
    )
    (while (> what 0) 
      (if (not (new_dialog "Hatch" dia)) (exit))
      (csh da)
      (action_tile "li" "(getno 4)")
      (action_tile "img0" "(getno 0)")
      (action_tile "img1" "(getno 1)")
      (action_tile "img2" "(getno 2)")
      (action_tile "img3" "(getno 3)")
      (action_tile "ed" "(#isnum 0 100000)")
      (action_tile "an" "(#isnum 0. 360)")
      (action_tile "sli" "(slid)")
      (action_tile "pp" "(gettxt 0)(done_dialog 5)")
      (action_tile "ob" "(gettxt 0)(done_dialog 6)")
      (action_tile "accept" "(gettxt 1)(done_dialog 7)")
      (action_tile "cancel" "(done_dialog 0)(setq what -2)")
      (action_tile "help" "(help \"Tssd\" \"JZHTCHCL\")")
      (setq what (start_dialog))
      (cond 
        ((= what 5) (setq ss (gets 0)))
        ((= what 6) (setq ss (gets 1)))
        ((= what 7) (setq what -8))
      )
    )
    (unload_dialog dia)
  )
  (if (< (&UTXT) 0) (exit))
  (setq nu   0
        hp   (getvar "hpbound")
        what 2
        dia  (lib::dcl "Tools")
        dat  (list "涂黑" "钢筋混凝土" "碎石" "卵石" "砂卵石、砂砾石" "堆块石" "干砌块石" "浆砌块石" "干砌条石" "浆砌条石" 
                   "黏土" "回填土" "回填石渣" "水、液体" "混凝土" "钢筋混凝土(水工)" "二期混凝土" "埋石混凝土" "沥青混凝土" 
                   "砂,灰土,水泥砂浆" "金属" "砖" "耐火砖、耐火材料" "玻璃、透明材料" "胶合板" "钢丝网、水泥板" "纤维材料" "多孔材料" 
                   "橡胶" "塑料" "防水或防潮材料" "沥青砂垫层" "花纹钢板" "草皮" "笼筐填石" "砂(土)袋" "梢捆" "沉竹(柳)排" 
                   "沉软体排"
             )
        _lt  '("no" 0 "ed" 100 "an" 0)
        txt  (#dcldata "Tssd/Dcl_HZH/Hatch" _lt 0)
        txt  (mapcar 'cdr txt)
  )
  (command ".undo" "mark")
  (&LJIG "填充" T)
  (setvar "hpbound" 0)
  (dcl dat)
  (if (and (/= what -8) (/= nu 0)) 
    (command ".undo" "back")
  )
  (setvar "hpbound" hp)
  (&TSTY)
)
(setfunhelp "c:lbkd" "Tssd" "lbkd")
(Defun c:lbkd (/ l inshole shdcl) 
  (defun shdcl (/ #shimg #chk #chk dc dd dh dw dx dy h0 tf w0) 
    (defun #shimg (/ a1 dh1 mx my p1 p2 sc x0 x1 x2 y0 y1 y2) 
      (start_image "img")
      (fill_image 0 0 w0 h0 -2)
      (setq dh1 (if (= tf "tf1") dh dw)
            sc  (/ w0 4000 1.)
            x0  (fix (+ (* w0 0.5) (* dx sc)))
            y0  (fix (- (* h0 0.5) (* dy sc)))
            mx  (fix (* dw sc 0.5))
            x1  (- x0 mx)
            x2  (+ x0 mx)
            my  (fix (* dh1 sc 0.5))
            y1  (- y0 my)
            y2  (+ y0 my)
      )
      (vector_image (/ w0 2) 0 (/ w0 2) h0 1)
      (vector_image 0 (/ h0 2) w0 (/ h0 2) 1)
      (vector_image (- x0 5) (- y0 5) (+ x0 5) (+ y0 5) 2)
      (vector_image (- x0 5) (+ y0 5) (+ x0 5) (- y0 5) 2)
      (if (= tf "tf1") 
        (progn 
          (vector_image x1 y1 x2 y1 7)
          (vector_image x2 y1 x2 y2 7)
          (vector_image x2 y2 x1 y2 7)
          (vector_image x1 y2 x1 y1 7)
        )
        (progn 
          (setq a1 0
                p1 (polar (list x0 y0) a1 mx)
          )
          (repeat 32 
            (setq a1 (+ a1 (/ pi 16))
                  p2 (polar (list x0 y0) a1 mx)
            )
            (vector_image 
              (fix (car p1))
              (fix (cadr p1))
              (fix (car p2))
              (fix (cadr p2))
              7
            )
            (setq p1 p2)
          )
        )
      )
      (if (= td "1") 
        (progn 
          (vector_image (+ x2 5) y1 (+ x2 15) y1 3)
          (vector_image (+ x2 5) y2 (+ x2 15) y2 3)
          (vector_image (+ x2 5) (/ h0 2) (+ x2 15) (/ h0 2) 3)
          (vector_image 
            (+ x2 15)
            (min y1 (/ h0 2))
            (+ x2 15)
            (max y2 (/ h0 2))
            3
          )
          (vector_image x1 (+ y2 5) x1 (+ y2 15) 3)
          (vector_image x2 (+ y2 5) x2 (+ y2 15) 3)
          (vector_image (/ w0 2) (+ y2 5) (/ w0 2) (+ y2 15) 3)
          (vector_image 
            (min x1 (/ w0 2))
            (+ y2 15)
            (max x2 (/ w0 2))
            (+ y2 15)
            3
          )
        )
      )
      (end_image)
    )
    (defun #chk () 
      (cond 
        ((or (= $key "dw") (= $key "dh"))
         (if (#isnum 0 10000.0) 
           (progn 
             (#shimg)
             (set_tile (strcat "s" (@substr $key 2 1)) $value)
           )
         )
        )
        ((or (= $key "dx") (= $key "dy"))
         (if (#isnum -10000.0 10000.0) 
           (progn 
             (#shimg)
             (set_tile (strcat "s" (@substr $key 2 1)) $value)
           )
         )
        )
        (t (#isnum 0 1000.0))
      )
    )
    (defun #togfy (tfb) 
      (if tfb 
        (if (/= tf $key) 
          (progn (setq tf $key) (&PMLAY "dw" "dd"))
        )
        (progn 
          (if (= tf "tf0") (&PMLAY "dw" "dd"))
          (set_tile tf "1")
        )
      )
      (if (= tf "tf1") 
        (progn 
          (set_tile "tfy" "宽度:")
          (mode_tile "dh" 0)
          (mode_tile "sh" 0)
        )
        (progn 
          (set_tile "tfy" "直径:")
          (mode_tile "dh" 1)
          (mode_tile "sh" 1)
        )
      )
      (set_tile "dw" (&RTXT dw))
      (set_tile "sw" (&RTXT dw))
      (#shimg)
    )
    (if (new_dialog "lbkd" (lib::dcl "tools")) 
      (progn 
        (if 
          (not 
            (and 
              (setq dc (&GPTS "Tssd/Dcl_Myj/lbkd"))
              (/= dc "")
              (setq dc (&PLCN dc "\t"))
              (= (length dc) 7)
            )
          )
          (setq dc '("1" "1" "900" "900" "300" "0" "0"))
        )
        (mapcar 'set '(tf td dw dh dd dx dy) dc)
        (setq tf (strcat "tf" tf))
        (mapcar 'set_tile 
                '("tf" "td" "dh" "sh" "dx" "sx" "dy" "sy")
                (list tf td dh dh dx dx dy dy)
        )
        (setq dc (car (&SWAP t))
              dw (atoi dw)
              dh (atoi dh)
              dd (atoi dd)
              dx (atoi dx)
              dy (atoi dy)
              w0 (dimx_tile "img")
              h0 (dimy_tile "img")
        )
        (set_tile "dc" (&RTXT dc))
        (#togfy nil)
        (action_tile "tf0" "(#togfy t)")
        (action_tile "tf1" "(#togfy t)")
        (action_tile "td" "(setq td $value) (#shimg)")
        (foreach x '("dw" "dh" "dx" "dy") 
          (action_tile x "(#chk)")
        )
        (action_tile "dc" "(#isnum 0 2000.0)")
        (action_tile 
          "sw"
          "(set_tile \"dw\" $value) (setq dw (atoi $value)) (#shimg)"
        )
        (action_tile 
          "sh"
          "(set_tile \"dh\" $value) (setq dh (atoi $value)) (#shimg)"
        )
        (action_tile 
          "sx"
          "(set_tile \"dx\" $value) (setq dx (atoi $value)) (#shimg)"
        )
        (action_tile 
          "sy"
          "(set_tile \"dy\" $value) (setq dy (atoi $value)) (#shimg)"
        )
        (action_tile "help" "(help \"Tssd\" \"lbkd\")")
        (if (= (start_dialog) 1) 
          (progn 
            (if (= tf "tf0") (&PMLAY "dw" "dd"))
            (&GLAY 
              "Tssd/Dcl_Myj/lbkd"
              (strcat 
                (@substr tf 3 1)
                "\t"
                td
                "\t"
                (&RTXT dw)
                "\t"
                (&RTXT dh)
                "\t"
                (&RTXT dd)
                "\t"
                (&RTXT dx)
                "\t"
                (&RTXT dy)
              )
            )
            (list (= tf "tf1") (= td "1") dc dw dh dd dx dy)
          )
        )
      )
    )
  )
  (defun inshole (tf td dc dw dh dd dx dy / l p0 p1 p2 pc ss st x1 x2 x3 xc y1 y2 y3 
                  yc zg
                 ) 
    (&GVAL dc)
    (setq st (if tf 
               (strcat "洞" (itoa dw) "x" (itoa dh))
               (strcat "洞%%c" (itoa dd))
             )
          dc (/ &sp dc 1.)
          dw (* dw dc)
          dh (* dh dc)
          dd (* dd dc)
    )
    (if tf 
      (setq dw1 (* 0.5 dw)
            dh1 (* 0.5 dh)
      )
      (setq dw1 (* 0.5 dd)
            dh1 (* 0.5 dd)
      )
    )
    (while (progn (setq p0 (&OSNP "\n点取洞口定位点<退出>： "))) 
      (setq xc (+ (car p0) (* dx dc))
            yc (+ (cadr p0) (* dy dc))
            pc (list xc yc)
      )
      (&LJIG "洞口" t)
      (if tf 
        (command ".insert" "Ts_holes" pc dw dh 0)
        (command ".insert" "Ts_holec" pc dd dd 0)
      )
      (initget 1)
      (setq p1 (&OSNP pc "\t标注位置: ")
            zg (/ (&INTS "标号文字") 3)
            p2 (list 
                 ((if (< (car p1) (car pc)) - +) (car p1) zg)
                 (+ (cadr p1) zg)
               )
      )
      (&LJIG "洞口文字" t)
      (if (< (car p1) (car pc)) 
        (command ".text" "r" p2 (* zg 3) 0 st)
        (command ".text" p2 (* zg 3) 0 st)
      )
      (setq l  (textbox (list (cons 1 st)))
            p2 (list 
                 ((if (< (car p1) (car pc)) - +) 
                   (car p1)
                   (* zg 2)
                   (- (caadr l) (caar l))
                 )
                 (cadr p1)
               )
      )
      (setvar "plinewid" 0)
      (command ".pline" pc p1 p2 "")
      (if td 
        (progn 
          (&LJIG "洞口尺寸" t)
          (setq x1 (car p0)
                x2 (- xc dw1)
                x3 (+ xc dw1)
                l  (@ran1 (list x1 x2 x3))
                x1 (car l)
                x2 (cadr l)
                x3 (caddr l)
                ss (ssadd)
          )
          (command 
            ".dim1"
            "rot"
            0
            (list x1 (- yc dh1))
            (list x2 (- yc dh1))
            (list (* 0.5 (+ x1 x2)) (- yc dh1 (* &sp 6)))
            ""
          )
          (ssadd (entlast) ss)
          (if x3 
            (progn 
              (command 
                ".dim1"
                "rot"
                0
                (list x2 (- yc dh1))
                (list x3 (- yc dh1))
                (list (* 0.5 (+ x2 x3)) (- yc dh1 (* &sp 6)))
                ""
              )
              (ssadd (entlast) ss)
              (&CODE ss)
            )
          )
          (setq y1 (cadr p0)
                y2 (- yc dh1)
                y3 (+ yc dh1)
                l  (@ran1 (list y1 y2 y3))
                y1 (car l)
                y2 (cadr l)
                y3 (caddr l)
                ss (ssadd)
          )
          (command 
            ".dim1"
            "rot"
            90
            (list (+ xc dw1) y1)
            (list (+ xc dw1) y2)
            (list (+ xc dw1 (* &sp 6)) (* 0.5 (+ y1 y2)))
            ""
          )
          (ssadd (entlast) ss)
          (if y3 
            (progn 
              (command 
                ".dim1"
                "rot"
                90
                (list (+ xc dw1) y2)
                (list (+ xc dw1) y3)
                (list (+ xc dw1 (* &sp 6)) (* 0.5 (+ y2 y3)))
                ""
              )
              (ssadd (entlast) ss)
              (&CODE ss)
            )
          )
        )
      )
    )
  )
  (if (and (> (&UTXT) -1) (setq l (shdcl))) 
    (apply 'inshole l)
  )
  (&TSTY)
)
(setfunhelp "c:Tchbj" "Tssd" "Tchbj")
(defun c:Tchbj (/ _lt hat dcl what dat txt name ed an ss no wo en enl cl) 
  (defun hat (what /) (if (/= what -8) (command ".U")))
  (defun dcl (da / vslide csh getno slid gets gettxt sld_list) 
    (defun vslide (no fl / vs n img) 
      (defun vs (na img / x y) 
        (if (not na) (setq na ""))
        (if (wcmatch (strcase na) "GRAVEL1") (setq na "Gravel"))
        (setq x (dimx_tile img)
              y (dimy_tile img)
        )
        (start_image img)
        (fill_image 0 0 x y 0)
        (slide_image 
          -20
          -20
          (+ x 40)
          (+ y 20)
          (strcat "Tssd(" na ")")
        )
        (end_image)
      )
      (setq n    (rem no 4)
            img  (strcat "img" (itoa n))
            name (nth no sld_list)
      )
      (cond 
        ((= n 0)
         (vs (nth no sld_list) "img0")
         (vs (nth (+ no 1) sld_list) "img1")
         (vs (nth (+ no 2) sld_list) "img2")
         (vs (nth (+ no 3) sld_list) "img3")
        )
        ((= n 1)
         (vs (nth (- no 1) sld_list) "img0")
         (vs (nth no sld_list) "img1")
         (vs (nth (+ no 1) sld_list) "img2")
         (vs (nth (+ no 2) sld_list) "img3")
        )
        ((= n 2)
         (vs (nth (- no 2) sld_list) "img0")
         (vs (nth (- no 1) sld_list) "img1")
         (vs (nth no sld_list) "img2")
         (vs (nth (+ no 1) sld_list) "img3")
        )
        ((= n 3)
         (vs (nth (- no 3) sld_list) "img0")
         (vs (nth (- no 2) sld_list) "img1")
         (vs (nth (- no 1) sld_list) "img2")
         (vs (nth no sld_list) "img3")
        )
      )
      (if (= fl 0) (mode_tile img 4))
    )
    (defun csh (da / va) 
      (mapcar 'set '(no ed an) txt)
      (start_list "li")
      (mapcar 'add_list da)
      (end_list)
      (vslide no 0)
      (set_tile "li" (&RTXT no))
      (set_tile "ed" (&RTXT ed))
      (set_tile "an" (&RTXT an))
      (setq va (itoa (- 40 no)))
      (set_tile "sli" va)
    )
    (defun getno (fl / n va) 
      (mapcar 'mode_tile '("ed" "col" "tcl" "an") '(0 0 0 0))
      (if (/= fl 4) 
        (progn 
          (setq n  (/ no 4)
                no (+ fl (* n 4))
          )
          (set_tile "li" (itoa no))
        )
        (setq no (atoi (get_tile "li")))
      )
      (setq txt (list no ed an)
            va  (itoa (- 39 no))
      )
      (set_tile "sli" va)
      (if (< no 40) 
        (vslide no 0)
        (mapcar 'mode_tile '("ed" "col" "tcl" "an") '(1 1 1 1))
      )
    )
    (defun slid (/ va) 
      (mapcar 'mode_tile '("ed" "col" "tcl" "an") '(0 0 0 0))
      (setq va (get_tile "sli")
            no (- 39 (read va))
      )
      (vslide no 1)
      (if (> no 40) 
        (mapcar 'mode_tile '("ed" "col" "tcl" "an") '(1 1 1 1))
      )
    )
    (defun gettxt (f) 
      (setq txt (list no ed an))
      (if (= f 1) (#dcldata "Tssd/Dcl_HZH/Hatch" _lt nil))
    )
    (defun getcol (cl / col x y flag) 
      (if (= cl -1) 
        (setq cl   256
              flag 1
        )
      )
      (setq col (acad_colordlg cl))
      (if (= flag 1) 
        (if (not col) (setq cl -1) (setq cl col))
        (if (not col) (setq col cl) (setq cl col))
      )
      (setq x (dimx_tile "col")
            y (dimy_tile "col")
      )
      (start_image "col")
      (if (or (= cl 0) (= cl 256) (= cl -1)) 
        (fill_image 0 0 x y 7)
        (fill_image 0 0 x y cl)
      )
      (end_image)
      (cond 
        ((= cl -1) (set_tile "col" "  原色"))
        ((= cl 256) (set_tile "col" "  随层"))
        ((= cl 0) (set_tile "col" "  随块"))
        ((= cl 1) (set_tile "col" "    红"))
        ((= cl 2) (set_tile "col" "    黄"))
        ((= cl 3) (set_tile "col" "    绿"))
        ((= cl 4) (set_tile "col" "    青"))
        ((= cl 5) (set_tile "col" "    兰"))
        ((= cl 6) (set_tile "col" "    紫"))
        ((= cl 7) (set_tile "col" "    白"))
      )
      cl
    )
    (setq sld_list (list "solid" "Armored" "Gravel1" "Scree_stone" "Grit_stone" 
                         "Stone1" "Stone2" "Stone3" "Stone4" "Stone5" "Clay1" 
                         "Return_Ground" "Return_clinker" "Water" "Concrete" "Armoredw" 
                         "Second_Concrete" "Concrete_stone" "Asphaltum" "Concrete1" "Metal" 
                         "Brick1" "FireBrick" "Glass" "Veneer" "Stell_Concrete" "Fibrefill" 
                         "Hole_matetiel" "Latex" "Plastic" "FireProofing" "Asphaltum_Sand" 
                         "Stell1" "Turf" "Stone6" "Sand_bag" "Bundle_Wood" "Bamboo" 
                         "Bamboo1"
                   )
    )
    (while (> what 0) 
      (if (not (new_dialog "HatchBj" dia)) (exit))
      (csh da)
      (cond 
        ((= cl -1) (set_tile "col" "  原色"))
        ((= cl 256) (set_tile "col" "  随层"))
        ((= cl 0) (set_tile "col" "  随块"))
        ((= cl 1) (set_tile "col" "    红"))
        ((= cl 2) (set_tile "col" "    黄"))
        ((= cl 3) (set_tile "col" "    绿"))
        ((= cl 4) (set_tile "col" "    青"))
        ((= cl 5) (set_tile "col" "    兰"))
        ((= cl 6) (set_tile "col" "    紫"))
        ((= cl 7) (set_tile "col" "    白"))
      )
      (action_tile "li" "(getno 4)")
      (action_tile "img0" "(getno 0)")
      (action_tile "img1" "(getno 1)")
      (action_tile "img2" "(getno 2)")
      (action_tile "img3" "(getno 3)")
      (action_tile "ed" "(#isnum 0 100000)")
      (action_tile "an" "(#isnum 0. 360)")
      (action_tile "sli" "(slid)")
      (action_tile "col" "(setq cl (getcol cl))")
      (action_tile 
        "accept"
        "(gettxt 1)(done_dialog 7)(setq what -8)"
      )
      (action_tile "cancel" "(done_dialog 0)(setq what -2)")
      (action_tile "help" "(help \"Tssd\" \"Tchbj\")")
      (set_tile "error" "     *****  填充比例设置为出图比例相同  *****")
      (start_dialog)
    )
    (unload_dialog dia)
  )
  (if (< (&UTXT) 0) (exit))
  (if (setq ss (&DSTR "\n选择需要改变填充样式的实体<退出>: " '((0 . "hatch")))) 
    (progn 
      (setq what 2
            dia  (lib::dcl "Tools")
            cl   -1
            dat  (list "涂黑" "钢筋混凝土" "碎石" "卵石" "砂卵石、砂砾石" "堆块石" "干砌块石" "浆砌块石" "干砌条石" 
                       "浆砌条石" "黏土" "回填土" "回填石渣" "水、液体" "混凝土" "钢筋混凝土(水工)" "二期混凝土" "埋石混凝土" 
                       "沥青混凝土" "砂,灰土,水泥砂浆" "金属" "砖" "耐火砖、耐火材料" "玻璃、透明材料" "胶合板" "钢丝网、水泥板" 
                       "纤维材料" "多孔材料" "橡胶" "塑料" "防水或防潮材料" "沥青砂垫层" "花纹钢板" "草皮" "笼筐填石" "砂(土)袋" 
                       "梢捆" "沉竹(柳)排" "沉软体排"
                 )
            _lt  '("no" 0 "ed" 100 "an" 0)
      )
      (setq txt (#dcldata "Tssd/Dcl_HZH/Hatch" _lt 0)
            txt (mapcar 'cdr txt)
      )
      (dcl dat)
      (setvar "cmdecho" 0)
      (if (= what -8) 
        (while (setq en (ssname ss 0)) 
          (ssdel en ss)
          (setq enl (entget en)
                enl (subst (cons 2 name) (assoc 2 enl) enl)
          )
          (if (/= cl -1) 
            (if (assoc 62 enl) 
              (setq enl (subst (cons 62 cl) (assoc 62 enl) enl))
              (setq enl (append enl (list (cons 62 cl))))
            )
          )
          (entmod enl)
          (if (not (wcmatch (strcase name) "SOLID")) 
            (command ".-hatchedit" en "p" "" ed an)
          )
        )
      )
    )
  )
  (&TSTY)
)
(setfunhelp "c:Dchfh" "Tssd" "Dchfh")
(defun c:Dchfh (/ pt1 pt2 p0 p1 ss) 
  (if (< (&UTXT) 0) (exit))
  (if 
    (and 
      (setq pt1 (&OSNP "\n点取对称符号的起点<退出>: "))
      (setq pt2 (&OSNP pt1 "\n点取对称符号的结束点<退出>: "))
    )
    (progn 
      (&LJIG "细线符号" T)
      (setq ang (angle pt1 pt2)
            pt1 (polar pt1 (+ ang pi) (* 4 &sp))
            pt2 (polar pt2 ang (* 4 &sp))
            p0  (polar 
                  (polar pt1 ang (* 2 &sp))
                  (+ ang (* 0.5 pi))
                  (* 1.5 &sp)
                )
            p1  (polar p0 (- ang (* 0.5 pi)) (* 3 &sp))
            ss  (ssadd)
      )
      (command ".Line" p0 p1 "")
      (ssadd (entlast) ss)
      (command 
        ".Line"
        (polar p0 ang &sp)
        (polar p1 ang &sp)
        ""
      )
      (ssadd (entlast) ss)
      (command 
        ".Line"
        (polar p0 ang (- (distance pt1 pt2) (* 5 &sp)))
        (polar p1 ang (- (distance pt1 pt2) (* 5 &sp)))
        ""
      )
      (ssadd (entlast) ss)
      (command 
        ".Line"
        (polar p0 ang (- (distance pt1 pt2) (* 4 &sp)))
        (polar p1 ang (- (distance pt1 pt2) (* 4 &sp)))
        ""
      )
      (ssadd (entlast) ss)
      (command ".Line" pt1 pt2 "")
      (command ".Change" (entlast) "" "P" "Lt" "Center" "")
      (ssadd (entlast) ss)
      (&DGAR ss)
    )
  )
  (&TSTY)
)
(setfunhelp "c:Shtshch" "Tssd" "Shtchch")
(defun C:Shtshch (/ name oname sout shch save) 
  (defun shch (sout / pt psc scrh en enl ent hand no) 
    (command ".Zoom" "e")
    (setq pt   (getvar "viewctr")
          scrh (/ (getvar "viewsize") 2)
          psc  (getvar "screensize")
          pt   (list 
                 (- (car pt) (* scrh (/ (car psc) (cadr psc))))
                 (+ (cadr pt) scrh)
                 0.0
               )
          psc  (/ (+ 0.95 (car psc)) (car psc))
          ent  (entlast)
          no   1
    )
    (command ".Wmfout" "TszWmfTemp" sout "")
    (command ".Wmfin" "TszWmfTemp" pt (* 2.0 psc) 2.0 "")
    (command ".Zoom" "p")
    (setq en (entlast))
    (command ".Explode" en)
    (while (setq en (ssname sout 0)) 
      (setq enl (entget en)
            enl (append enl (list (cons 60 1)))
      )
      (entmod enl)
      (ssdel en sout)
    )
    (redraw)
  )
  (defun save (/ name nname oname l nam ch path) 
    (setq name (getvar "Dwgname")
          path (getvar "DWGPREFIX")
    )
    (if (setq nname (getfiled "指定审图图纸文件输出位置" name "Dwg" 1)) 
      (progn 
        (setq l   (strlen nname)
              nam ""
        )
        (while (/= "\\" (setq ch (substr nname l 1))) 
          (setq nam (strcat ch nam)
                l   (1- l)
          )
        )
        (if (wcmatch (strcase name) (strcase nam)) 
          (progn 
            (setq nam   (strcat "Tsz_" nam)
                  nname (strcat (substr nname 1 l) nam)
            )
            (alert 
              "\n*****审图图纸输出不允许存储为现有名称*****\n\n***程序自动在现有名称前添加 Tsz_ 的前缀***"
            )
          )
        )
        (command ".saveas" "" nname)
        (if (not (zerop (getvar "cmdactive"))) (command "y"))
      )
    )
    (list nname (strcat path name))
  )
  (if (< (&UTXT) 0) (exit))
  (if (setq sout (ssget "x" '((0 . "text,mtext")))) 
    (shch sout)
  )
  (setq name  (save)
        oname (car name)
        name  (cadr name)
  )
  (command ".Undo" "Back")
  (command ".saveas" "" name)
  (if (not (zerop (getvar "cmdactive"))) (command "y"))
  (&TSTY)
)
(defun c:RYBH (/ a di dm e h l p0 p1 pa pb st att ozj zj) 
  (if (< (&UTXT) 0) (exit))
  (setq ozj 8.0
        zj  (getreal (strcat "\n输入编号直径<" (rtos ozj) ">: "))
  )
  (if (not zj) (setq zj ozj))
  (&LJIG "尺寸" t)
  (&INTS "标号")
  (setq h   (&INTS "标号")
        att (getvar "attdia")
        ozj (* &sp zj)
        ost "1"
  )
  (setvar "attreq" 1)
  (setvar "attdia" 0)
  (while (setq pt (&OSNP "\n点取要编号的实体线 或直接选点 <退出>: ")) 
    (setq st (getstring (strcat "\n输入编号内容<" ost ">: ")))
    (if (= st "") (setq st ost))
    (if 
      (setq s (ssget 
                "cp"
                (list 
                  (polar pt (* 0.25 pi) (* 0.2 &sp))
                  (polar pt (* 0.75 pi) (* 0.2 &sp))
                  (polar pt (* 1.25 pi) (* 0.2 &sp))
                  (polar pt (* 1.75 pi) (* 0.2 &sp))
                )
                (list (cons 0 "LINE,LWPOLYLINE,ARC,CIRCLE"))
              )
      )
      (progn 
        (setq en   (ssname s 0)
              flag (strcase (&DRAG en 0))
        )
        (if (wcmatch flag "LINE") 
          (setq ang (angle (&DRAG 10) (&DRAG 11)))
          (if (wcmatch flag "LWPOLYLINE") 
            (setq l   (@whpl (@pl2l (&DRAG 10) (&DRAG 70) (&DRAG 42)) pt)
                  ang (angle (car l) (cadr l))
            )
            (setq ang (- (angle (&DRAG 10) pt) (* 0.5 pi)))
          )
        )
        (if (equal ang (+ pi pi) 0.01) (setq ang 0.0))
        (if (> ang pi) (setq ang (- ang pi)))
        (setq pt (polar 
                   pt
                   (+ ang (* 0.5 pi))
                   (+ (* 0.5 ozj) (* 3 &sp))
                 )
        )
      )
      (setq ang 0.0)
    )
    (command 
      ".insert"
      "ts_reino"
      pt
      ozj
      ""
      (/ (* ang 180.0) pi)
      (setq ost st)
    )
  )
  (setvar "attdia" att)
  (&TSTY)
)
(defun C:Chyfh () 
  (menucmd "I=TSSD.SYMBOL")
  (menucmd "I=*")
  (princ)
)
(defun c:Ie () 
  (if (> (&UTXT) -1) 
    (command "browser" "http://www.tsz.com.cn")
  )
  (&TSTY)
)
(defun c:Gfcx (/ st) 
  (if 
    (and 
      (> (&UTXT) -1)
      (setq st (&SGET))
      (setq st (strcat st "prg\\结构规范查询.chm"))
      (setq st (findfile st))
    )
    (startapp (strcat "hh " st))
  )
  (&TSTY)
)
(defun c:Ck () (@run_exe "Ck.exe"))
(defun c:Gjchxb () (@run_exe "GJCHXB.exe"))
(defun c:ZLJSS () (@run_exe "ZLJSS.exe"))
 
