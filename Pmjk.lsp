(lib::lsp "myfan")
(defun txt4pm (e0 lay0 sc / asc asc1 el i len ll st stn sto) 
  (setq el (entget e0))
  (if (= (&DRAG e0 0) "TEXT") 
    (progn 
      (setq sto (&DRAG e0 1)
            stn ""
            i   1
            len (strlen sto)
      )
      (while (<= i len) 
        (setq st   (substr sto i)
              asc  (ascii st)
              asc1 (if (= i len) 0 (ascii (substr sto (1+ i))))
        )
        (cond 
          ((= asc 129) (setq st (&FLD "GJFH1")))
          ((= asc 130) (setq st (&FLD "GJFH1")))
          ((= asc 131) (setq st (&FLD "GJFH2")))
          ((= asc 132) (setq st (&FLD "GJFH3")))
          ((= asc 133) (setq st (&FLD "GJFH4")))
          ((= asc 139) (setq st (&FLD "GJFH4")))
          ((= asc 135) (setq st (&FLD "GJFHA")))
          ((wcmatch st "%%130*,%%129*")
           (setq st (&FLD "GJFH1")
                 i  (+ i 4)
           )
          )
          ((wcmatch st "%%131*")
           (setq st (&FLD "GJFH2")
                 i  (+ i 4)
           )
          )
          ((wcmatch st "%%132*")
           (setq st (&FLD "GJFH3")
                 i  (+ i 4)
           )
          )
          ((wcmatch st "%%133*")
           (setq st (&FLD "GJFH4")
                 i  (+ i 4)
           )
          )
          ((wcmatch st "%%139*")
           (setq st (&FLD "GJFH4")
                 i  (+ i 4)
           )
          )
          ((wcmatch st "\u+0082*")
           (setq st (&FLD "GJFH1")
                 i  (+ i 6)
           )
          )
          ((wcmatch st "\u+0083*")
           (setq st (&FLD "GJFH2")
                 i  (+ i 6)
           )
          )
          ((wcmatch st "\u+0084*")
           (setq st (&FLD "GJFH3")
                 i  (+ i 6)
           )
          )
          ((wcmatch st "\u+0085*")
           (setq st (&FLD "GJFHA")
                 i  (+ i 6)
           )
          )
          ((and (= asc 170) (= asc1 161))
           (setq st (&FLD "GJFH1")
                 i  (1+ i)
           )
          )
          ((and (= asc 170) (= asc1 162))
           (setq st (&FLD "GJFH2")
                 i  (1+ i)
           )
          )
          ((and (= asc 170) (= asc1 163))
           (setq st (&FLD "GJFH3")
                 i  (1+ i)
           )
          )
          ((and (= asc 170) (= asc1 164))
           (setq st (&FLD "GJFH4")
                 i  (1+ i)
           )
          )
          ((and (= asc 170) (= asc1 175))
           (setq st (&FLD "GJFHA")
                 i  (1+ i)
           )
          )
          ((and (= asc 161) (= asc1 193))
           (setq st "X"
                 i  (1+ i)
           )
          )
          ((and (> asc 159) (> asc1 159))
           (setq st (substr sto i 2)
                 i  (1+ i)
           )
          )
          (T (setq st (substr sto i 1)))
        )
        (setq stn (strcat stn st)
              i   (1+ i)
        )
      )
      (if (/= sto stn) (setq ll (cons (cons 1 stn) ll)))
      (setq ll (cons '(72 . 0) ll)
            ll (cons '(73 . 0) ll)
            ll (cons (aslyts lay0) ll)
      )
      (if (= (&DRAG 7) "STANDARD") 
        (setq ll (cons '(7 . "TSSD_Rein") ll))
        (setq ll (cons '(7 . "TSSD_Bold") ll)
              sc nil
        )
      )
      (if sc (setq ll (cons (cons 41 sc) ll)))
      (emod el ll)
    )
    (emod el (list (aslyts lay0)))
  )
)
(defun dim4pm (e0 lay0 / a d1 d2 e e1 e2 e3 e4 e5 lay lp mle mlp mls n p1 p2 p3 p4 p5 
               p6 q1 q2 sc str
              ) 
  (defun fnd1 (e p1 p2 / n q1 q2) 
    (while 
      (and (setq e (entnext e)) 
           (= (&DRAG e 8) lay)
           (= (&DRAG 0) "LINE")
           (setq q1 (&DRAG 10)
                 q2 (&DRAG 11)
           )
           (equal (&MIDP q1 p1 p2) (&MIDP q2 p1 p2) mm)
      )
      (setq mlp (cons (list q1 q2) mlp)
            mle (cons e mle)
      )
    )
    (if (> (setq n (length mlp)) 0) n)
  )
  (defun fnd2 (e n / i) 
    (setq i 0)
    (repeat n 
      (if (and (setq e (entnext e)) (= (&DRAG e 8) lay) (= (&DRAG 0) "LWPOLYLINE")) 
        (setq mle (cons e mle)
              i   (1+ i)
        )
      )
    )
    (= i n)
  )
  (defun fnd3 (e n / i) 
    (setq i 0)
    (repeat n 
      (if (and (setq e (entnext e)) (= (&DRAG e 8) lay) (= (&DRAG 0) "TEXT")) 
        (setq mle (cons e mle)
              i   (1+ i)
              mls (cons (&DRAG 1) mls)
        )
      )
    )
    (= i n)
  )
  (defun gtsc (d1 d2 / sc) 
    (setq sc (* 5 (fix (+ 0.49 (* (/ (abs d2) d1) &sp 0.2)))))
    (if (zerop sc) (setq sc 1))
    sc
  )
  (cond 
    ((= (&DRAG e0 0) "TEXT") (txt4pm e0 lay0 nil) (setq e e0))
    ((= (&DRAG e0 0) "DIMENSION")
     (setq str (&DRAG 1)
           e   (cdr (assoc -2 (tblsearch "block" (&DRAG 2))))
           p1  (&DRAG e 10)
           p2  (&DRAG 11)
           a   (angle p2 p1)
           e   (entnext e)
           p3  (&DRAG e 10)
           p4  (&DRAG 11)
           e   (entnext e)
           p5  (&DRAG e 10)
           e   (entnext e)
           q1  (&DRAG e 10)
           e   (entnext e)
           q2  (&DRAG e 10)
     )
     (entdel e0)
     (if (equal (&MIDP p1 q1 q2) 0 1) 
       (setq p1 p2
             p3 p4
             a  (+ a pi)
       )
     )
     (if (= (read str) (setq d2 (atoi str))) 
       (setq d1  (distance q1 q2)
             sc  (gtsc d1 d2)
             str ""
       )
       (setq sc (fix (/ &sp &sc)))
     )
     (&GVAL sc)
     (setq d1 (getvar "dimexo")
           p1 (polar p1 a d1)
           p3 (polar p3 a d1)
     )
     (command ".dim1" "ali" p1 p3 (&N2S q1 q2) str)
     (setq e (entlast))
     (emod (entget e) (list (aslyts lay0)))
    )
    ((and (setq lay (&DRAG e0 8)) 
          (= (&DRAG 0) "LINE")
          (setq q1 (&DRAG 10)
                q2 (&DRAG 11)
          )
          (setq e1 (entnext e0))
          (= (&DRAG e1 8) lay)
          (= (&DRAG 0) "LINE")
          (setq p1 (&DRAG 10)
                p2 (&DRAG 11)
          )
          (setq e2 (entnext e1))
          (= (&DRAG e2 8) lay)
          (= (&DRAG 0) "LINE")
          (setq p3 (&DRAG 10)
                p4 (&DRAG 11)
          )
          (setq e3 (entnext e2))
          (= (&DRAG e3 8) lay)
          (= (&DRAG 0) "LWPOLYLINE")
          (setq e4 (entnext e3))
          (= (&DRAG e4 8) lay)
          (= (&DRAG 0) "LWPOLYLINE")
          (setq e5 (entnext e4))
          (= (&DRAG e5 8) lay)
          (= (&DRAG 0) "TEXT")
          (setq str (&DRAG 1))
     )
     (foreach e (list e0 e1 e2 e3 e4 e5) (entdel e))
     (setq a  (angle p2 p1)
           d1 (abs (&MIDP p1 p3 p4))
     )
     (if (equal (&MIDP p1 q1 q2) 0 1) 
       (setq p1 p2
             p3 p4
             a  (+ a pi)
       )
     )
     (if (= (read str) (setq d2 (atoi str))) 
       (setq sc  (gtsc d1 d2)
             str ""
       )
       (setq sc (fix (/ &sp &sc)))
     )
     (&GVAL sc)
     (setq d1 (getvar "dimexo")
           p1 (polar p1 a d1)
           p3 (polar p3 a d1)
     )
     (command ".dim1" "ali" p1 p3 (&N2S q1 q2) str)
     (setq e (entlast))
     (emod (entget e) (list (aslyts lay0)))
    )
    ((and (setq lay (&DRAG e0 8)) 
          (= (&DRAG 0) "LINE")
          (setq p1 (&DRAG 10)
                p2 (&DRAG 11)
          )
          (setq mlp nil
                mls nil
                mle (list e0)
                n   (fnd1 e0 p1 p2)
          )
          (setq e1 (entnext (car mle)))
          (= (&DRAG e1 8) lay)
          (= (&DRAG 0) "LINE")
          (setq q1 (&DRAG 10)
                q2 (&DRAG 11)
          )
          (equal (abs (- (&MIDP p1 q1 q2) (&MIDP p2 q1 q2))) (distance p1 p2) mm)
          (setq mle (cons e1 mle))
          (fnd2 e1 (1+ n))
          (fnd3 (car mle) n)
     )
     (foreach e mle (entdel e))
     (setq mlp (reverse mlp)
           mls (reverse mls)
           a   (angle p2 p1)
           p3  (caar mlp)
           p4  (cadar mlp)
           d1  (abs (&MIDP p1 p3 p4))
           str (car mls)
     )
     (if (equal (&MIDP p1 q1 q2) 0 1) 
       (setq a   (+ a pi)
             mlp (mapcar 'cadr mlp)
             p1  p2
       )
       (setq mlp (mapcar 'car mlp))
     )
     (if (= (read str) (setq d2 (atoi str))) 
       (setq sc  (gtsc d1 d2)
             str ""
       )
       (setq sc  (fix (/ &sp &sc))
             str nil
       )
     )
     (&GVAL sc)
     (setq d1 (getvar "dimexo")
           p1 (polar p1 a d1)
           n  0
     )
     (foreach p3 mlp 
       (setq p3 (polar p3 a d1))
       (command ".dim1" 
                "rot"
                (angtos (angle q1 q2) 0 0)
                p1
                p3
                (&N2S q1 q2)
                (if str str (nth n mls))
       )
       (setq e (entlast))
       (emod (entget e) (list (aslyts lay0)))
       (setq p1 p3
             n  (1+ n)
       )
     )
    )
    ((and (setq lay (&DRAG e0 8)) 
          (= (&DRAG 0) "LINE")
          (setq p1 (&DRAG 10)
                p2 (&DRAG 11)
          )
          (setq e1 (entnext e0))
          (= (&DRAG e1 8) lay)
          (= (&DRAG 0) "LINE")
          (setq p3 (&DRAG 10)
                p4 (&DRAG 11)
          )
          (setq e2 (entnext e1))
          (= (&DRAG e2 8) lay)
          (= (&DRAG 0) "LINE")
          (setq q1 (&DRAG 10)
                q2 (&DRAG 11)
          )
          (setq e3 (entnext e2))
          (= (&DRAG e3 8) lay)
          (= (&DRAG 0) "LWPOLYLINE")
          (setq e4 (entnext e3))
          (= (&DRAG e4 8) lay)
          (= (&DRAG 0) "LWPOLYLINE")
          (setq lp (&DRAG 10)
                p5 (car lp)
                p6 (cadr lp)
          )
          (setq e5 (entnext e4))
          (= (&DRAG e5 8) lay)
          (= (&DRAG 0) "LWPOLYLINE")
     )
     (foreach e (list e0 e1 e2 e3 e4 e5) (entdel e))
     (setq a (angle p2 p1))
     (&GVAL)
     (setq d1 (getvar "dimexo")
           p1 (polar p1 a d1)
           p3 (polar p3 a d1)
           p2 (inters p1 p3 p5 p6 nil)
     )
     (command ".dim1" "rot" (angtos (angle q1 q2) 0 0) p1 p2 (&N2S q1 q2) " ")
     (emod (entget (entlast)) (list (aslyts lay0)))
     (command ".dim1" "rot" (angtos (angle q1 q2) 0 0) p2 p3 (&N2S q1 q2) " ")
     (setq e (entlast))
     (emod (entget e) (list (aslyts lay0)))
    )
    ((and (setq lay (&DRAG e0 8)) 
          (= (&DRAG 0) "LINE")
          (setq q1 (&DRAG 10)
                q2 (&DRAG 11)
          )
          (setq e1 (entnext e0))
          (= (&DRAG e1 8) lay)
          (= (&DRAG 0) "LINE")
          (setq p1 (&DRAG 10)
                p2 (&DRAG 11)
          )
          (setq e2 (entnext e1))
          (= (&DRAG e2 8) lay)
          (= (&DRAG 0) "LWPOLYLINE")
          (setq e3 (entnext e2))
          (= (&DRAG e3 8) lay)
          (= (&DRAG 0) "TEXT")
          (setq str (&DRAG 1))
          (setq e4 (entnext e3))
          (= (&DRAG e4 8) lay)
          (= (&DRAG 0) "LINE")
          (setq p3 (&DRAG 10)
                p4 (&DRAG 11)
          )
          (equal (&MIDP p3 q1 q2) (&MIDP p4 q1 q2) 0.01)
     )
     (foreach e (list e0 e1 e2 e3) (entdel e))
     (setq a  (angle p2 p1)
           d1 (distance q1 q2)
     )
     (if (= (read str) (setq d2 (atoi str))) 
       (setq sc  (gtsc d1 d2)
             str ""
       )
       (setq sc (fix (/ &sp &sc)))
     )
     (&GVAL sc)
     (setq d1 (+ (getvar "dimexo") (min (distance p1 q1) (distance p1 q2)))
           p1 (polar q2 a d1)
           p2 (polar q1 a d1)
     )
     (command ".dim1" "ali" p1 p2 (&N2S q1 q2) str)
     (setq e (entlast))
     (emod (entget e) (list (aslyts lay0)))
    )
    (t (setq e nil))
  )
  e
)
(defun no4pm (e0 lay0 / a e lay no pc r ty) 
  (if 
    (and (setq lay (&DRAG e0 8)) 
         (= (&DRAG 0) "CIRCLE")
         (setq pc (&DRAG 10)
               r  (&DRAG 40)
         )
         (setq e (entnext e0))
         (= (&DRAG e 8) lay)
         (= (&DRAG 0) "TEXT")
         (setq a  (&DRAG 50)
               no (&DRAG 1)
         )
    )
    (progn (setq ty (cond ((= lay0 "平面轴线编号") 1) ((= lay0 "详图楼板编号") 2) (t 0))) 
           (&GSCL (nth ty '("钢筋号圆圈直径" "轴号圆圈直径" "板号圆圈直径")) (/ r &sp 0.5))
           (&GSCL (nth ty '("钢筋号文字高度" "轴号文字高度" "板号文字高度")) (* 1.2 (/ r &sp)))
           (&FIND ty pc -1 a lay0 no)
           (entdel e0)
           (entdel e)
           (setq e (entlast))
           (emod (entget e) (list (aslyts lay0)))
    )
    (setq e nil)
  )
  e
)
(defun zfj4pm (e0 lay0 / el lp lt p1 p2 p3 p4 p5 p6 q1 q2 q3 q4 q5 q6) 
  (setq lp (&DRAG e0 10)
        lt (&DRAG 42)
        el (entget e0)
  )
  (mapcar 'set '(p1 p2 p3 p4 p5 p6) lp)
  (cond 
    ((equal lt '(0.0 1.0 0.0 1.0 0.0 0.0) 0.1)
     (setq q3 (polar p3 (angle p4 p3) (- (* 0.5 (distance p2 p3)) g_ygr))
           q2 (polar q3 (angle p3 p2) (+ g_ygr g_ygr))
           q1 (polar q2 (angle p2 p1) g_ygl)
           q4 (polar p4 (angle p3 p4) (- (* 0.5 (distance p2 p3)) g_ygr))
           q5 (polar q4 (angle p4 p5) (+ g_ygr g_ygr))
           q6 (polar q5 (angle p5 p6) g_ygl)
     )
     (emod el (list (aslyts lay0) (cons 43 g_bjw) (list 10 q1 q2 q3 q4 q5 q6)))
    )
    ((equal lt '(0.0 0.0 0.0 0.0) 0.1)
     (setq q1 (polar p2 (angle p2 p1) g_zgl)
           q4 (polar p3 (angle p3 p4) g_zgl)
     )
     (emod el (list (aslyts lay0) (cons 43 g_bjw) (list 10 q1 p2 p3 q4)))
    )
    (t (emod el (list (aslyts lay0) (cons 43 g_bjw))))
  )
)
(defun gj4pm (e0 lay0 w / el lp lt p0 p1 p2) 
  (setq lp (&DRAG e0 10)
        lt (&DRAG 42)
        el (entget e0)
        p1 (car lp)
        p2 (cadr lp)
  )
  (if (< (distance (car lp) (last lp)) mm) (setq w g_gjw))
  (if (and (equal lt '(1.0 1.0 1.0) 0.1) (equal (distance p1 p2) (&DRAG 43) mm)) 
    (progn 
      (setq p0 (&N2S p1 p2)
            p1 (polar p0 (angle p0 p1) (* 0.25 g_djd))
            p2 (polar p0 (angle p0 p2) (* 0.25 g_djd))
      )
      (emod 
        el
        (list (aslyts (strcat lay0 "点钢筋")) 
              (cons 43 (* 0.5 g_djd))
              (list 10 p1 p2 p1)
        )
      )
    )
    (emod el (list (aslyts (strcat lay0 "线钢筋")) (cons 43 w)))
  )
)
(defun memp (l1 l2 / l p1 p2 tf) 
  (setq tf nil)
  (while (and (null tf) (setq p1 (car l1))) 
    (setq l l2)
    (while (and (null tf) (setq p2 (car l))) 
      (setq l  (cdr l)
            tf (< (distance p1 p2) 1)
      )
    )
    (setq l1 (cdr l1))
  )
  tf
)
(defun ad1p (l p tf) 
  (if tf (setq l (reverse l)))
  (setq p1 (car l)
        p2 (cadr l)
  )
  (if (equal (distance p p2) (+ (distance p p1) (distance p1 p2)) mm) 
    (setq l (cdr l))
  )
  (setq l (cons p l))
  (if tf (setq l (reverse l)))
  l
)
(defun ml2pl (ll / l ll1 lp p1 p2 p3 p4 tf) 
  (setq l  (car ll)
        p1 (car l)
        p2 (cadr l)
        lp (list p1 p2)
        ll (cdr ll)
        tf t
  )
  (while (and ll tf) 
    (setq ll1 nil
          tf  nil
    )
    (foreach l ll 
      (setq p3 (car l)
            p4 (cadr l)
      )
      (cond 
        ((< (distance p1 p3) mm)
         (setq lp (ad1p lp p4 nil)
               p1 p4
               tf t
         )
        )
        ((< (distance p1 p4) mm)
         (setq lp (ad1p lp p3 nil)
               p1 p3
               tf t
         )
        )
        ((< (distance p2 p3) mm)
         (setq lp (ad1p lp p4 t)
               p2 p4
               tf t
         )
        )
        ((< (distance p2 p4) mm)
         (setq lp (ad1p lp p3 t)
               p2 p3
               tf t
         )
        )
        (t (setq ll1 (append ll1 (list l))))
      )
    )
    (setq p1 (car lp)
          p2 (last lp)
          ll ll1
    )
  )
  lp
)
(defun lnkpl (e0 lay0 w / e l l1 ll lp p p1 p2 d tf na) 
  (setq tf (= (&DRAG e0 0) "LWPOLYLINE")
        l1 (&DRAG 10)
  )
  (cond 
    ((and tf (= (length l1) 2) (equal (car l1) (cadr l1) &sc)) (entdel e0))
    ((and tf (or (apply '= (cons 1 (&DRAG 42))) (apply '= (cons -1 (&DRAG 42)))))
     (setq p1 (car l1)
           p2 (cadr l1)
           p  (&N2S p1 p2)
           d  (distance p1 p2)
     )
     (entdel e0)
     (if (< d (* 2 (&DRAG 43))) 
       (progn (if (wcmatch lay0 "*柱*") (setq g_hat (cons p g_hat))))
       (progn (command ".donut" d d p "") 
              (setq e0 (entlast))
              (emod (entget e0) (list (aslyts lay0) (cons 43 w)))
              (if (wcmatch lay0 "*柱*") (setq g_lco (cons e0 g_lco)))
       )
     )
    )
    ((and tf 
          (> (length l1) 2)
          (or (setq tf (= (&DRAG 70) 1)) (< (distance (car l1) (last l1)) mm))
     )
     (if (not tf) 
       (progn (setq l1 (reverse (cdr (reverse l1)))) 
              (entdel e0)
              (command ".pline")
              (foreach p l1 (command p))
              (command "c")
              (setq e0 (entlast))
       )
     )
     (emod (entget e0) (list (aslyts lay0) (cons 43 w)))
     (if (wcmatch lay0 "*柱*") (setq g_lco (cons e0 g_lco)))
    )
    (T
     (setq e e0)
     (while 
       (and e 
            (cond 
              ((and (= (&DRAG e 0) "LINE") (wcmatch lay0 "*柱*"))
               (setq l1 (list (&DRAG 10) (&DRAG 11)))
              )
              ((and (= (&DRAG e 0) "LWPOLYLINE") 
                    (= (&DRAG 70) 0)
                    (setq l1 (&DRAG 10))
                    (> (distance (car l1) (last l1)) mm)
               )
               t
              )
              (t nil)
            )
            (or (null l) (memp l1 l))
       )
       (setq p1 (car l1))
       (foreach p2 (cdr l1) 
         (setq ll (cons (list p1 p2) ll)
               p1 p2
         )
       )
       (setq l  (append l1 l)
             e0 e
             e  (entnext e)
       )
       (entdel e0)
     )
     (if ll 
       (progn (setq lp (ml2pl ll)) 
              (setvar "plinewid" w)
              (if (setq tf (< (distance (car lp) (last lp)) mm)) 
                (setq lp (cdr lp))
              )
              (if (and tf (equal (&MIDP (car lp) (cadr lp) (last lp)) 0 mm)) 
                (setq lp (cdr lp))
              )
              (if 
                (and tf 
                     (equal (&MIDP (last lp) (car lp) (cadr (reverse lp))) 0 mm)
                )
                (setq lp (reverse (cdr (reverse lp))))
              )
              (if (> (length lp) 1) 
                (progn (command ".pline") 
                       (foreach p lp (command p))
                       (if tf (command "c") (command ""))
                       (setq e (entlast))
                       (if (wcmatch lay0 "*柱*") (setq g_lco (cons e g_lco)))
                       (emod (entget e) (list (aslyts lay0)))
                )
              )
       )
     )
    )
  )
  e
)
(defun hat4pm (e0 lay0 / e e70 i l l1 ll lp p1 p2 p3 p4 q1 q2) 
  (setq e e0)
  (cond 
    ((= (&DRAG e0 0) "3DFACE")
     (while 
       (and e 
            (= (&DRAG e 0) "3DFACE")
            (setq e70 (&DRAG 70)
                  p1  (&DRAG 10)
                  p2  (&DRAG 11)
                  p3  (&DRAG 12)
                  p4  (&DRAG 13)
            )
            (or (null l) (memp (list p1 p2 p3 p4) l))
       )
       (mapcar 
         '(lambda (i q1 q2) 
            (if (/= (logand e70 i) i) (setq ll (cons (list q1 q2) ll)))
          )
         (list 1 2 4 8)
         (list p1 p2 p3 p4)
         (list p2 p3 p4 p1)
       )
       (setq l  (append (list p1 p2 p3 p4) l)
             e0 e
             e  (entnext e)
       )
       (entdel e0)
     )
    )
    ((= (&DRAG e0 0) "LINE")
     (while 
       (and e 
            (= (&DRAG e 0) "LINE")
            (wcmatch lay0 "*柱*")
            (setq l1 (list (&DRAG 10) (&DRAG 11)))
            (or (null l) (memp l1 l))
       )
       (setq ll (cons l1 ll)
             l  (append l1 l)
             e0 e
             e  (entnext e)
       )
       (entdel e0)
     )
    )
    ((= (&DRAG e0 0) "SOLID")
     (setq p1 (&DRAG 10)
           p2 (&DRAG 11)
           p3 (&DRAG 12)
           p4 (&DRAG 13)
     )
     (entdel e0)
     (if (wcmatch lay0 "*柱*") 
       (setq g_hat (cons (&N2S p2 p3) g_hat))
       (progn (command ".hatch" "solid" "" "n" p1 p2 p4 p3 p1 "" "") 
              (setq e0 (entlast))
       )
     )
    )
  )
  (if (and ll (setq lp (ml2pl ll)) (< (distance (car lp) (last lp)) mm)) 
    (if (wcmatch lay0 "*柱*") 
      (setq lp    (cdr lp)
            i     (length lp)
            p1    (list (/ (apply '+ (mapcar 'car lp)) i) 
                        (/ (apply '+ (mapcar 'cadr lp)) i)
                  )
            g_hat (cons p1 g_hat)
      )
      (progn (command ".hatch" "solid" "" "n") 
             (foreach p1 lp (command p1))
             (command "" "")
             (setq e0 (entlast))
      )
    )
  )
  (if (setq e (entget e0)) (emod e (list (aslyts lay0))))
)
(defun grpzfj (e0 sty / ds e lp ss str minds) 
  (defun minds (lp p0 / d1 d2 p1 p2) 
    (setq p1 (car lp)
          d1 1e10
    )
    (foreach p2 (cdr lp) 
      (setq d2 (abs (&MIDP p0 p1 p2))
            p1 p2
      )
      (if (> d1 d2) (setq d1 d2))
    )
    d1
  )
  (zfj4pm e0 (strcat "平面楼板" sty "钢筋"))
  (setq ss (ssadd e0)
        lp (&DRAG e0 10)
        ds (* &sp 8)
  )
  (if 
    (and (setq e (entnext e0)) 
         (= (&DRAG e 0) "CIRCLE")
         (< (minds lp (&DRAG 10)) ds)
         (setq e (no4pm e (strcat "平面楼板" sty "编号")))
    )
    (ssadd e ss)
  )
  (if 
    (and (setq e (entnext e0)) 
         (= (&DRAG e 0) "TEXT")
         (setq str (&DRAG 1))
         (/= (atoi str) (read str))
         (< (minds lp (&DRAG 10)) ds)
    )
    (progn (txt4pm e (strcat "平面楼板" sty "文字") g_tsc) 
           (ssadd e ss)
           (setq e0 (entnext e0))
    )
  )
  (if 
    (and (setq e (entnext e0)) 
         (wcmatch (&DRAG e 0) "LINE,TEXT,DIMENSION")
         (< (minds lp (&DRAG 10)) ds)
         (setq e (dim4pm e (strcat "平面楼板" sty "尺寸")))
    )
    (progn (ssadd e ss) (if (eq (entnext e0) e) (setq e0 e)))
  )
  (if 
    (and (setq e (entnext e0)) 
         (wcmatch (&DRAG e 0) "LINE,TEXT,DIMENSION")
         (< (minds lp (&DRAG 10)) ds)
         (setq e (dim4pm e (strcat "平面楼板" sty "尺寸")))
    )
    (progn (ssadd e ss) (if (eq (entnext e0) e) (setq e0 e)))
  )
  (if (> (sslength ss) 1) (&DGAR ss))
  e0
)
(defun Pmfbxj (e0 sty / e lay) 
  (setq e   e0
        lay (&DRAG e 8)
  )
  (command ".pedit" e0 "j")
  (while (and (setq e (entnext e)) (= (&DRAG e 0) "LWPOLYLINE") (= (&DRAG 8) lay)) 
    (command e)
  )
  (command "" "")
  (grpzfj e0 sty)
)
(defun grpjzb (e0 lay0 / e l lay le p0 p1 p2 pt ss tf) 
  (setq ss (ssadd e0))
  (cond 
    ((= (&DRAG e0 0) "LINE")
     (setq p0  (&DRAG 11)
           lay (&DRAG 8)
           e   e0
     )
     (emod (entget e) (list (aslyts lay0)))
     (while 
       (and (setq e (entnext e)) 
            (= (&DRAG e 0) "TEXT")
            (= lay (&DRAG 8))
            (setq p1 (&DRAG 10)
                  p2 (polar p1 (+ (&DRAG 50) _pi2) (&DRAG 40))
            )
            (< (abs (&MIDP p0 p1 p2)) (&DRAG 40))
       )
       (if (wcmatch (&DRAG 1) "*L*(*)*,*Z*") 
         (if pt 
           (setq tf nil)
           (setq pt p1
                 tf t
           )
         )
         (setq tf t)
       )
       (if tf (progn (setq le (cons (cons p1 e) le)) (txt4pm e lay0 g_tsc)))
     )
     (if pt 
       (progn 
         (setq le (@ran3 
                    (mapcar '(lambda (l) (cons (distance (car l) pt) (cdr l))) le)
                  )
         )
         (foreach l le (ssadd (cdr l) ss))
         (&DGAR ss)
       )
     )
    )
    ((and (= (&DRAG e0 0) "TEXT") 
          (setq lay (&DRAG 8)
                e   (entnext e0)
          )
          (= (&DRAG e 0) "LINE")
          (= lay (&DRAG 8))
     )
     (ssadd e ss)
     (emod (entget e) (list (aslyts lay0)))
     (txt4pm e0 lay0 g_tsc)
     (&DGAR ss)
    )
    (t (txt4pm e0 lay0 g_tsc))
  )
)
(defun pl2l (e0 lay0 / e) 
  (if (= (&DRAG e0 0) "LWPOLYLINE") 
    (progn 
      (emod 
        (entget e0)
        (list '(40 . 0.0) '(41 . 0.0) '(43 . 0.0) (aslyts lay0))
      )
      (setq e (entlast))
      (command ".explode" e0)
      (while (setq e (entnext e)) 
        (if (= (&DRAG e 0) "LINE") (setq g_lwl (cons e g_lwl)))
      )
    )
    (emod (entget e0) (list (aslyts lay0)))
  )
)
(defun face2l (e0 lay0 / e70 i p1 p2 p3 p4 q1 q2) 
  (if (= (&DRAG e0 0) "3DFACE") 
    (progn 
      (setq e70 (&DRAG 70)
            p1  (&DRAG 10)
            p2  (&DRAG 11)
            p3  (&DRAG 12)
            p4  (&DRAG 13)
      )
      (entdel e0)
      (mapcar 
        '(lambda (i q1 q2) 
           (if (/= (logand e70 i) i) 
             (progn (command ".line" q1 q2 "") 
                    (emod (entget (entlast)) (list (aslyts lay0)))
             )
           )
         )
        (list 1 2 4 8)
        (list p1 p2 p3 p4)
        (list p2 p3 p4 p1)
      )
    )
    (emod (entget e0) (list (aslyts lay0)))
  )
)
(defun emod (el ln / e el1 ipl ispl l l1) 
  (if (not (member (setq e (cdr (assoc -1 el))) g_le)) 
    (progn (if (null (assoc 62 ln)) (setq ln (cons '(62 . 256) ln))) 
           (setq ispl (= (cdr (assoc 0 el)) "LWPOLYLINE")
                 ipl  0
                 g_le (cons e g_le)
           )
           (foreach l el 
             (setq l1 (assoc (car l) ln))
             (cond 
               ((and ispl (= (car l) 10) l1)
                (setq l   (cons 10 (nth ipl (cdr l1)))
                      ipl (1+ ipl)
                )
               )
               (l1 (setq l l1))
             )
             (setq el1 (append el1 (list l)))
           )
           (entmod el1)
    )
  )
)
(defun aslyts (lyx / lay) 
  (if (not (setq lay (cdr (assoc lyx g_lay)))) 
    (setq lay   (&LJIG lyx T T)
          g_lay (cons (cons lyx lay) g_lay)
    )
  )
  (cons 8 lay)
)
(defun intco (/ e ec ew i l ll p1 p2 p3 p4 q1 q2) 
  (princ "\n正在处理墙柱相交...")
  (foreach ew g_lwl 
    (setq p1 (&DRAG ew 10)
          p2 (&DRAG 11)
    )
    (if (> (distance p1 p2) (* 60 &sc)) 
      (foreach ec g_lco 
        (setq l (&DRAG ec 10))
        (setq p3 (car l)
              p4 (last l)
              ll nil
        )
        (if (> (distance p3 p4) mm) 
          (setq l  (cons p4 l)
                p3 p4
          )
        )
        (foreach p4 (cdr l) 
          (if (setq p3 (inters p1 p2 p3 p4)) (setq ll (cons p3 ll)))
          (setq p3 p4)
        )
        (cond 
          ((and ll 
                (= (length ll) 1)
                (setq q1 (car ll))
                (> (distance q1 p1) mm)
                (> (distance q1 p2) mm)
           )
           (if (@pinl p1 (&DRAG ec 10) mm) 
             (setq i 10)
             (setq i  11
                   p3 p1
                   p1 p2
                   p2 p3
             )
           )
           (if (> (distance q1 p2) (* 60 &sc)) 
             (emod (entget ew) (list (cons i q1)))
             (entdel ew)
           )
          )
          ((and ll 
                (= (length ll) 2)
                (setq q1 (car ll)
                      q2 (cadr ll)
                )
                (> (distance q1 q2) mm)
                (> (distance q1 p1) mm)
                (> (distance q1 p2) mm)
                (> (distance q2 p1) mm)
                (> (distance q2 p2) mm)
           )
           (if (> (distance q1 p1) (distance q2 p1)) 
             (setq p3 q1
                   q1 q2
                   q2 p3
             )
           )
           (setq ew (entget ew))
           (entmake ew)
           (setq e (entget (entlast)))
           (emod ew (list (cons 11 q1)))
           (emod e (list (cons 10 q2)))
          )
        )
      )
      (entdel ew)
    )
  )
)
(defun hatco (/ e ec i l lc lp p0 ss) 
  (if g_lco 
    (progn (princ "\n正在处理柱子填充...") 
           (foreach ec g_lco 
             (setq lp (&DRAG ec 10))
             (if (< (distance (car lp) (last lp)) mm) (setq lp (cdr lp)))
             (setq i  (length lp)
                   p0 (list (/ (apply '+ (mapcar 'car lp)) i) 
                            (/ (apply '+ (mapcar 'cadr lp)) i)
                      )
                   lc (cons (cons p0 ec) lc)
             )
           )
           (setvar "pickstyle" 2)
           (setq ss (ssadd))
           (foreach p0 g_hat 
             (foreach l lc 
               (if (< (distance p0 (car l)) mm) 
                 (progn (command ".-bhatch" "p" "s" "s" (cdr l) "" "") 
                        (setq e (entlast))
                        (emod (entget e) (list (aslyts "平面柱子填充")))
                        (ssadd e ss)
                 )
               )
             )
           )
           (if (> (sslength ss) 0) (command ".draworder" ss "" "b"))
    )
    (princ "\n**单步转换是无法建立柱子与填充的关系，柱子填充将被删除。\n  请使用TSSD柱填实功能进行处理！")
  )
)
(defun chgle (le / e el g_2gx g_bjw g_djd g_gjw g_lay g_lco g_le g_lwl g_plw g_tsc 
              g_ygl g_ygr g_zgl g_zjw i l lay mm msg nam ops ss
             ) 
  (setvar "cmdecho" 0)
  (setq ops (getvar "pickstyle"))
  (setvar "pickstyle" 0)
  (setvar "ltscale" (* 10 &sp))
  (&INTS "粗体文字" 1 t)
  (setq g_tsc (cadr (&INTS "钢筋文字" 1 t))
        g_tsc (if g_tsc g_tsc 0.7)
        g_plw (&GSYS "多义线宽度")
        g_plw (if g_plw (* g_plw &sp) (* 0.5 &sp))
        g_2gx (&GSYS "二级钢弯钩形式")
        g_2gx (if g_2gx (= g_2gx 0) t)
        g_zgl (&GSYS "直斜钩长")
        g_zgl (* &sp (if g_zgl g_zgl 1.5))
        g_ygl (&GSYS "圆钩长度")
        g_ygl (* &sp (if g_ygl g_ygl 1.2))
        g_ygr (&GSYS "圆钩半径")
        g_ygr (* &sp (if g_ygr g_ygr 0.5))
        g_djd (&GSYS "点筋直径")
        g_djd (* &sp (if g_djd g_djd 1.0))
        g_zjw (&GSYS "主筋宽度")
        g_zjw (* &sp (if g_zjw g_zjw 0.6))
        g_gjw (&GSYS "箍筋宽度")
        g_gjw (* &sp (if g_gjw g_gjw 0.3))
        g_bjw (&GSYS "板筋宽度")
        g_bjw (* &sp (if g_bjw g_bjw 0.3))
        msg   (strcat "\r图中共有实体 " (itoa (length le)) " 个 , 已经处理: ")
        i     1
        mm    (* &sp 1e-3)
        g_lay nil
        g_le  nil
        g_lwl nil
        g_lco nil
        g_hat nil
  )
  (foreach l le 
    (if (zerop (rem (setq i (1+ i)) 50)) (princ (strcat msg " " (itoa i) " 个...")))
    (mapcar 'set '(e lay nam) l)
    (if (and e lay (setq el (entget e))) 
      (progn 
        (cond 
          ((= lay "轴线") (emod el (list (aslyts "平面轴线"))))
          ((= lay "轴线标注")
           (cond 
             ((and (wcmatch nam "LINE,DIMENSION") (dim4pm e "平面轴线尺寸")))
             ((and (= nam "CIRCLE") (no4pm e "平面轴线编号")))
             (t (emod el (list (aslyts "平面轴线尺寸"))))
           )
          )
          ((= lay "梁实线") (emod el (list (aslyts "平面主梁实线"))))
          ((= lay "梁虚线") (emod el (list (aslyts "平面主梁"))))
          ((= lay "次梁实线") (emod el (list (aslyts "平面次梁实线"))))
          ((= lay "柱子")
           (cond 
             ((= nam "3DFACE") (entdel e))
             ((= nam "SOLID") (hat4pm e "平面柱子填充"))
             (T (lnkpl e "平面柱子" g_plw))
           )
          )
          ((= lay "柱涂实") (hat4pm e "平面柱子填充"))
          ((= lay "砼墙") (pl2l e "平面砼墙"))
          ((= lay "砖墙") (pl2l e "平面砖墙"))
          ((= lay "墙涂实") (emod el (list (aslyts "平面砼墙填充"))))
          ((= lay "门窗") (emod el (list (aslyts "平面门"))))
          ((= lay "门虚线") (emod el (list (aslyts "平面门虚线"))))
          ((= lay "悬挑板") (emod el (list (aslyts "平面楼板实线"))))
          ((= lay "洞口") (emod el (list (aslyts "洞口"))))
          ((= lay "洞口阴影") (hat4pm e "洞口虚线"))
          ((= lay "图名底线") (emod el (list (aslyts "粗线符号"))))
          ((wcmatch lay "正筋,负筋")
           (cond 
             ((= nam "LWPOLYLINE") (grpzfj e lay))
             ((wcmatch nam "LINE,DIMENSION") (dim4pm e (strcat "平面楼板" lay "尺寸")))
             (t (emod el (list (aslyts (strcat "平面楼板" lay "尺寸")))))
           )
          )
          ((= lay "板编号")
           (if (not (and (= nam "CIRCLE") (no4pm e "平面楼板编号"))) 
             (emod el (list (aslyts "平面楼板编号")))
           )
          )
          ((= lay "钢筋表")
           (cond 
             ((= nam "LWPOLYLINE") (lnkpl e "表格线钢筋" g_bjw))
             ((and (= nam "CIRCLE") (no4pm e "表格编号")))
             ((= nam "TEXT") (txt4pm e "表格文字" g_tsc))
             (t (emod el (list (aslyts "表格线"))))
           )
          )
          ((= lay "板剖面") (emod el (list (aslyts "详图楼板"))))
          ((= lay "文字说明")
           (if (= nam "TEXT") (txt4pm e "文字" g_tsc) (emod el (list (aslyts "文字"))))
          )
          ((= lay "构件标注")
           (cond 
             ((wcmatch nam "LINE,DIMENSION") (dim4pm e "尺寸"))
             ((= nam "TEXT") (txt4pm e "文字" g_tsc))
             (t (emod el (list (aslyts "文字"))))
           )
          )
          ((= lay "梁水平集中标注") (grpjzb e "平面主梁水平集中标"))
          ((= lay "梁水平原位标注") (txt4pm e "平面主梁水平原位标" g_tsc))
          ((= lay "梁竖直集中标注") (grpjzb e "平面主梁竖直集中标"))
          ((= lay "梁竖直原位标注") (txt4pm e "平面主梁竖直原位标" g_tsc))
          ((= lay "梁附加钢筋") (lnkpl e "平面主梁水平附加筋" g_gjw))
          ((= lay "梁附加钢筋标注") (txt4pm e "平面主梁水平附加筋标注" g_tsc))
          ((= lay "柱平法截面")
           (cond 
             ((= nam "3DFACE") (face2l e "详图柱子"))
             ((= nam "CIRLCE") (emod el (list (aslyts "详图柱子"))))
             (t (txt4pm e "详图柱子文字" g_tsc))
           )
          )
          ((= lay "柱集中标注") (grpjzb e "平面柱子集中标"))
          ((= lay "柱原位标注") (txt4pm e "平面柱子原位标" g_tsc))
          ((= lay "柱截面标注") (dim4pm e "平面柱子尺寸"))
          ((= lay "柱详图标注") (dim4pm e "详图柱子尺寸"))
          ((= lay "柱平法钢筋") (gj4pm e "详图柱子" g_gjw))
          ((= lay "基础平面") (emod el (list (aslyts "平面独立基础"))))
          ((= lay "基础详图") (emod el (list (aslyts "详图独立基础"))))
          ((= lay "基础梁") (emod el (list (aslyts "平面主梁实线"))))
          ((= lay "基础拉梁") (emod el (list (aslyts "平面次梁实线"))))
          ((= lay "基础梁水平标注") (grpjzb e "平面主梁水平尺寸"))
          ((= lay "基础梁垂直标注") (grpjzb e "平面主梁竖直尺寸"))
          ((= lay "基础钢筋") (gj4pm e "详图独立基础" g_zjw))
          ((= lay "筏板上筋")
           (setq lay "负筋")
           (cond 
             ((= nam "LWPOLYLINE") (Pmfbxj e lay))
             ((wcmatch nam "LINE,DIMENSION") (dim4pm e (strcat "平面楼板" lay "尺寸")))
             (t (emod el (list (aslyts (strcat "平面楼板" lay "尺寸")))))
           )
          )
          ((= lay "筏板下筋")
           (setq lay "正筋")
           (cond 
             ((= nam "LWPOLYLINE") (Pmfbxj e lay))
             ((wcmatch nam "LINE,DIMENSION") (dim4pm e (strcat "平面楼板" lay "尺寸")))
             (t (emod el (list (aslyts (strcat "平面楼板" lay "尺寸")))))
           )
          )
          ((= lay "基础钢筋标注")
           (cond 
             ((and (= nam "CIRCLE") (no4pm e "详图独立基础编号")))
             ((= nam "TEXT") (txt4pm e "详图独立基础文字" g_tsc))
             (t (emod el (list (aslyts "详图独立基础文字"))))
           )
          )
          ((= lay "基础尺寸标注")
           (if (not (dim4pm e "详图独立基础尺寸")) (emod el (list (aslyts "详图独立基础尺寸"))))
          )
          ((= lay "基础图名") (emod el (list (aslyts "详图独立基础编号"))))
          ((= lay "基础标高") (emod el (list (aslyts "细线符号"))))
          ((= lay "图框") (emod el (list (aslyts "图框"))))
        )
      )
    )
  )
  (setvar "pickstyle" ops)
  (princ (strcat msg " " (itoa (1- i)) " 个..."))
  (if (and g_lco g_lwl) (intco))
  (if g_hat (hatco))
  (setq ss (ssadd))
  (foreach e g_le (if (= (&DRAG e 0) "DIMENSION") (ssadd e ss)))
  (if (> (sslength ss) 0) (&CODE ss))
)
(defun OffDel (/ e lay slay ss) 
  (tblnext "layer" t)
  (while (setq e (tblnext "layer")) 
    (if (minusp (cdr (assoc 62 e))) 
      (setq lay  (cdr (assoc 2 e))
            slay (if slay (strcat slay "," lay) lay)
      )
    )
  )
  (if (and slay (setq ss (ssget "x" (list (cons 8 slay))))) (ss::del ss))
)
(defun io4pm (fn / e l lay le ll llay x) 
  (if 
    (and (> (&UTXT) -1) 
         (if (entlast) t (prompt "\n**先打开待转换的图形后，再运行本命令！"))
         (if (setq fn (findfile fn)) t (prompt "\n**无法找到转换文件, 请重新安装Tssd！"))
         (setq ll (&BEG fn))
    )
    (progn 
      (foreach l ll 
        (setq lay (car l))
        (foreach x (cdr l) (setq llay (cons (cons x lay) llay)))
      )
      (OffDel)
      (setq e  (entnext)
            le (list (list e (cdr (assoc (&DRAG e 8) llay)) (&DRAG 0)))
      )
      (while (setq e (entnext e)) 
        (setq le (append le 
                         (list (list e (cdr (assoc (&DRAG e 8) llay)) (&DRAG 0)))
                 )
        )
      )
      (chgle le)
      (command ".purge" "la" "*")
      (while (not (zerop (getvar "cmdactive"))) (command "y"))
      (princ "\n图形转换完毕! ")
    )
  )
  (&TSTY)
)
(setfunhelp "c:ZBPJT" "Tssd" "ZBPJT")
(defun c:ZBPJT () (io4pm "jk2010-3.pmb"))
(setfunhelp "c:ZLPJT" "Tssd" "ZLPJT")
(defun c:ZLPJT () (io4pm "jk2010-3.pml"))
(setfunhelp "c:ZZPJT" "Tssd" "ZZPJT")
(defun c:ZZPJT () (io4pm "jk2010-3.pmz"))
(setfunhelp "c:ZJCHT" "Tssd" "ZJCHT")
(defun c:ZJCHT () (io4pm "jk2010-3.pmj"))
(defun @sfnd (st st1 / i j n) 
  (setq i 1
        j (strlen st1)
        n (1+ (strlen st))
  )
  (while (and (< i n) (/= (substr st i j) st1)) (setq i (1+ i)))
  i
)
(defun io1pm (na hlp / e lay lay1 le ll no si sl ss) 
  (defun mkll (na / fn i j ll ln) 
    (if (and (setq fn (findfile na)) (setq fn (open fn "r"))) 
      (progn 
        (while (setq ln (read-line fn)) 
          (if (wcmatch ln "Item#*=*") 
            (setq i  (1+ (@sfnd ln "="))
                  j  (@sfnd ln ",")
                  ll (append ll (list (substr ln i (- j i))))
            )
          )
        )
        (close fn)
      )
    )
    (if ll ll (prompt (strcat "\n**无法找到转换文件" na ", 请重新安装Tssd！")))
  )
  (if 
    (and (> (&UTXT) -1) 
         (if (entlast) t (prompt "\n**先打开待转换的图形后，再运行本命令！"))
         (setq ll (mkll na))
         (new_dialog "pment" (lib::dcl "tssdio"))
    )
    (progn (setq no "0") 
           (#dis_list "ll" no)
           (action_tile "ll" "(setq no $value)")
           (action_tile "help" "(help \"tssd\" hlp)")
           (if 
             (and (= 1 (start_dialog)) 
                  (setq lay (nth (atoi no) ll)
                        e   (entsel 
                              (strcat "\n选取任一要转换的〖" (nth (atoi no) ll) "〗<退出>: ")
                            )
                  )
             )
             (progn 
               (setq lay1 (&DRAG (car e) 8)
                     ss   (ssget "x" (list (cons 8 lay1)))
                     si   0
                     sl   (sslength ss)
               )
               (while (< si sl) 
                 (setq e  (ssname ss si)
                       si (1+ si)
                       le (cons (list e lay (&DRAG e 0)) le)
                 )
               )
               (chgle le)
               (princ "\n图形转换完毕! ")
             )
           )
    )
  )
  (&TSTY)
)
(setfunhelp "c:FBZBT" "Tssd" "FBZBT")
(defun c:FBZBT () (io1pm "jk2010-3.pmb" "FBZBT"))
(setfunhelp "c:FBZLT" "Tssd" "FBZLT")
(defun c:FBZLT () (io1pm "jk2010-3.pml" "FBZLT"))
(setfunhelp "c:FBZZT" "Tssd" "FBZZT")
(defun c:FBZZT () (io1pm "jk2010-3.pmz" "FBZZT"))
(setfunhelp "c:FBZJC" "Tssd" "FBZJC")
(defun c:FBZJC () (io1pm "jk2010-3.pmj" "FBZJC"))