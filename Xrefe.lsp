(defun C:XrefBeg (/ lay wdict en ss layerl dict no ent entl name cm os flag blkl 
                  bkname pt xscal yscal ang date att
                 ) 
  (defun lay (/ flag blkl bkname layerl) 
    (setq flag T)
    (while (setq blkl (tblnext "BLOCK" flag)) 
      (setq bkname (strcase (cdr (assoc 2 blkl)))
            flag   nil
      )
      (if (cdr (assoc 1 blkl)) 
        (if (not (member bkname layerl)) 
          (setq layerl (append layerl (list bkname)))
        )
      )
    )
    layerl
  )
  (defun wdict (ent name flag / laye write hand oldlay layerenl) 
    (defun laye (layer / len i char) 
      (setq len (strlen layer)
            i   1
      )
      (while (> len i) 
        (setq char (substr layer i 1))
        (if (= char "$") 
          (if (= (substr layer i 3) "$0$") 
            (setq layer (substr layer (+ i 3) len)
                  i     len
            )
          )
          (setq i (1+ i))
        )
      )
      layer
    )
    (defun write (hand / dictname newdict dict newdict xname datalist) 
      (if (setq dictname (dictsearch (namedobjdict) "TSSD_DIC_XREF")) 
        (setq newdict (cdr (assoc -1 dictname)))
        (setq dict     (list (cons 0 "DICTIONARY") (cons 100 "AcDbDictionary"))
              dictname (entmakex dict)
              newdict  (dictadd (namedobjdict) "TSSD_DIC_XREF" dictname)
        )
      )
      (setq datalist (append (list (cons 0 "XRECORD") (cons 100 "AcDbXrecord")) 
                             (list (cons 1 hand))
                     )
      )
      (setq xname (entmakex datalist)
            no    (1+ no)
      )
      (dictadd newdict (strcat "DATA_RECORD_" (itoa no)) xname)
    )
    (defun del_color (el / cl col flag exdata) 
      (setq col (cdr (assoc 62 el)))
      (if (or (not col) (= col 0) (= col 256)) 
        (setq flag T
              col  (cdr (assoc 62 (tblsearch "Layer" (cdr (assoc 8 el)))))
        )
      )
      (cond 
        ((or (= col 1) (= col 10)) (setq cl 14))
        ((or (= col 2) (= col 50)) (setq cl 54))
        ((or (= col 3) (= col 90)) (setq cl 94))
        ((or (= col 4) (= col 130)) (setq cl 134))
        ((or (= col 5) (= col 170)) (setq cl 174))
        ((or (= col 6) (= col 210)) (setq cl 214))
        ((or (= col 7) (= col 255)) (setq cl 251))
        ((> col 250) (setq cl col))
        (T (setq cl (+ col 4)))
      )
      (if flag 
        (setq el (append el (list (cons 62 cl))))
        (setq el (subst (cons 62 cl) (assoc 62 el) el))
      )
      (regapp "Tsz_Color")
      (setq exdata (list (list -3 (list "Tsz_Color" (cons 1000 (itoa cl)))))
            el     (append el exdata)
      )
      (entmod el)
    )
    (if (= flag 0) (command ".xref" "b" name))
    (command ".explode" ent)
    (while (setq en (entnext en)) 
      (setq enl    (entget en)
            hand   (cdr (assoc 5 enl))
            oldlay (cdr (assoc 8 enl))
            layer  (laye oldlay)
      )
      (write hand)
      (del_color enl)
      (if (not (wcmatch (cdr (assoc 0 enl)) "TCH_*")) 
        (progn 
          (if (not (tblsearch "LAYER" layer)) 
            (command ".Rename" "la" oldlay layer)
          )
          (setq enl (subst (cons 8 layer) (assoc 8 enl) enl))
          (entmod enl)
        )
      )
    )
  )
  (defun find_path (name / flag blkl bkname path) 
    (setq flag T)
    (while (setq blkl (tblnext "BLOCK" flag)) 
      (setq bkname (strcase (cdr (assoc 2 blkl)))
            flag   nil
      )
      (if (eq bkname name) (setq path (cdr (assoc 1 blkl))))
    )
    path
  )
  (if (< (&UTXT) 0) (exit))
  (setq layerl (lay)
        len    (length layerl)
  )
  (if (setq dict (dictsearch (namedobjdict) "TSSD_DIC_XREF")) 
    (setq no (fix (/ (- (length dict) 9) 2)))
    (setq no 0)
  )
  (while (> len 0) 
    (if (and (setq en (entlast)) (setq ss (ssget "x" (list (cons 0 "INSERT"))))) 
      (progn 
        (while (setq ent (ssname ss 0)) 
          (ssdel ent ss)
          (setq entl (entget ent)
                name (strcase (cdr (assoc 2 entl)))
                date (getvar "cdate")
                date (rtos (* date 100000000) 2 8)
          )
          (if (member name layerl) 
            (progn 
              (setq en    (entlast)
                    path  (find_path (strcase name))
                    pt    (cdr (assoc 10 entl))
                    xscal (cdr (assoc 41 entl))
                    yscal (cdr (assoc 42 entl))
                    ang   (cdr (assoc 50 entl))
              )
              (if (/= (cdr (assoc 60 entl)) 1) 
                (progn 
                  (command ".Insert" 
                           (strcat "Tsz_xref" date "=" path)
                           pt
                           xscal
                           yscal
                           ang
                  )
                  (while (not (zerop (getvar "cmdactive"))) (command ""))
                  (setq ent  (entlast)
                        entl (append entl (list (cons 60 1)))
                  )
                  (entmod entl)
                  (wdict ent name 1)
                )
              )
            )
          )
        )
      )
    )
    (setq len (1- len))
  )
  (repeat 2 
    (command ".purge" "all" "*")
    (while (not (zerop (getvar "cmdactive"))) (command "y"))
  )
  (&TSTY)
)
(defun C:XrefBack (/ dictl entdict ent entl hand en cm os ss len el) 
  (if (< (&UTXT) 0) (exit))
  (if (setq dictl (dictsearch (namedobjdict) "TSSD_DIC_XREF")) 
    (progn (setq entdict (cdr (assoc -1 dictl))) 
           (while (setq ent (cdr (assoc 350 dictl))) 
             (setq entl  (entget ent)
                   hand  (cdr (assoc 1 entl))
                   dictl (cdr (member (cons 350 ent) dictl))
             )
             (if (setq en (handent hand)) (if (entget en) (entdel en)))
           )
           (entdel entdict)
    )
  )
  (if (setq ss (ssget "x" (list (cons 0 "INSERT") (cons 60 1)))) 
    (while (setq en (ssname ss 0)) 
      (ssdel en ss)
      (setq enl (entget en)
            enl (subst (cons 60 0) (assoc 60 enl) enl)
      )
      (entmod enl)
    )
  )
  (repeat 2 
    (command ".purge" "all" "*")
    (while (not (zerop (getvar "cmdactive"))) (command "y"))
  )
  (command ".xref" "reload" "*")
  (&TSTY)
)
(setfunhelp "c:Gbczhc" "Tssd" "Gbczhc")
(defun C:Gbczhc (/ cm os en lay ss enl flag) 
  (if (< (&UTXT) 0) (exit))
  (if (setq en (car (entsel "\n选择需要关闭参照图层上的任意实体<退出>: "))) 
    (if 
      (setq ss (ssget "x" 
                      (list (cons 8 (cdr (assoc 8 (entget en)))) 
                            (list -3 (list "Tsz_Color"))
                      )
               )
      )
      (while (setq en (ssname ss 0)) 
        (ssdel en ss)
        (setq enl (entget en))
        (if (cdr (assoc 60 enl)) 
          (setq enl (subst (cons 60 1) (assoc 60 enl) enl))
          (setq enl (append enl (list (cons 60 1))))
        )
        (entmod enl)
      )
    )
  )
  (&TSTY)
)
(setfunhelp "c:Dkczhc" "Tssd" "Dkczhc")
(defun C:Dkczhc (/ cm os en lay ss enl flag) 
  (if (< (&UTXT) 0) (exit))
  (if (setq en (car (entsel "\n选择需要打开参照图层上的任意实体<全部>: "))) 
    (setq ss (ssget "x" 
                    (list (cons 8 (cdr (assoc 8 (entget en)))) 
                          (list -3 (list "Tsz_Color"))
                    )
             )
    )
    (setq ss (ssget "x" (list (list -3 (list "Tsz_Color")))))
  )
  (if ss 
    (while (setq en (ssname ss 0)) 
      (ssdel en ss)
      (setq enl (entget en))
      (if (cdr (assoc 60 enl)) 
        (setq enl (subst (cons 60 0) (assoc 60 enl) enl))
        (setq enl (append enl (list (cons 60 0))))
      )
      (entmod enl)
    )
  )
  (&TSTY)
)