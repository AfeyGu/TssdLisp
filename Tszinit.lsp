(if (and (not (member "tszsys.arx" (arx))) (findfile "tszsys.arx")) 
  (arxload "tszsys.arx" t)
)
(if (null &UTXT) (quit))
(vl-load-com)
(defun *merr* (msg) 
  (setvar "cmdecho" 0)
  (cond 
    ((= msg "quit / exit abort") nil)
    ((= (getvar "cmdactive") 8))
    ((/= (getvar "cmdactive") 0) (command) (if &TSTY (&TSTY) (princ)))
    (t (if &TSTY (&TSTY) (princ)))
  )
  (if (listp le@Hlt) (foreach e le@Hlt (redraw e 4)))
  (setq le@Hlt nil)
  (princ)
)
(if (load "tsdebug" nil) (tsdebug) (setq *error* *merr*))
(defun lib::lsp (file / fl) 
  (cond 
    ((member file tssd_lsp))
    ((and (setq fl (&SSYS file)) (load fl))
     (setq tssd_lsp (cons file tssd_lsp))
     (princ)
    )
    ((null *error*) (prompt (strcat "\n**系统错误，无法加载 [" file ".lsp]！\n")) (exit))
    (t (&TSTY))
  )
)
(defun lib::dcl (file / id) 
  (cond 
    ((cdr (assoc file tssd_dcl)))
    ((and (setq file (&SSYS (strcat file ".dcl"))) 
          (> (setq id (load_dialog file)) 0)
     )
     (setq tssd_dcl (cons (cons file id) tssd_dcl))
     id
    )
    ((null *error*) (princ (strcat "\n**系统错误，无法找到对话框[ " file "]！")) (exit))
    (t (&TSTY))
  )
)
(defun c:cln () 
  (if (null *error*) 
    (setq *error* *merr*)
    (progn (foreach ll tssd_dcl (unload_dialog (cdr ll))) 
           (setq tssd_lsp nil
                 tssd_dcl nil
                 *error*  nil
           )
    )
  )
  (&TSTY)
)
(Defun tsz:RwMod (new / f fn ll ln old) 
  (if new (setq new (strcat "CurModel=" new)))
  (setq fn (strcat (&SGET) "prg\\tszabout.ini"))
  (if (setq f (open fn "r")) 
    (progn 
      (while (setq ln (read-line f)) 
        (setq ll (cons ln ll))
        (if (wcmatch (strcase ln t) "curmodel=*") (setq old ln))
      )
      (setq ll (reverse ll))
      (close f)
    )
  )
  (cond 
    ((and new old)
     (if (and (/= new old) (setq f (open fn "w"))) 
       (progn (foreach ln ll (write-line (if (= ln old) new ln) f)) (close f))
     )
    )
    (new (if (setq f (open fn "a")) (progn (write-line new f) (close f))))
    (old
     (setq old (strcase (substr old (+ (vl-string-search "=" old) 2)) t))
     (if 
       (not 
         (wcmatch old 
                  "tssd,tspt,tsjc,t3sd,tslc,tswd,tsyt,tstk,t3pt,tsbdi,tsmg,tszq,tspr,tswt,tshvac,tselec"
         )
       )
       (setq old "tssd")
     )
    )
    (t (setq old "tssd"))
  )
  old
)
(defun c:Tszhelp (/ fl tsmod) 
  (if 
    (setq tsMod (tsz:RwMod nil)
          fl    (findfile (strcat tsMod ".chm"))
    )
    (help fl)
    (princ "\n**未发现帮助文件！")
  )
  (princ)
)
(defun c:readme (/ fl tsmod) 
  (setq tsMod (tsz:RwMod nil))
  (if (not (wcmatch tsMod "tssd,tspt,tslc,tsjc,t3sd,tswd,tsyt")) 
    (setq tsMod "tssd")
  )
  (if (setq fl (findfile (strcat tsMod ".txt"))) 
    (startapp "notepad.exe" fl)
    (princ "\n**未发现 Readme 文件！")
  )
  (princ)
)
(defun c:jshtj (/ fl tsmod) 
  (setq tsMod (tsz:RwMod nil))
  (if (not (wcmatch tsMod "tssd,tspt,tsjc,tslc,t3sd")) (setq tsMod "tssd"))
  (if 
    (setq tsMod (strcat tsMod "条件.chm")
          fl    (findfile tsMod)
    )
    (help fl)
    (alert (strcat "\n**未发现【" tsMod "】文件！"))
  )
  (princ)
)
(defun c:q2a (/ fl) 
  (if (setq fl (findfile "q_a.chm")) (help fl) (alert "\n**未发现【q_a.chm】文件！"))
  (princ)
)
(defun c:s_tab (/ fl) 
  (if (setq fl (findfile "s_tab.chm")) (help fl) (alert "\n**未发现【s_tab.chm】文件！"))
  (princ)
)
(defun c:Gfcx (/ fl) 
  (if (setq fl (findfile "结构规范查询.chm")) (help fl) (alert "\n**未发现【结构规范查询.chm】文件！"))
  (princ)
)
(defun c:Ie () 
  (setvar "cmdecho" 0)
  (command "browser" "http://www.tsz.com.cn")
  (princ)
)
(defun c:Arun (/ kw) 
  (initget "Yes No")
  (setq kw (getkword "\n启动时是否提示选择AutoCAD平台? [是(Y)/否(N)]<是>: ")
        kw (if (= kw "No") 1 0)
  )
  (vl-registry-write 
    (strcat "HKEY_CURRENT_USER\\Software\\TszCAD\\" (getvar "cprofile"))
    "Arun"
    kw
  )
  (princ)
)
(defun c:appload (/ cpf opf reg) 
  (setq key (vlax-product-key)
        reg (strcat "HKEY_CURRENT_USER\\" key "\\Profiles")
        opf (vl-registry-read reg)
        cpf (getvar "cprofile")
  )
  (vl-registry-write reg "" cpf)
  (princ "...\n")
  (command ".acad_appload.appload")
  (vl-registry-write reg "" opf)
  (if (> (atof (getvar "acadver")) 15.9) 
    (progn 
      (setq reg (strcat "HKEY_CURRENT_USER\\" key "\\Applications\\AcadAppload"))
      (vl-registry-write reg "LOADCTRLS" 15)
    )
    (progn 
      (if (getenv "ProgramW6432") 
        (setq key (strcat "Software\\Wow6432Node" 
                          (substr key (strlen "Software\\"))
                  )
        )
      )
      (setq reg (strcat "HKEY_LOCAL_MACHINE\\" key "\\Applications\\AcadAppload"))
      (vl-registry-write reg "LOADCTRLS" 6)
    )
  )
  (princ)
)
(Defun c:tspmlayer () (setvar "cmdecho" 0) (initdia) (command ".layer") (princ))
(defun tsz:HidPop (mnu) 
  (if mnu (setq mnu (strcase mnu T)))
  (vlax-for pop 
            (vla-get-menubar (vlax-get-acad-object))
            (if 
              (or (null mnu) 
                  (= mnu 
                     (strcase (vla-get-name (vla-get-Parent (vla-get-Parent pop))) 
                              t
                     )
                  )
              )
              (vla-RemoveFromMenuBar pop)
            )
  )
)
(defun ChgMod (tsMod / cmd i l larx lm lmod lprg lsp oldmod pop x TbarShow RePop) 
  (Defun TbarShow (mnu lbar Show / bar $aver$) 
    (setq mnu    (strcase mnu T)
          $aver$ (atof (getvar "acadver"))
    )
    (if (and (> $aver$ 16.1) (= mnu "tssduser")) (setq mnu "tsuserui"))
    (if (and lbar (menugroup mnu)) 
      (vlax-for bar 
                (vla-get-toolbars 
                  (vla-item (vla-get-MenuGroups (vlax-get-acad-object)) mnu)
                )
                (if (member (strcase (vla-get-name bar)) lbar) 
                  (vla-put-Visible bar (if Show :vlax-true :vlax-false))
                )
      )
    )
  )
  (defun RePop (lpop / lpb pop) 
    (vlax-for pop 
              (vla-get-menubar (vlax-get-acad-object))
              (if 
                (= "tssd" 
                   (strcase (vla-get-name (vla-get-Parent (vla-get-Parent pop))) t)
                )
                (setq lpb (cons (strcase (substr (vla-get-TagString pop) 6)) lpb))
              )
    )
    (setq tf nil)
    (while (and (null tf) (setq pop (car lpop))) 
      (setq tf   (not (member (strcase pop) lpb))
            lpop (cdr lpop)
      )
    )
    tf
  )
  (setq lMod   '(("tssd" 
                   "tssd"
                   ("Tssd1" "Tssd2" "Tsz1" "Tsz2" "Tsz3" "Tsz4" "Tsz5")
                 )
                 ("tspt" 
                   "tspt"
                   ("Tspt1" "Tspt2" "Tspt3" "Tspt4" "Tsz1" "Tsz2" "Tsz3" "Tsz4" 
                            "Tsz5"
                   )
                 )
                 ("tsjc" 
                   "tsjc"
                   ("Tsjc1" "Tsjc2" "Tsjc3" "Tsjc4" "Tsjc5" "Tsjc6" "Tsjc7" "Tsz1" 
                            "Tsz2" "Tsz3" "Tsz4" "Tsz5"
                   )
                 )
                 ("t3sd" 
                   "钢结构"
                   ("T3sd1" "T3sd2" "T3sd3" "Tsz1" "Tsz2" "T3sd4" "Tsz4" "Tsz5")
                   ("T3SD_常用工具")
                 )
                 ("tslc" "剪力墙" ("Tslc1" "Tsz1" "Tsz2" "Tsz3" "Tsz4" "Tsz5"))
                 ("tswd" 
                   "水工"
                   ("Tswd1" "Tswd2" "Tswd3" "Tsz1" "Tsz2" "Tsz3" "Tsz4" "Tsz5")
                 )
                 ("tsyt" "烟囱" ("Tsyt1" "Tsz1" "Tsz2" "Tsz3" "Tsz4" "Tsz5"))
                 ("tstk" 
                   "水池"
                   ("Tstk1" "Tstk2" "Tstk3" "Tstk4" "Tsz1" "Tsz5")
                   ("TSTK_常用工具")
                 )
                 ("t3pt" "t3pt" ("T3pt1" "T3pt2" "T3pt3" "T3pt4" "Tsz1" "Tsz5"))
                 ("tsbdi" 
                   "tsbdi"
                   ("Tsbdi1" "Tsbdi2" "Tsbdi3" "Tsbdi4" "Tsbdi5" "Tsbdi6" "Tsz1" 
                             "Tsz5"
                   )
                 )
                 ("tsmg" "门刚" ("Tsmg1" "Tsmg2" "Tsmg3" "Tsmg4" "Tsz1" "Tsz5"))
                 ("tszq" "栈桥" ("Tszq1" "Tszq2" "Tszq3" "Tszq4" "Tsz1" "Tsz5"))
                 ("tspr" "管廊" ("Tspr1" "Tspr2" "Tspr3" "Tspr4" "Tsz1" "Tsz5"))
                 ("tswt" "给排水" ("Tswt1" "Tswt2" "Tswt3" "Tswt4" "Tsz1" "Tsz5"))
                 ("tshvac" 
                   "暖通"
                   ("Tshvac1" "Tshvac2" "Tshvac3" "Tshvac4" "Tsz1" "Tsz5")
                 )
                 ("tselec" 
                   "电器"
                   ("Tselec1" "Tselec2" "Tselec3" "Tselec4" "Tsz1" "Tsz5")
                 )
                )
        tsMod  (strcase tsMod T)
        oldMod (tsz:RwMod nil)
  )
  (if (setq l (assoc tsMod lMod)) 
    (progn (vl-propagate 'tsMod) 
           (vl-load-all (strcat (&SGET) "prg\\doc_mod"))
           (if (= (read tsMod) 'tssd) 
             (foreach l 
               '(("TSPMXCHCHX" "XCHCHX")
                 ("TSPMXSHBH" "XSHBH")
                 ("TSPMXCFB" "GBXK")
                 ("TSPMXCLJ" "GBXK")
                 ("TSPMJDJC" "JDJC")
                 ("MMWJXCL" "LJXCHL")
                 ("TSPMZHZHX" "ZHZHX")
                 ("TSPMZHCHCBZH" "ZHCHCBZH")
                 ("TSPMDZHBZH" "DZHBZH")
                 ("TSPMFHGJZH" "FHGJZH")
                 ("TSPMCCHTBZH" "CCHTBZH")
                 ("TSPMCJCHJC" "CJCHJC")
                 ("TSPMPFTD" "PFTD")
                 ("TSPMLJ" "LJ")
                 ("TSPMPDX" "PDX")
                 ("TSPMLJFH" "LJFH")
                 ("TSPMWZSHR" "WZSHR")
                 ("TSPMWZS" "WZS")
                 ("TSPMXGDQFS" "XGDQFS")
                 ("TSPMQPWZ" "QPWZ")
                 ("TSPMWZDQ" "WZDQ")
                 ("TSPMWZXZH" "WZXZH")
                 ("TSPMTZHHJJ" "TZHHJJ")
                 ("TSPMBHDZKB" "BHDZKB")
                 ("TSPMBGHZH" "BGHZH")
                 ("TSPMCHFZCH" "CHFZCH")
                 ("TSPMLJWZ" "LJWZ")
                 ("TSPMWZDD" "WZDD")
                 ("TSPMFHTH" "FHTH")
                 ("TSPMQCHWZ" "QCHWZ")
                 ("TSPMCHZHTH" "CHZHTH")
                 ("TSPMWZQHZH" "WZQHZH")
                 ("TSPMBHDZXG" "BHDZXG")
                 ("TSPMBREAK" "BREAK")
                 ("TSPMBZHDK" "BZHDK")
                 ("TSPMWZYD" "WZYD")
                 ("TSPMXCGB" "XCGB")
                 ("TSPMXCXSH" "XCXSH")
                 ("TSPMZCHJ" "ZCHJ")
                 ("TSPMZZK" "ZZK")
                 ("TSPMZTJ" "ZTJ")
                 ("TSPMZYCH" "ZYCH")
                 ("TSPMKJSHT" "KJSHT")
                 ("TSPMKSHSHT" "KSHSHT")
                 ("TSPMZLJSS" "ZLJSS")
                )
               (eval 
                 (list 'defun 
                       (read (strcat "c:" (car l)))
                       nil
                       (list 'if 
                             (read (strcat "c:" (cadr l)))
                             (read (strcat "(c:" (cadr l) ")"))
                             (list 'command (cadr l))
                       )
                       '(princ)
                 )
               )
             )
           )
           (tsz:RwMod tsMod)
           (setvar "UserS4" (cadr l))
           (if (RePop (caddr l)) 
             (progn (tsz:HidPop "tssd") 
                    (setq i 15)
                    (foreach pop (caddr l) 
                      (menucmd (strcat "P" (itoa i) "=+tssd." pop))
                      (setq i (1+ i))
                    )
                    (TbarShow "tssduser" (apply 'append (mapcar 'cadddr lMod)) nil)
                    (TbarShow "tssduser" (cadddr l) T)
                    (if (/= tsMod oldMod) (&PATH tsMod))
             )
           )
           (setq lPrg '(("tssd" 
                          ("tssdcalccad.arx" "tssdcalccad2.arx" "tssddet.arx" 
                                             "tspmmain.arx" "tspmbeam.arx" "tspmboard.arx" 
                                             "tspmcolumn.arx" "tspmwall.arx" "tspmplatform.arx" 
                                             "tspmtext.arx" "tspmtool.arx"
                          )
                        )
                        ("tspt" 
                          ("tssdcalccad.arx" "tssdcalccad2.arx" "tssddet.arx" 
                                             "tsptcad1.arx" "tsptcad2.arx" "tsptcad3.arx" "tsptcad4.arx" 
                                             "tsptcad5.arx"
                          )
                        )
                        ("tsjc" 
                          ("tssdcalccad.arx" "tssdcalccad2.arx" "tssddet.arx" 
                                             "tsjcaerial.arx" "tsjccad1.arx" "tsjccad2.arx" 
                                             "tsjccad3.arx" "tsjccad4.arx" "tsjctools.arx"
                          )
                        )
                        ("t3sd" 
                          ("tssdcalccad.arx" "tssdcalccad2.arx" "tssddet.arx" 
                                             "t3sdcad1.arx" "t3sdcad2.arx" "t3sdcad3.arx" "t3sdcad4.arx" 
                                             "t3sdcad5.arx" "t3sdcad6.arx"
                          )
                        )
                        ("tslc" 
                          ("tssdcalccad.arx" 
                            "tssdcalccad2.arx"
                            "tssddet.arx"
                            "tslccad2.arx"
                          )
                        )
                        ("tswd" 
                          ("tssdcalccad.arx" 
                            "tssdcalccad2.arx"
                            "tssddet.arx"
                            "tswdcad1.arx"
                            "tswdcad2.arx"
                          )
                        )
                        ("tsyt" 
                          ("tssdcalccad.arx" 
                            "tssdcalccad2.arx"
                            "tssddet.arx"
                            "tsyccad1.arx"
                          )
                        )
                        ("tstk" ("tnkhoapp.arx"))
                        ("t3pt" ("t3ptpluginu.arx"))
                        ("tsbdi" ("tsbdipluginu.arx"))
                        ("tsmg" ("tsmgpluginu.arx"))
                        ("tszq" ("tstbpluginu.arx"))
                        ("tspr" ("tsprpluginu.arx"))
                        ("tswt" ("tswtpluginu.arx"))
                        ("tshvac" ("tshvacpluginu.arx"))
                        ("tselec" ("tselecpluginu.arx"))
                       )
           )
           (if 
             (setq lArx (arx)
                   lm   (cadr (assoc tsMod lPrg))
             )
             (foreach x lm 
               (if (and (not (member x lArx)) (findfile x)) (arxload x T))
             )
           )
    )
    (princ (strcat "***[" tsMod "]模块没有正确注册！"))
  )
  (princ)
)
(defun s::startup (/ $aver$ bcad btssd i l larx pop reg st tag tsmod x fn LoadMenu) 
  (Defun LoadMenu (mnu / ext fmnu ret tbar $aver$) 
    (setq $aver$ (atof (getvar "acadver")))
    (if (and (> $aver$ 16.1) (= mnu "tssduser")) (setq mnu "tsuserui"))
    (setq ext  (cond ((> $aver$ 17.1) "cuix") ((> $aver$ 16.1) "cui") (t "mnu"))
          fmnu (findfile (strcat mnu "." ext))
    )
    (if (= mnu "tssd") 
      (if 
        (and (not (setq ret (wcmatch (strcase (getvar "menuname") t) "*tssd"))) 
             fmnu
        )
        (command "_.menu" fmnu)
      )
      (if (and (not (setq ret (menugroup mnu))) fmnu) (command "_.menuload" fmnu))
    )
    ret
  )
  (setvar "cmdecho" 0)
  (if 
    (and (setq fn (getvar "cprofile")) 
         (wcmatch fn "T[s3]*")
         (setq fn (findfile (strcat fn ".arg")))
    )
    (progn (vl-file-delete fn) 
           (setq reg (vl-registry-read 
                       (strcat "HKEY_CURRENT_USER\\" (vlax-product-key))
                       "RoamableRootFolder"
                     )
                 st  (strcat (&SGET) "\\Prg")
           )
           (if (setq fn (findfile (strcat reg "Support\\Acetmain.Mnl"))) 
             (progn (vl-file-delete fn) 
                    (vl-file-copy (strcat st "\\Acetmain.Mnl") fn)
             )
           )
           (if 
             (setq fn (vl-registry-read 
                        (strcat "HKEY_CURRENT_USER\\" 
                                (vlax-product-key)
                                "\\Profiles\\"
                                (getvar "cprofile")
                                "\\General"
                        )
                        "PrinterStyleSheetDir"
                      )
             )
             (progn (setq fn (strcat (vl-string-right-trim "\\" fn) "\\Tspt.Ctb")) 
                    (vl-file-delete fn)
                    (vl-file-copy (strcat st "\\Tspt.Ctb") fn)
             )
           )
    )
  )
  (setq reg (strcat "HKEY_CURRENT_USER\\" (vlax-product-key) "\\Profiles"))
  (foreach st (vl-registry-descendents reg) 
    (if (and reg (wcmatch st "<<*>>")) 
      (progn (vl-registry-write reg "" st) (setq reg nil))
    )
  )
  (if (not (member "appload.arx" (arx))) (arxload "appload.arx"))
  (command "undefine" "appload")
  (if 
    (and 
      (setq x (vl-registry-read "HKEY_CURRENT_USER\\EUDC\\936" 
                                "SystemDefaultEUDCFont"
              )
      )
      (not (wcmatch (strcase x) "*TSSDENG.TTE"))
      (setq x (findfile "TszEUDC.exe"))
    )
    (startapp x "/S")
  )
  (setq l    '("tszdline.arx" "tszdraw.arx" "tszent.arx" "tszplan.arx" "tszrein.arx" 
               "tszsign.arx" "tszsteel.arx" "tsztext.arx"
              )
        larx (arx)
  )
  (foreach x l (if (and (not (member x larx)) (findfile x)) (arxload x T)))
  (setq $aver$ (atof (getvar "acadver"))
        tsMod  (tsz:RwMod nil)
        bTssd  (LoadMenu "tssd")
        bCad   (LoadMenu "acad")
  )
  (LoadMenu "tssduser")
  (LoadMenu "tsbonus")
  (if (and bTssd bCad) 
    (progn 
      (setq i     0
            bTssd nil
            bCad  nil
      )
      (vlax-for pop 
                (vla-get-menubar (vlax-get-acad-object))
                (setq i   (1+ i)
                      tag (strcase (vla-get-tagstring pop))
                )
                (if (and (= i 1) (= tag "ID_MNFILE")) (setq bCad T))
                (if (and (> i 11) (= tag "ID_MNTSZ1")) (setq bTssd T))
      )
    )
  )
  (if (not (and bCad bTssd)) 
    (progn 
      (if (and (> $aver$ 16.1) (/= (getvar "wscurrent") "AutoCAD 经典")) 
        (setvar "wscurrent" "AutoCAD 经典")
      )
      (tsz:HidPop "acad")
      (setq i 0)
      (while (< (setq i (1+ i)) 15) 
        (menucmd (strcat "p" (itoa i) "=+acad.pop" (itoa i)))
      )
      (if (and (null (menugroup "EXPRESS")) c:expresstools) (c:expresstools))
    )
  )
  (chgmod tsMod)
  (command ".undo" "c" "n" ".undo" "")
  (load "tssdapp" nil)
  (if tssdapp::init (tssdapp::init))
  (princ)
)
(princ)