(lib::lsp "MyFan.lsp")
 (setfunhelp "c:ljwz" "Tssd" "ljwz")
 (defun c:Ljwz
	(/ e el l le msg pt str)
	(if
		(> (&UTXT) -1)
		(progn
			(setq str "")
			(while
				(setq
					l (@gstr
						nil
						(if (= str "") "\n选取要连接文字<退出>: " "\n下一组文字<结束>: ")
						"TEXT,MTEXT"
						'T
					)
				)
				(setq
					e (caddr l)
					le (cons e le)
					str (strcat str (car l))
				)
				(@Hlt e 3)
			)
			(foreach e le (@Hlt e 4))
			(if
				(and (/= str "") (setq pt (&OSNP "\n点取新文字的左下角<退出>: ")))
				(progn
					(setq
						el (entget (last le))
						el (subst (cons 10 (@wcs pt)) (assoc 10 el) el)
						el (subst (cons 1 str) (assoc 1 el) el)
						el (subst (cons 72 0) (assoc 72 el) el)
						el (subst (cons 73 0) (assoc 73 el) el)
					)
					(entmake el)
					(initget "Yes No")
					(setq pt (getkword "\n是否删除原有文字? [是(Y)/否(N)]<是>: "))
					(if (/= pt "No") (foreach e le (entdel e)))
				)
			)
		)
	)
	(&TSTY)
)
 (defun txt_mak
	(str el p10 ax ay dy e72 e73 / dx p101)
	(setq
		el (subst (cons 1 str) (assoc 1 el) el)
		dx (caadr (textbox el))
	)
	(cond
		(
			(and (= e72 0) (= e73 0))
			(setq el (subst (cons 10 (@wcs p10)) (assoc 10 el) el))
		)
		(
			(and (= e72 1) (member e73 '(0 1)))
			(setq
				p101 (polar p10 ax (* 0.5 dx))
				el (subst (cons 11 (@wcs p101)) (assoc 11 el) el)
			)
		)
		(
			(and (= e72 2) (member e73 '(0 1)))
			(setq
				p101 (polar p10 ax dx)
				el (subst (cons 11 (@wcs p101)) (assoc 11 el) el)
			)
		)
		(
			(and (member e72 '(3 5)) (= e73 0))
			(setq
				el (subst (cons 10 (@wcs p10)) (assoc 10 el) el)
				p101 (polar p10 ax dx)
				el (subst (cons 11 (@wcs p101)) (assoc 11 el) el)
			)
		)
		(
			(or
				(and (= e72 4) (= e73 0))
				(and (= e72 1) (= e73 2))
			)
			(setq
				p101 (polar p10 ax (* 0.5 dx))
				p101 (polar p101 ay (* 0.5 dy))
				el (subst (cons 11 (@wcs p101)) (assoc 11 el) el)
			)
		)
		(
			(and (= e72 0) (= e73 1))
			(setq
				p101 p10
				el (subst (cons 11 (@wcs p101)) (assoc 11 el) el)
			)
		)
		(
			(and (= e72 0) (= e73 2))
			(setq
				p101 (polar p10 ay (* 0.5 dy))
				el (subst (cons 11 (@wcs p101)) (assoc 11 el) el)
			)
		)
		(
			(and (= e72 2) (= e73 2))
			(setq
				p101 (polar p10 ax dx)
				p101 (polar p101 ay (* 0.5 dy))
				el (subst (cons 11 (@wcs p101)) (assoc 11 el) el)
			)
		)
		(
			(and (= e72 0) (= e73 3))
			(setq
				p101 (polar p10 ay dy)
				el (subst (cons 11 (@wcs p101)) (assoc 11 el) el)
			)
		)
		(
			(and (= e72 1) (= e73 3))
			(setq
				p101 (polar p10 ax (* 0.5 dx))
				p101 (polar p101 ay dy)
				el (subst (cons 11 (@wcs p101)) (assoc 11 el) el)
			)
		)
		(
			(and (= e72 2) (= e73 3))
			(setq
				p10 (polar p10 ax dx)
				p10 (polar p10 ay dy)
				el (subst (cons 11 (@wcs p101)) (assoc 11 el) el)
			)
		)
	)
	(entmake el)
	(entlast)
)
 (defun text--split
	(ss
		msg-p
		/
		ax
		ay
		dx1
		dx2
		dy
		e
		e72
		e73
		el
		l
		p10
		si
		sl
		sst
		str
		sum
	)
	(setq si 0 sl (sslength ss) sum 0 sst (ssadd))
	(while
		(< si sl)
		(setq
			e (ssname ss si)
			si (1+ si)
			el (entget e)
			l (textbox el)
			dx2 (caadr l)
			dy (+ (cadar l) (cadadr l))
			str (&DRAG e 1)
			l (@txt2lst (&GENT str))
			p10 (&DRAG 10)
			ax (&DRAG 50)
			ay (+ ax _pi2)
			e72 (&DRAG 72)
			e73 (&DRAG 73)
		)
		(while
			l
			(if
				(/= (car l) " ")
				(progn
					(ssadd (txt_mak (car l) el p10 ax ay dy e72 e73) sst)
					(setq sum (1+ sum))
				)
			)
			(if
				(setq l (cdr l))
				(setq
					str (apply'strcat l)
					el (subst (cons 1 str) (assoc 1 el) el)
					dx1 (caadr (textbox el))
					p10 (polar p10 ax (- dx2 dx1))
					dx2 dx1
				)
			)
		)
		(entdel e)
	)
	(if
		msg-p
		(princ (strcat "\n拆分出" (itoa sum) "个单个文字!"))
		sst
	)
)
 (setfunhelp "c:Chfzch" "Tssd" "chfzch")
 (defun c:Chfzch
	(/ ss)
	(if
		(and
			(> (&UTXT) -1)
			(setq ss (&DSTR "\n选择要拆分的文字<退出>: " '( (0 . "TEXT"))))
		)
		(text--split ss T)
	)
	(&TSTY)
)
 (defun tx_pos
	(el l dl dx / d1 d2 l1 l2 stm str)
	(setq stm "" l1 nil l2 nil d2 dl l (reverse l))
	(while
		l
		(setq
			str (car l)
			l (cdr l)
			l2 (cons str l2)
			stm (strcat str stm)
			el (subst (cons 1 stm) (assoc 1 el) el)
			d1 (- dl (caadr (textbox el)))
		)
		(if (< d1 dx d2) (setq l1 l l nil) (setq d2 d1))
	)
	(if l1 (cons (reverse l1) l2))
)
 (setfunhelp "c:Wzdd" "Tssd" "wzdd")
 (defun c:Wzdd
	(/ ax ay dx dx2 dy e e72 e73 el l p10 p11 pt str)
	(if
		(and
			(> (&UTXT) -1)
			(setq l (@gstr nil "\n选取要打断的文字<退出>: " "TEXT" nil))
			(setq str (car l) e (caddr l) pt (cadddr l))
			(@Hlt e 3)
			(setq pt (&OSNP pt "\n点取要打断的位置<退出>: "))
		)
		(progn
			(setq
				el (entget e)
				l (textbox el)
				dx2 (caadr l)
				dy (+ (cadar l) (cadadr l))
				p10 (&DRAG e 10)
				ax (&DRAG 50)
				ay (+ ax _pi2)
				e72 (&DRAG 72)
				e73 (&DRAG 73)
				p11 (polar p10 ay 1e3)
				dx (&MIDP pt p10 p11)
				l (@txt2lst str)
			)
			(if
				(setq l (tx_pos el l dx2 dx))
				(progn
					(setq str "")
					(foreach x (car l) (setq str (@mrg2str str x)))
					(txt_mak str el p10 ax ay dy e72 e73)
					(setq str "")
					(foreach x (cdr l) (setq str (@mrg2str str x)))
					(setq
						el (subst (cons 1 str) (assoc 1 el) el)
						dx (- dx2 (caadr (textbox el)))
						p10 (polar p10 ax dx)
					)
					(txt_mak str el p10 ax ay dy e72 e73)
					(entdel e)
				)
				(princ "\n**无法在该位置打断!")
			)
		)
	)
	(if e (@Hlt e 4))
	(&TSTY)
)
 (setfunhelp "c:Ljchz" "Tssd" "ljchz")
 (defun c:Ljchz
	(/
		tran
		ptyl
		an
		an_list
		e
		en
		en_pt_list
		i
		j
		k
		l
		lay
		le
		len
		pt
		py_list
		ss
		st
		str
		str1
		sty
		zg
		zk
		en1
		enl
		s
	)
	(defun tran
		(p an / dis ka p_x p_y px py t_a)
		(setq px (car p) py (cadr p))
		(cond
			(
				(equal an (* 0.5 pi) 0.001)
				(setq p_x py p_y (* -1 px))
			)
			(
				(equal an pi 0.001)
				(setq p_x (* -1 px) p_y (* -1 py))
			)
			(
				(equal an (* 1.5 pi) 0.001)
				(setq p_x (* -1 py) p_y px)
			)
			(T
				(setq
					dis (sqrt (+ (* px px) (* py py)))
					ka (atan (/ py px))
				)
				(cond
					( (and (< px 0) (> py 0)) (setq ka (+ pi ka)))
					( (and (< px 0) (< py 0)) (setq ka (+ pi ka)))
					( (and (> px 0) (< py 0)) (setq ka (+ (* 2 pi) ka)))
				)
				(setq
					t_a (- ka an)
					p_x (* dis (cos t_a))
					p_y (* dis (sin t_a))
				)
			)
		)
		(list p_x p_y (caddr p))
	)
	(defun ptyl
		(ss
			/
			p_y
			a
			an
			ang
			an_list
			e
			el
			en_list
			i
			j
			l
			len
			no
			pt
			pt10
			pyl
			zg
			zk
		)
		(defun p_y
			(a_l / p_y_x la m py py_list y m py_xl no)
			(defun p_y_x
				(y_l / ly n px px_list x no)
				(setq ly (length y_l) n 0 px (car pt))
				(while
					(< n ly)
					(setq
						px_list (nth n y_l)
						n (1+ n)
						x (car
							(if (/= an 0) (tran (cadr px_list) ang) (cadr px_list))
						)
					)
					(if (> px x) (setq no n))
				)
				(if
					no
					(setq
						py_list (@aditem
							py_list
							(1+ no)
							(list an pt10 zg zk sty str lay e)
						)
					)
					(setq
						py_list (cons (list an pt10 zg zk sty str lay e) py_list)
					)
				)
				py_list
			)
			(setq la (length a_l) m 0 py (cadr pt) no nil)
			(while
				(< m la)
				(setq
					py_list (nth m a_l)
					m (1+ m)
					py_xl nil
					y (cadr
						(if
							(/= an 0)
							(tran (cadar py_list) ang)
							(cadar py_list)
						)
					)
				)
				(if
					(equal py y (/ zg 3))
					(setq py_xl (p_y_x py_list) no m m la)
					(if (< py y) (setq no m))
				)
			)
			(if
				py_xl
				(setq an_list (@subst an_list (- no 1) py_xl))
				(if
					no
					(setq
						an_list (@aditem
							an_list
							(1+ no)
							(list (list an pt10 zg zk sty str lay e))
						)
					)
					(setq
						an_list (cons (list (list an pt10 zg zk sty str lay e)) an_list)
					)
				)
			)
			an_list
		)
		(setq i 0 len (sslength ss))
		(while
			(< i len)
			(setq
				e (ssname ss i)
				el (entget e)
				pt10 (&DRAG e 10)
				zg (&DRAG 40)
				an (fix (* 180 (/ (setq ang (&DRAG 50)) pi)))
				zk (&DRAG 41)
				sty (cdr (assoc 7 el))
				str (cdr (assoc 1 el))
				lay (cdr (assoc 8 el))
				tf 0
				j 0
				l (length en_list)
				i (1+ i)
			)
			(if (/= an 0) (setq pt (tran pt10 ang)) (setq pt pt10))
			(while
				(< j l)
				(setq
					an_list (nth j en_list)
					a (caaar an_list)
					j (1+ j)
					tf 1
					pyl nil
					no nil
				)
				(if
					(equal an a 5)
					(setq pyl (p_y an_list) no j j l)
					(if (> an a) (setq no j))
				)
			)
			(if
				(= tf 0)
				(setq
					en_list (list (list (list (list an pt10 zg zk sty str lay e))))
				)
				(if
					pyl
					(setq en_list (@subst en_list (- no 1) pyl))
					(if
						no
						(setq
							en_list (@aditem
								en_list
								(1+ no)
								(list (list (list an pt10 zg zk sty str lay e)))
							)
						)
						(setq
							en_list (cons
								(list (list (list an pt10 zg zk sty str lay e)))
								en_list
							)
						)
					)
				)
			)
		)
		en_list
	)
	(defun del_g
		(en / el)
		(setq el (entget en))
		(if
			(= (cdr (assoc 48 el)) 99)
			(progn
				(setq el (subst (cons 48 1) (assoc 48 el) el))
				(entmod el)
				(entupd en)
			)
		)
		(if
			(cdr (assoc 102 el))
			(progn (entdel en) (entmake el) (entlast))
			en
		)
	)
	(if
		(and
			(> (&UTXT) -1)
			(setq ss (&DSTR "\n选择要连接成组的文字<退出>: " '( (0 . "TEXT"))))
		)
		(progn
			(setq
				ss (text--split ss nil)
				en_pt_list (ptyl ss)
				len (length en_pt_list)
				i 0
			)
			(while
				(< i len)
				(setq
					an_list (nth i en_pt_list)
					le (length an_list)
					i (1+ i)
					j 0
					s nil
					s (ssadd)
				)
				(while
					(< j le)
					(setq
						py_list (nth j an_list)
						l (length py_list)
						j (1+ j)
						k 0
					)
					(if
						(> l 1)
						(progn
							(setq
								st (car py_list)
								en (nth 7 st)
								enl (entget en)
								en1 (assoc 1 enl)
								str ""
							)
							(while
								(< k l)
								(setq
									st (nth k py_list)
									str1 (nth 5 st)
									e (nth 7 st)
									k (1+ k)
								)
								(if (/= str1 " ") (setq str (strcat str str1)))
								(entdel e)
							)
							(setq enl (subst (cons 1 str) en1 enl))
							(if
								(and (member (&DRAG en 72) '(3 5)) (zerop (&DRAG 73)))
								(setq
									enl (subst
										(cons
											11
											(@wcs
												(polar (&DRAG 10) (&DRAG 50) (caadr (textbox enl)))
											)
										)
										(assoc 11 enl)
										enl
									)
								)
							)
							(entmake enl)
							(ssadd (del_g (entlast)) s)
						)
						(ssadd (del_g (nth 7 (car py_list))) s)
					)
					(ssadd (del_g (nth 7 (car py_list))) s)
				)
				(&DGAR s)
			)
		)
	)
	(&TSTY)
)
 (setfunhelp "c:Shpwz" "Tssd" "shpwz")
 (defun c:Shpwz
	(/
		ax
		ay
		dx2
		dy
		e
		e72
		e73
		el
		hi
		l
		le
		p0
		p10
		si
		sl
		ss
		ss1
		str
	)
	(if
		(and
			(> (&UTXT) -1)
			(setq ss (&DSTR "\n选择要竖排的文字<退出>: " '( (0 . "text"))))
		)
		(progn
			(setq si 0 sl (sslength ss) str "")
			(while
				(< si sl)
				(setq
					e (ssname ss si)
					si (1+ si)
					str (strcat str (&DRAG e 1))
					le (cons e le)
				)
			)
			(setq
				e (ssname ss 0)
				el (entget e)
				l (textbox el)
				dx2 (caadr l)
				dy (+ (cadar l) (cadadr l))
				p10 (&DRAG 10)
				ax (&DRAG 50)
				ay (+ ax _pi2)
				e72 (&DRAG 72)
				e73 (&DRAG 73)
				hi (* -1.5 (&DRAG 40))
				ss1 (ssadd)
				p0 p10
			)
			(foreach
				str
				(@txt2lst (&GENT str))
				(setq
					e (txt_mak str el p10 ax ay dy e72 e73)
					p10 (polar p10 ay hi)
				)
				(ssadd e ss1)
			)
			(if
				(> (sslength ss1) 0)
				(progn
					(foreach e le (entdel e))
					(&DGAR ss1)
					(&END "\n点取竖排文字的插入点<退出>: " ss1 p0)
				)
			)
		)
	)
	(&TSTY)
)
 (defun txt_spc
	(kw
		kw1
		kw2
		/
		ax
		ay
		dx
		dx2
		e
		el
		i
		l
		l1
		l2
		n
		p10
		p11
		pt
		str
		spc_chk
	)
	(defun spc_chk
		(str / i sl tf)
		(if
			(or
				(wcmatch str (strcat "*" (&FLD "KZZF1") "*"))
				(wcmatch str (strcat "*" (&FLD "KZZF2") "*"))
				(wcmatch str (strcat "*" (&FLD "KZZF3") "*"))
				(wcmatch str (strcat "*" (&FLD "KZZF4") "*"))
				(wcmatch str (strcat "*" (&FLD "KZZF5") "*"))
				(wcmatch str (strcat "*" (&FLD "KZZF6") "*"))
				(wcmatch str (strcat "*" (&FLD "YQ1") "*"))
				(wcmatch str (strcat "*" (&FLD "YQ2") "*"))
				(wcmatch str (strcat "*" (&FLD "YQ3") "*"))
				(wcmatch str (strcat "*" (&FLD "YQ4") "*"))
				(wcmatch str (strcat "*" (&FLD "YQ5") "*"))
				(wcmatch str (strcat "*" (&FLD "YQ6") "*"))
			)
			(setq tf (prompt "\n**无法将控制字符转换成上下标!"))
			(progn
				(setq sl (strlen str) i 1)
				(while
					(<= i sl)
					(if
						(> (ascii (substr str i 1)) 159)
						(setq tf (prompt "\n**无法将中文转换成上下标!") sl 0)
						(setq i (1+ i) tf t)
					)
				)
			)
		)
		tf
	)
	(if
		(and
			(> (&UTXT) -1)
			(setq
				l (@gstr
					nil
					(strcat "\n选取要改为" kw "的文字(选取点靠近第一个" kw "文字)<退出>: ")
					"TEXT"
					nil
				)
			)
		)
		(progn
			(setq
				str (car l)
				e (caddr l)
				pt (cadddr l)
				el (entget e)
				l (textbox el)
				dx2 (caadr l)
				p10 (&DRAG e 10)
				ax (&DRAG 50)
				ay (+ ax _pi2)
				p11 (polar p10 ay 1e3)
				dx (&MIDP pt p10 p11)
				l (tx_pos el (@txt2lst str) dx2 dx)
				l1 (car l)
				l2 (cdr l)
				str (car l2)
				kw1 (&FLD kw1)
				kw2 (&FLD kw2)
			)
			(@Hlt e 3)
			(if
				(spc_chk str)
				(progn
					(initget (+ 2 4))
					(setq
						n (getint (strcat "\n输入改为" kw "的字符数 <1>: "))
						n (if n n 1)
						l1 (append l1 (list kw1 str))
						l2 (cdr l2)
						i 1
					)
					(while
						(< i n)
						(setq str (car l2))
						(if
							(spc_chk str)
							(setq l1 (append l1 (list str)) l2 (cdr l2) i (1+ i))
							(setq n 0)
						)
					)
					(if
						(> n 0)
						(progn
							(setq l1 (append l1 (list kw2)))
							(foreach str l2 (setq l1 (append l1 (list str))))
							(setq str "")
							(foreach x l1 (setq str (@mrg2str str x)))
							(setq el (subst (cons 1 str) (assoc 1 el) el))
							(entmod el)
						)
					)
				)
			)
			(@Hlt e 4)
		)
	)
	(&TSTY)
)
 (setfunhelp "c:Ggshb" "Tssd" "ggshb")
 (defun c:Ggshb () (txt_spc "上标" "KZZF1" "KZZF2"))
 (setfunhelp "c:Ggxb" "Tssd" "ggxb")
 (defun c:Ggxb () (txt_spc "下标" "KZZF3" "KZZF4"))
 (defun spc_txt
	(kw kw1 kw2 / e el ie is j l l1 n n1 n2 sti str tf)
	(if
		(and
			(> (&UTXT) -1)
			(setq
				l (@gstr
					nil
					(strcat "\n选取要把" kw "恢复正常的文字<退出>: ")
					"TEXT"
					nil
				)
			)
		)
		(progn
			(setq
				str (car l)
				e (caddr l)
				el (entget e)
				kw1 (&FLD kw1)
				n1 (strlen kw1)
				kw2 (&FLD kw2)
				n2 (strlen kw2)
				n (strlen str)
				is 1
				ie 1
				j 0
				l nil
			)
			(while
				(<= ie n)
				(setq sti (substr str ie))
				(if
					(or
						(setq tf (wcmatch sti (strcat kw1 "*")))
						(wcmatch sti (strcat kw2 "*"))
					)
					(progn
						(if
							(/= is ie)
							(setq
								sti (substr str is (- ie is))
								l (cons (cons 0 sti) l)
							)
						)
						(if
							tf
							(setq
								j (1+ j)
								l (cons (cons j kw1) l)
								ie (+ ie n1)
								is ie
							)
							(setq l (cons (cons (- j) kw2) l) ie (+ ie n2) is ie)
						)
					)
					(progn
						(setq ie (+ ie (if (> (ascii sti) 159) 2 1)))
						(if
							(and (> ie n) (/= is ie))
							(setq
								sti (substr str is (- ie is))
								l (cons (cons 0 sti) l)
							)
						)
					)
				)
			)
			(if
				(> j 0)
				(progn
					(if
						(> j 1)
						(progn
							(setq tf T)
							(while
								tf
								(initget (+ 2 4))
								(setq
									n (getint
										(strcat "\n程序发现有[" (itoa j) "]处" kw "，要恢复第几个<全部>: ")
									)
								)
								(cond
									(
										(null n)
										(setq n 1 tf nil)
										(while (<= n j) (setq l1 (cons n l1) n (1+ n)))
									)
									( (<= 1 n j) (setq l1 (list n) tf nil))
									('T (princ (strcat "\n**输入值应该在 1~" (itoa j) "之间!")))
								)
							)
						)
						(setq l1 (list j))
					)
					(foreach
						n
						l1
						(setq
							l (subst (cons n "") (assoc n l) l)
							n (- n)
							l (subst (cons n "") (assoc n l) l)
						)
					)
					(setq
						str (apply'strcat (reverse (mapcar'cdr l)))
						el (subst (cons 1 str) (assoc 1 el) el)
					)
					(entmod el)
					(entupd e)
				)
				(princ (strcat "\n**没有可恢复的" kw "!"))
			)
		)
	)
	(&TSTY)
)
 (setfunhelp "c:Shbhf" "Tssd" "shbhf")
 (defun c:Shbhf () (spc_txt "上标" "KZZF1" "KZZF2"))
 (setfunhelp "c:Xbhf" "Tssd" "xbhf")
 (defun c:Xbhf () (spc_txt "下标" "KZZF3" "KZZF4"))
 (setfunhelp "c:Zhywdzh" "Tssd" "Zhywdzh")
 (defun c:Zhywdzh
	(/
		d1
		d2
		dl
		dx1
		dx2
		e
		el
		kw
		l
		ops
		p1
		p10
		p11
		p2
		si
		sl
		ss
		st1
		st2
		st3
		stm
		str
		mdcl
	)
	(defun mdcl
		(str
			/
			dat_list
			dat_list_dx
			dia
			fi
			fil
			file
			le
			len
			strl
			zhw
			delb
			dele
			writefile
			addict
			chadict
			getzhw
		)
		(defun delb
			(str / ch i len no)
			(setq len (strlen str) i 1 no "")
			(while
				(<= i len)
				(setq ch (substr str i 1) i (1+ i))
				(if
					(/= ch " ")
					(setq str (substr str (1- i) len) i (1+ len))
					(setq no (strcat no ch))
				)
			)
			(list str no)
		)
		(defun dele
			(str / ch len no)
			(setq len (strlen str) no "")
			(while
				(> len 0)
				(setq ch (substr str len 1) len (1- len))
				(if
					(= ch " ")
					(setq str (substr str 1 len) no (strcat no ch))
					(setq len 0)
				)
			)
			(list str no)
		)
		(defun writefile
			(dat_l / l i fil wrl)
			(setq
				l (length dat_l)
				i 0
				fil (open (findfile "Zhywdzh.ini") "w")
			)
			(while
				(> l i)
				(setq
					wrl (strcat (nth i dat_l) "\t" (nth (1+ i) dat_l))
					i (+ 2 i)
				)
				(write-line wrl fil)
			)
			(close fil)
		)
		(defun addict
			(/ datl datt wrl yw zhw)
			(setq
				zhw (get_tile "zhw")
				zhw (car (delb zhw))
				zhw (car (dele zhw))
				yw (get_tile "yw")
				yw (car (delb yw))
				yw (car (dele yw))
			)
			(if
				(and (/= zhw "") (/= yw ""))
				(progn
					(if
						(and
							(> (ascii (substr zhw 1 1)) 159)
							(and
								(or
									(>= (ascii (substr yw 1 1)) 65)
									(<= (ascii (substr yw 1 1)) 90)
								)
								(or
									(>= (ascii (substr yw 1 1)) 97)
									(<= (ascii (substr yw 1 1)) 122)
								)
							)
						)
						(setq wrl (strcat yw "\t" zhw) datl (list yw zhw))
					)
					(if
						(and
							(> (ascii (substr yw 1 1)) 159)
							(and
								(or
									(>= (ascii (substr zhw 1 1)) 65)
									(<= (ascii (substr zhw 1 1)) 90)
								)
								(or
									(>= (ascii (substr zhw 1 1)) 97)
									(<= (ascii (substr zhw 1 1)) 122)
								)
							)
						)
						(setq wrl (strcat zhw "\t" yw) datl (list zhw yw))
					)
					(if
						wrl
						(if
							(not (member (car datl) dat_list))
							(progn
								(setq dat_list (append dat_list datl))
								(writefile dat_list)
								(set_tile "error" "***该条目成功存入词典***")
							)
							(progn
								(setq datt (cadr (member (car datl) dat_list)))
								(set_tile
									"error"
									(strcat "*该条目  " (car datl) "->" datt "  已经在词典存在*")
								)
								(mode_tile "cha" 0)
								(mode_tile "del" 0)
							)
						)
						(set_tile "error" "***该条目不能存入词典***")
					)
				)
				(set_tile "error" "***该条目不能存入词典***")
			)
			(list wrl datl)
		)
		(defun chadict
			(wr flag / dat_beg dat_end dat_l datl wrl)
			(setq
				wrl (car wr)
				datl (cadr wr)
				dat_end (cddr (member (car datl) dat_list))
				dat_beg (cdr (member (car datl) (reverse dat_list)))
			)
			(setq dat_l nil)
			(if
				(= flag 1)
				(setq dat_l (append (reverse dat_beg) datl dat_end))
				(setq dat_l (append (reverse dat_beg) dat_end))
			)
			(if (> (length dat_l) 0) (setq dat_list dat_l))
			(writefile dat_list)
		)
		(defun getzhw
			(/ zhw kg)
			(setq
				zhw (get_tile "zhw")
				zhw (car (delb zhw))
				zhw (car (dele zhw))
				kg (get_tile "kg")
			)
			(if (= kg "1") (setq zhw (strcat " " zhw)))
			zhw
		)
		(setq zhw str fil (open (findfile "Zhywdzh.ini") "r"))
		(while
			(setq fi (read-line fil))
			(setq
				file (&PLCN (strcase fi) "\t")
				dat_list_dx (append dat_list_dx file)
				file (&PLCN fi "\t")
				dat_list (append dat_list file)
			)
		)
		(close fil)
		(if
			(null (setq strl (member zhw dat_list)))
			(setq strl (member (strcase zhw) dat_list_dx))
		)
		(if
			strl
			(progn
				(setq
					len (length dat_list)
					le (length strl)
					len (- len le)
					strl nil
				)
				(while
					(> le 0)
					(setq
						strl (append strl (list (nth len dat_list)))
						le (1- le)
						len (1+ len)
					)
				)
				(if
					(cadr strl)
					(if
						(> (ascii (substr (cadr strl) 1 1)) 159)
						(setq zhw (cadr strl))
						(setq
							zhw (cadr (member (strcase zhw) (reverse dat_list)))
						)
					)
					(if
						(> (ascii (substr (car strl) 1 1)) 159)
						(setq
							zhw (cadr (member (strcase zhw) (reverse dat_list)))
						)
					)
				)
			)
		)
		(if
			(new_dialog "Zhywdzh" (setq dia (lib::dcl "Wztool")))
			(progn
				(if
					zhw
					(set_tile "zhw" zhw)
					(set_tile "error" "***目前词典中没有收录该条目***")
				)
				(set_tile "yw" str)
				(mode_tile "cha" 1)
				(mode_tile "del" 1)
				(if
					(> (ascii (substr zhw 1 1)) 159)
					(progn (set_tile "kg" "1") (mode_tile "kg" 0))
					(mode_tile "kg" 1)
				)
				(action_tile "dict" "(setq wr (addict))")
				(action_tile "cha" "(chadict wr 1)")
				(action_tile "del" "(chadict wr 0)")
				(action_tile
					"accept"
					"(setq zhw (getzhw)) (done_dialog 1)"
				)
				(action_tile "cancel" "(setq zhw nil) (done_dialog 0)")
				(action_tile "help" "(help \"Tssd\" \"Zhywdzh\")")
				(start_dialog)
				(unload_dialog dia)
			)
		)
		(if (and zhw (read zhw) (/= zhw str)) zhw)
	)
	(if
		(and
			(> (&UTXT) -1)
			(setq ops (getvar "PICKSTYLE"))
			(setvar "PICKSTYLE" 0)
			(setq p1 (&OSNP "\n框选要翻译文字的第一角点<退出>: "))
			(setq p2 (&OSNP p1 "\t另一角点<退出>: " T))
			(setq
				ss (ssget
					"CP"
					(list
						p1
						(list (car p1) (cadr p2) 0)
						p2
						(list (car p2) (cadr p1) 0)
					)
					'( (0 . "text"))
				)
			)
		)
		(progn
			(setq si 0 sl (sslength ss))
			(if
				(> sl 1)
				(while
					(and
						(setq e (ssname ss si))
						(@Hlt e 3)
						(progn
							(initget "P N")
							(setq
								kw (getkword "\n选中的文字不止一个，需要进一步确认 [下一个(N)/上一个(P)]<确认>: ")
							)
						)
					)
					(setq si (if (= kw "N") (1+ si) (1- si)))
					(if (< si 0) (setq si (1- sl)))
					(if (= si sl) (setq si 0))
					(@Hlt e 4)
				)
				(setq e (ssname ss 0))
			)
			(@Hlt e 4)
			(setq
				el (entget e)
				l (textbox el)
				dl (caadr l)
				p10 (&DRAG e 10)
				p11 (polar p10 (+ _pi2 (&DRAG 50)) 1e3)
				d1 (&MIDP p1 p10 p11)
				d2 (&MIDP p2 p10 p11)
				l (@txt2lst (&GENT (&DRAG 1)))
				dx2 dl
				kw nil
				st1 ""
				st2 ""
				st3 ""
				stm ""
			)
			(if (> d1 d2) (&PMLAY "d1" "d2"))
			(foreach
				str
				(reverse l)
				(setq
					stm (strcat str stm)
					el (subst (cons 1 stm) (assoc 1 el) el)
					dx1 (- dl (caadr (textbox el)))
				)
				(cond
					(
						(< d1 (* 0.5 (+ dx1 dx2)) d2)
						(setq st2 (@mrg2str str st2) kw'T)
					)
					(kw (setq st1 (@mrg2str str st1)))
					('T (setq st3 (@mrg2str str st3)))
				)
				(setq dx2 dx1)
			)
			(setq kw t)
			(while
				kw
				(setq sl (strlen st2))
				(cond
					(
						(wcmatch st2 "%%O*")
						(setq st2 (substr st2 4) st1 (@mrg2str st1 "%%O"))
					)
					(
						(wcmatch st2 "%%U*")
						(setq st2 (substr st2 4) st1 (@mrg2str st1 "%%U"))
					)
					(
						(wcmatch st2 " *")
						(setq st2 (substr st2 2) st1 (strcat st1 " "))
					)
					(
						(wcmatch st2 "*%%O")
						(setq
							st2 (substr st2 1 (- sl 3))
							st3 (@mrg2str "%%O" st3)
						)
					)
					(
						(wcmatch st2 "*%%U")
						(setq
							st2 (substr st2 1 (- sl 3))
							st3 (@mrg2str "%%U" st3)
						)
					)
					(
						(wcmatch st2 "* ")
						(setq st2 (substr st2 1 (- sl 1)) st3 (strcat " " st3))
					)
					(T (setq kw nil))
				)
			)
			(if
				(if
					(= st2 "")
					(prompt "\n**没有找到要翻译的字符!")
					(setq st2 (mdcl st2))
				)
				(progn
					(setq
						str (strcat st1 st2 st3)
						el (subst (cons 1 str) (assoc 1 el) el)
					)
					(entmod el)
					(entupd e)
				)
			)
		)
	)
	(if ops (setvar "PICKSTYLE" ops))
	(&TSTY)
)
 (defun add_txt
	(kw / e el i ss st str)
	(if
		(and
			(> (&UTXT) -1)
			(setq
				ss (&DSTR
					(strcat "\n选取需要添加" kw "缀的文字对象<退出>: ")
					'( (0 . "TEXT,MTEXT"))
				)
			)
			(setq i 0 str (getstring "\n添加字符串内容: "))
			(/= str "")
		)
		(while
			(setq e (ssname ss i))
			(setq
				i (1+ i)
				el (entget e)
				st (if
					(= kw "前")
					(strcat str (cdr (assoc 1 el)))
					(strcat (cdr (assoc 1 el)) str)
				)
				el (subst (cons 1 st) (assoc 1 el) el)
			)
			(entmod el)
			(entupd e)
		)
	)
	(&TSTY)
)
 (setfunhelp "c:Wzjqzh" "Tssd" "wzjqzh")
 (defun c:Wzjqzh () (add_txt "前"))
 (setfunhelp "c:Wzjhzh" "Tssd" "wzjhzh")
 (defun c:Wzjhzh () (add_txt "后"))
 (setfunhelp "c:Wzsf" "Tssd" "wzsf")
 (defun c:Wzsf
	(/ e el i nel ss zg zgxs zk zkxs)
	(if
		(and
			(> (&UTXT) -1)
			(setq
				ss (&DSTR "\n选择要缩放的文字<退出>: " '( (0 . "TEXT,MTEXT")))
			)
		)
		(progn
			(setq
				i 0
				zgxs (getreal "\n输入文字高度的放大系数<1.0>: ")
				zkxs (getreal "\n输入文字宽高比的放大系数<1.0>: ")
			)
			(if (not zgxs) (setq zgxs 1.0))
			(if (not zkxs) (setq zkxs 1.0))
			(if
				(or (/= zgxs 1.0) (/= zkxs 1.0))
				(while
					(setq e (ssname ss i))
					(setq
						el (entget e)
						zg (* zgxs (&DRAG e 40))
						zk (* zkxs (&DRAG 41))
						i (1+ i)
						nel (subst (cons 40 zg) (assoc 40 el) el)
						nel (subst (cons 41 zk) (assoc 41 nel) nel)
					)
					(entmod nel)
					(entupd e)
				)
			)
		)
	)
	(&TSTY)
)
 (setfunhelp "c:Jdsf" "Tssd" "jdsf")
 (defun c:Jdsf
	(/ e el i nel p1 p2 p3 ss zg zgxs zk zkxs)
	(if
		(and
			(> (&UTXT) -1)
			(setq p1 (&OSNP "\n点取表格的第一角点<退出>: "))
			(setq p2 (&OSNP p1 "\t另一角点<退出>: " T))
			(setq p3 (&OSNP p1 "\t另一角点的新位置 <退出>: " T))
			(setq ss (ssget "W" p1 p2 '( (0 . "TEXT,MTEXT"))))
		)
		(progn
			(setq
				zkxs (abs
					(/ (- (car p3) (car p1)) (- (car p2) (car p1)))
				)
				zgxs (abs
					(/ (- (cadr p3) (cadr p1)) (- (cadr p2) (cadr p1)))
				)
				i 0
			)
			(while
				(setq e (ssname ss i))
				(setq
					el (entget e)
					zg (* zgxs (&DRAG e 40))
					zk (/ (* zkxs (&DRAG 41)) zgxs)
					i (1+ i)
					nel (subst (cons 40 zg) (assoc 40 el) el)
					nel (subst (cons 41 zk) (assoc 41 nel) nel)
				)
				(entmod nel)
				(entupd e)
			)
		)
	)
	(&TSTY)
)
 (setfunhelp "c:Tyzg" "Tssd" "tyzg")
 (defun c:Tyzg
	(/ e el i nel nzg ss zg)
	(if
		(and
			(> (&UTXT) -1)
			(setq
				ss (&DSTR "\n选择要统一字高的文字<退出>: " '( (0 . "TEXT,MTEXT")))
			)
		)
		(progn
			(setq
				i 0
				nzg (/ (&INTS "标号文字") &sp)
				zg (getreal (strcat "\n输入文字的字高<" (&RTXT nzg) ">: "))
			)
			(if (not zg) (setq zg nzg))
			(while
				(setq e (ssname ss i))
				(setq
					el (entget e)
					nel (subst (cons 40 (* &sp zg)) (assoc 40 el) el)
					i (1+ i)
				)
				(entmod nel)
				(entupd e)
			)
		)
	)
	(&TSTY)
)
 (setfunhelp "c:Wzqx" "Tssd" "wzqx")
 (defun c:Wzqx
	(/ e el i nel qxxs ss)
	(if
		(and
			(> (&UTXT) -1)
			(setq
				ss (&DSTR "\n选择要倾斜的文字<退出>: " '( (0 . "TEXT,MTEXT")))
			)
		)
		(progn
			(setq i 0 qxxs (getreal "\n输入文字倾斜的角度<0>: "))
			(if (not qxxs) (setq qxxs 0))
			(setq qxxs (* (/ qxxs 180.0) pi))
			(while
				(setq e (ssname ss i))
				(setq
					el (entget e)
					i (1+ i)
					nel (subst (cons 51 (@wcs qxxs)) (assoc 51 el) el)
				)
				(entmod nel)
				(entupd e)
			)
		)
	)
	(&TSTY)
)
 (setfunhelp "c:xgwzys" "Tssd" "xgwzys")
 (defun c:xgwzys
	(/ ss en enl cl)
	(if
		(and
			(> (&UTXT) -1)
			(setq
				ss (&DSTR "\n选择要改变颜色的文字<退出>: " (list (cons 0 "TEXT")))
			)
			(setq cl (acad_colordlg 7))
		)
		(while
			(setq en (ssname ss 0))
			(ssdel en ss)
			(setq enl (entget en))
			(if
				(assoc 62 enl)
				(setq enl (subst (cons 62 cl) (assoc 62 enl) enl))
				(setq enl (append enl (list (cons 62 cl))))
			)
			(entmod enl)
		)
	)
	(&TSTY)
)
 
