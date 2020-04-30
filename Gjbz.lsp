(lib::lsp "Myfan")
 (setfunhelp "c:ddgjbzh" "Tswd" "ddgjbzh")
 (defun c:ddgjbzh
	(/ an ang d1 d2 en l p p0 p1 p2 p3 st str zg ss zj)
	(if (< (&UTXT 2) 0) (exit))
	(setq zg (&INTS "钢筋文字") p0 1 zj (* 0.5 zg))
	(while
		p0
		(setq p0 (&OSNP "\n点取钢筋标注引线的起始点<退出>: ") ss nil)
		(if
			p0
			(progn
				(setq p1 (&OSNP p0 "\t结束点<退出>: "))
				(if
					p1
					(progn
						(setq ang (angle p0 p1) ss (ssadd))
						(princ "\n输入钢筋文字<无>: ")
						(setq st (getstring))
						(cond
							(
								(and (>= ang (* 0.25 pi)) (< ang (* 0.75 pi)))
								(setq
									ang (* 0.5 pi)
									str "BR"
									an 90
									p1 (polar p0 ang (abs (- (nth 1 p0) (nth 1 p1))))
									p (polar p1 (* 1.25 pi) zj)
								)
							)
							(
								(and (>= ang (* 0.75 pi)) (< ang (* 1.25 pi)))
								(setq
									ang pi
									str "BL"
									an 0
									p1 (polar p0 ang (abs (- (nth 0 p0) (nth 0 p1))))
									p (polar p1 (* 0.25 pi) zj)
								)
							)
							(
								(and (>= ang (* 1.25 pi)) (< ang (* 1.75 pi)))
								(setq
									ang (* 1.5 pi)
									str "BL"
									an 90
									p1 (polar p0 ang (abs (- (nth 1 p0) (nth 1 p1))))
									p (polar p1 (* 0.75 pi) zj)
								)
							)
							(T
								(setq
									ang 0
									str "BR"
									an 0
									p1 (polar p0 ang (abs (- (nth 0 p0) (nth 0 p1))))
									p (polar p1 (* 0.75 pi) zj)
								)
							)
						)
						(&LJIG "粗线符号" T)
						(command
							".pline"
							p0
							"w"
							0
							(* 30 &sc)
							(polar p0 ang (* 200 &sc))
							""
						)
						(ssadd (entlast) ss)
						(&LJIG "细线符号" T)
						(command ".line" p0 p1 "")
						(ssadd (entlast) ss)
						(setq en (entlast) p2 p0 l 0)
						(command ".text" "J" str p zg an (@rtxt st))
						(ssadd (entlast) ss)
						(while
							p2
							(initget "U u")
							(if
								(= l 0)
								(setq p2 (&OSNP "\n点取添加箭头的位置<退出>: "))
								(setq p2 (&OSNP "\n点取添加箭头的位置 [回退(U)]<退出>: "))
							)
							(if
								(and p2 (/= p2 "U"))
								(progn
									(setq
										p3 (polar p2 (+ ang (* 0.5 pi)) 1000)
										p2 (inters p2 p3 p0 p1 nil)
										l (+ l 1)
									)
									(&LJIG "粗线符号" T)
									(command
										".undo"
										"Mark"
										".pline"
										p2
										"w"
										0
										(* 30 &sc)
										(polar p2 ang (* 200 &sc))
										""
									)
									(ssadd (entlast) ss)
									(if
										(not
											(equal
												(distance p0 p1)
												(+
													(setq d1 (distance p0 p2))
													(setq d2 (distance p1 p2))
												)
												&sp
											)
										)
										(progn
											(entdel en)
											(&LJIG "细线符号" T)
											(if
												(> d1 d2)
												(command ".line" p0 p2 "")
												(command ".line" p1 p2 "")
											)
											(ssadd (entlast) ss)
										)
									)
								)
								(if
									(= p2 "U")
									(progn
										(if
											(> l 0)
											(progn (command ".undo" "Back") (setq l (- l 1)))
											(princ "\n没有可回退的内容!")
										)
									)
								)
							)
						)
					)
				)
				(&DGAR ss)
				(setq ss nil)
			)
		)
	)
	(&TSTY)
)
 (defun findpt
	(s / pl l p e e42)
	(setq l (sslength s))
	(while
		(> l 0)
		(setq
			e (ssname s (- l 1))
			l (- l 1)
			e42 (&DRAG e 42)
			p (&DRAG e 10)
		)
		(if
			(and
				(or (equal e42 '(1 1)) (equal e42 '(-1 -1)))
				(equal (distance (car p) (cadr p)) (&DRAG e 43) 0.1)
			)
			(setq
				p (&N2S (car p) (cadr p))
				pl (append pl (list p))
			)
		)
	)
	pl
)
 (setfunhelp "c:ychgjbzh" "Tswd" "ychgjbzh")
 (defun c:ychgjbzh
	(/
		min_max
		ang
		d
		d1
		d2
		i
		l
		no1
		no2
		p
		p_l
		p0
		p1
		p2
		p3
		pend
		pl
		pm
		pmax
		pmin
		pn
		pp
		ss
		st
		str
		zg
		zj
		flag
		lay
	)
	(defun min_max
		(pl / a b i j lw ma1 mi1 no1 no2)
		(setq
			a (car (nth 0 pl))
			i 0
			lw (length pl)
			mi1 a
			ma1 a
			no1 0
			no2 0
		)
		(while
			(> lw 0)
			(setq b (car (nth i pl)))
			(if (> mi1 b) (setq mi1 b no1 i))
			(if (< ma1 b) (setq ma1 b no2 i))
			(setq i (+ i 1) lw (- lw 1))
		)
		(list no1 no2)
	)
	(if (< (&UTXT 2) 0) (exit))
	(setq zg (&INTS "钢筋文字") p0 1 zj (* 0.5 zg))
	(while
		p0
		(if
			(equal p0 (list 0 0) 0.1)
			(progn
				(setq p0 (&OSNP "\n点取钢筋标注引线的第一点<选点钢筋>: "))
				(if (not p0) (setq p0 "X"))
			)
			(progn
				(initget "X")
				(setq p0 (&OSNP "\n点取钢筋标注引线的第一点[选取点钢筋(X)]<退出>: "))
			)
		)
		(if
			(and p0 (listp p0))
			(setq p1 0 pl (append pl (list p0)))
		)
		(while
			p1
			(setq p1 (&OSNP "\t下一点<结束>: "))
			(if p1 (setq pl (append pl (list p1))))
		)
		(if
			(and (= p0 "X") (not pl))
			(progn
				(setq
					lay (strcase (&LJIG "点钢筋"))
					ss (&DSTR
						"\n选取点钢筋<退出>: "
						(list (cons 0 "LWPOLYLINE") (cons 8 lay))
					)
				)
				(if ss (setq pl (findpt ss)))
				(setq ss nil)
				(if pl (setq p0 (list 0 0)) (setq p0 nil))
			)
		)
		(if
			pl
			(progn
				(&LJIG "细线符号" T)
				(setq
					l (length pl)
					p_l (min_max pl)
					no1 (nth 0 p_l)
					no2 (nth 1 p_l)
					ss (ssadd)
				)
				(prompt "\n选取标注位置点: ")
				(while
					(not p2)
					(setq
						p (grread T 3)
						p3 (nth 1 p)
						i 0
						pp p0
						flag (nth 0 p)
					)
					(if (/= flag 5) (if (/= flag 3) (setq p3 pp)))
					(if
						(> (distance p3 pp) &sp)
						(progn
							(redraw)
							(setq
								pend (nth (- l 1) pl)
								p1 (polar p3 0 1000)
								ang (angle pend p3)
								pp p3
							)
							(while
								(< i l)
								(setq
									pm (nth i pl)
									pn (polar pm ang 1000)
									pn (inters p1 p3 pm pn nil)
									i (+ i 1)
								)
								(if (= no1 (- i 1)) (setq pmin pn))
								(if (= no2 (- i 1)) (setq pmax pn))
								(if
									(and
										(not (equal ang 0 0.01))
										(not (equal ang pi 0.01))
									)
									(grdraw pm pn 7)
								)
							)
						)
					)
					(if (= (nth 0 p) 3) (setq p2 p3))
				)
				(setq i 0 p3 nil p_l nil)
				(if
					(= flag 3)
					(progn
						(while
							(< i l)
							(setq
								pm (nth i pl)
								pn (polar pm ang 1000)
								pn (inters p1 p2 pm pn nil)
								i (+ i 1)
							)
							(command ".line" pm pn "")
							(ssadd (entlast) ss)
							(setq p_l (append p_l (list pn)))
						)
						(while
							(not p3)
							(setq
								p (grread T 1)
								p2 (nth 1 p)
								pp p0
								flag (nth 0 p)
								pl (min_max p_l)
								pmin (nth (nth 0 pl) p_l)
								pmax (nth (nth 1 pl) p_l)
							)
							(if (/= flag 5) (if (/= flag 3) (setq p2 pp)))
							(if
								(> (distance p2 pp) &sp)
								(progn
									(setq
										d (distance pmin pmax)
										d1 (distance p2 pmin)
										d2 (distance p2 pmax)
										p2 (list (nth 0 p2) (nth 1 pmin))
										pp p2
									)
									(redraw)
									(if
										(equal d (+ d1 d2) 10)
										(grdraw pmin pmax 7)
										(if (> d1 d2) (grdraw p2 pmin 7) (grdraw p2 pmax 7))
									)
								)
							)
							(if (= (nth 0 p) 3) (setq p3 p2))
						)
						(if
							(and (= flag 3) p3)
							(progn
								(setq
									d (distance pmin pmax)
									d1 (distance p3 pmin)
									d2 (distance p3 pmax)
								)
								(if
									(/= d (+ d1 d2))
									(if
										(> d1 d2)
										(progn
											(setq str "BR" p (polar p3 (* 0.75 pi) zj))
											(command ".line" p3 pmin "")
										)
										(progn
											(setq str "BL" p (polar p3 (* 0.25 pi) zj))
											(command ".line" p3 pmax "")
										)
									)
									(progn
										(setq str "BC" p (polar p3 (* 0.5 pi) zj))
										(command ".line" pmin pmax "")
									)
								)
								(if
									(equal d 0 1)
									(if
										(equal (angle pmin p3) 0 0.01)
										(setq str "BR" p (polar p3 (* 0.75 pi) zj))
										(setq str "BL" p (polar p3 (* 0.25 pi) zj))
									)
								)
								(ssadd (entlast) ss)
								(princ "\n输入钢筋文字<无>: ")
								(setq st (getstring))
								(&LJIG "文字" T)
								(command ".text" "J" str p zg 0 (@rtxt st))
								(ssadd (entlast) ss)
							)
						)
					)
				)
				(&DGAR ss)
				(setq ss nil pl nil p1 nil p2 nil p3 nil p_l nil)
			)
		)
	)
	(&TSTY)
)
 (defun min_max
	(pl / a b dis_x dis_y i lw ma1 mi1 no1 no2 nx1 nx2)
	(setq
		a (car (nth 0 pl))
		i 0
		lw (length pl)
		mi1 a
		ma1 a
		no1 0
		no2 0
		b 0
	)
	(while
		(> lw 0)
		(setq b (car (nth i pl)))
		(if (> mi1 b) (setq mi1 b no1 i))
		(if (< ma1 b) (setq ma1 b no2 i))
		(setq i (+ i 1) lw (- lw 1))
	)
	(setq
		dis_x (- (nth 0 (nth no2 pl)) (nth 0 (nth no1 pl)))
		nx1 no1
		nx2 no2
	)
	(setq
		a (cadr (nth 0 pl))
		i 0
		lw (length pl)
		mi1 a
		ma1 a
		no1 0
		no2 0
		b 0
	)
	(while
		(> lw 0)
		(setq b (cadr (nth i pl)))
		(if (> mi1 b) (setq mi1 b no1 i))
		(if (< ma1 b) (setq ma1 b no2 i))
		(setq i (+ i 1) lw (- lw 1))
	)
	(setq
		dis_y (- (nth 1 (nth no2 pl)) (nth 1 (nth no1 pl)))
	)
	(if
		(>= dis_x dis_y)
		(list nx1 nx2 "x")
		(list no1 no2 "y")
	)
)
 (setfunhelp "c:dchgjbzh" "Tswd" "dchgjbzh")
 (defun c:dchgjbzh
	(/
		an
		ang
		flag
		i
		l
		no
		no1
		no2
		p
		p_l
		p0
		p1
		p2
		p3
		pl
		pm
		pma
		pmax
		pmax_x
		pmax_y
		pmi
		pmid
		pmin
		pmin_x
		pmin_y
		pn
		pp
		s
		ss
		st
		str
		what
		zg
		lay
	)
	(if (< (&UTXT 2) 0) (exit))
	(setq zg (&INTS "钢筋文字") p0 1)
	(while
		p0
		(if
			(equal p0 (list 0 0) 0.1)
			(progn
				(setq p0 (&OSNP "\n点取钢筋标注引线的第一点<选点钢筋>: "))
				(if (not p0) (setq p0 "X"))
			)
			(progn
				(initget "X")
				(setq p0 (&OSNP "\n点取钢筋标注引线的第一点或 [选取点钢筋(X)]<退出>: "))
			)
		)
		(if
			(and p0 (listp p0))
			(setq p1 0 pl (append pl (list p0)))
		)
		(while
			p1
			(setq p1 (&OSNP "\t下一点<结束>: "))
			(if p1 (setq pl (append pl (list p1))))
		)
		(if
			(and (= p0 "X") (not pl))
			(progn
				(setq
					lay (strcase (&LJIG "点钢筋"))
					ss (&DSTR
						"\n选取点钢筋<退出>: "
						(list (cons 0 "LWPOLYLINE") (cons 8 lay))
					)
				)
				(if ss (setq pl (findpt ss)))
				(setq ss nil)
				(if pl (setq p0 (list 0 0)) (setq p0 nil))
			)
		)
		(if
			pl
			(progn
				(&LJIG "细线符号" T)
				(setq
					l (length pl)
					p_l (min_max pl)
					no1 (nth 0 p_l)
					no2 (nth 1 p_l)
					what (nth 2 p_l)
					ss (ssadd)
				)
				(prompt "\n选取标注位置点: ")
				(while
					(not p2)
					(setq
						p (grread T 3)
						p3 (nth 1 p)
						i 0
						pp p0
						flag (nth 0 p)
					)
					(if (/= flag 5) (if (/= flag 3) (setq p3 pp)))
					(if
						(> (distance p3 pp) &sp)
						(progn
							(redraw)
							(setq
								pmin (nth no1 pl)
								pmax (nth no2 pl)
								pmid (&N2S pmin pmax)
								pmin_x (nth 0 pmin)
								pmin_y (nth 1 pmin)
								pmax_x (nth 0 pmax)
								pmax_y (nth 1 pmax)
							)
							(if
								(= what "x")
								(progn
									(if
										(>= (nth 1 p3) (max pmin_y pmax_y))
										(progn
											(setq
												p1 (polar p3 0 1000)
												pmi (polar pmin (* 0.25 pi) 1000)
												pmi (inters p1 p3 pmin pmi nil)
												pma (polar pmax (* 0.75 pi) 1000)
												pma (inters p1 p3 pmax pma nil)
												no 1
											)
											(grdraw pmi pma 7)
											(while
												(< i l)
												(if
													(>= (nth 0 (setq pm (nth i pl))) (nth 0 pmid))
													(setq
														pn (polar pm (* 0.75 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
													(setq
														pn (polar pm (* 0.25 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
												)
												(grdraw pm pn 7)
												(setq i (+ i 1))
											)
										)
										(progn
											(setq
												p1 (polar p3 0 1000)
												pmi (polar pmin (* 1.75 pi) 1000)
												pmi (inters p1 p3 pmin pmi nil)
												pma (polar pmax (* 1.25 pi) 1000)
												pma (inters p1 p3 pmax pma nil)
												no 2
											)
											(grdraw pmi pma 7)
											(while
												(< i l)
												(if
													(>= (nth 0 (setq pm (nth i pl))) (nth 0 pmid))
													(setq
														pn (polar pm (* 1.25 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
													(setq
														pn (polar pm (* 1.75 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
												)
												(grdraw pm pn 7)
												(setq i (+ i 1))
											)
										)
									)
								)
								(progn
									(if
										(>= (nth 0 p3) (max pmin_x pmax_x))
										(progn
											(setq
												p1 (polar p3 (* 0.5 pi) 1000)
												pmi (polar pmin (* 0.25 pi) 1000)
												pmi (inters p1 p3 pmin pmi nil)
												pma (polar pmax (* 1.75 pi) 1000)
												pma (inters p1 p3 pmax pma nil)
												no 3
											)
											(grdraw pmi pma 7)
											(while
												(< i l)
												(if
													(>= (nth 1 (setq pm (nth i pl))) (nth 1 pmid))
													(setq
														pn (polar pm (* 1.75 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
													(setq
														pn (polar pm (* 0.25 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
												)
												(grdraw pm pn 7)
												(setq i (+ i 1))
											)
										)
										(progn
											(setq
												p1 (polar p3 (* 0.5 pi) 1000)
												pmi (polar pmin (* 0.75 pi) 1000)
												pmi (inters p1 p3 pmin pmi nil)
												pma (polar pmax (* 1.25 pi) 1000)
												pma (inters p1 p3 pmax pma nil)
												no 4
											)
											(grdraw pmi pma 7)
											(while
												(< i l)
												(if
													(>= (nth 1 (setq pm (nth i pl))) (nth 1 pmid))
													(setq
														pn (polar pm (* 1.25 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
													(setq
														pn (polar pm (* 0.75 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
												)
												(grdraw pm pn 7)
												(setq i (+ i 1))
											)
										)
									)
								)
							)
						)
					)
					(if (= (nth 0 p) 3) (setq p2 p3))
				)
				(setq i 0 p3 nil p_l nil)
				(if
					(= flag 3)
					(progn
						(cond
							(
								(= no 1)
								(while
									(< i l)
									(if
										(>= (nth 0 (setq pm (nth i pl))) (nth 0 pmid))
										(setq
											pn (polar pm (* 0.75 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
										(setq
											pn (polar pm (* 0.25 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
									)
									(command ".line" pm pn "")
									(ssadd (entlast) ss)
									(setq i (+ i 1) p_l (append p_l (list pn)))
								)
								(setq pl (min_max p_l) str "BC" an 0 ang (* 1.5 pi))
								(command
									".line"
									(setq pmi (nth (nth 0 pl) p_l))
									(setq pma (nth (nth 1 pl) p_l))
									""
								)
								(ssadd (entlast) ss)
							)
							(
								(= no 2)
								(while
									(< i l)
									(if
										(>= (nth 0 (setq pm (nth i pl))) (nth 0 pmid))
										(setq
											pn (polar pm (* 1.25 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
										(setq
											pn (polar pm (* 1.75 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
									)
									(command ".line" pm pn "")
									(ssadd (entlast) ss)
									(setq i (+ i 1) p_l (append p_l (list pn)))
								)
								(setq pl (min_max p_l) str "TC" an 0 ang (* 0.5 pi))
								(command
									".line"
									(setq pmi (nth (nth 0 pl) p_l))
									(setq pma (nth (nth 1 pl) p_l))
									""
								)
								(ssadd (entlast) ss)
							)
							(
								(= no 3)
								(while
									(< i l)
									(if
										(>= (nth 1 (setq pm (nth i pl))) (nth 1 pmid))
										(setq
											pn (polar pm (* 1.75 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
										(setq
											pn (polar pm (* 0.25 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
									)
									(command ".line" pm pn "")
									(ssadd (entlast) ss)
									(setq i (+ i 1) p_l (append p_l (list pn)))
								)
								(setq pl (min_max p_l) str "TC" an 90 ang pi)
								(command
									".line"
									(setq pmi (nth (nth 0 pl) p_l))
									(setq pma (nth (nth 1 pl) p_l))
									""
								)
								(ssadd (entlast) ss)
							)
							(
								(= no 4)
								(while
									(< i l)
									(if
										(>= (nth 1 (setq pm (nth i pl))) (nth 1 pmid))
										(setq
											pn (polar pm (* 1.25 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
										(setq
											pn (polar pm (* 0.75 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
									)
									(command ".line" pm pn "")
									(setq i (+ i 1) p_l (append p_l (list pn)))
								)
								(setq pl (min_max p_l) str "BC" an 90 ang 0)
								(command
									".line"
									(setq pmi (nth (nth 0 pl) p_l))
									(setq pma (nth (nth 1 pl) p_l))
									""
								)
								(ssadd (entlast) ss)
							)
						)
						(if
							(and (= flag 3) p2)
							(progn
								(princ "\n输入钢筋文字<无>: ")
								(setq st (getstring) s (ssadd))
								(&LJIG "文字" T)
								(command ".text" "J" str (list 0 0) zg an (@rtxt st))
								(ssadd (entlast) s)
								(ssadd (entlast) ss)
								(if
									(/= st "")
									(&END
										"\n点取引出文字标注的位置: "
										s
										(polar (list 0 0) ang (* 0.3 zg))
									)
								)
							)
						)
					)
				)
				(&DGAR ss)
				(setq s nil ss nil p1 nil p2 nil p3 nil pl nil p_l nil)
			)
		)
	)
	(&TSTY)
)
 (setfunhelp "c:dchychbzh" "Tswd" "dchychbzh")
 (defun c:dchychbzh
	(/
		an
		flag
		dis
		i
		l
		no
		no1
		no2
		p
		p_l
		p0
		p1
		p2
		p3
		pl
		pm
		pma
		pmax
		pmax_x
		pmax_y
		pmi
		pmid
		pmin
		pmin_x
		pmin_y
		pn
		pp
		s
		ss
		st
		str
		what
		zg
		zj
		lay
	)
	(if (< (&UTXT 2) 0) (exit))
	(setq zg (&INTS "钢筋文字") p0 1 zj (* 0.5 zg))
	(while
		p0
		(if
			(equal p0 (list 0 0) 0.1)
			(progn
				(setq p0 (&OSNP "\n点取钢筋标注引线的第一点<选点钢筋>: "))
				(if (not p0) (setq p0 "X"))
			)
			(progn
				(initget "X")
				(setq p0 (&OSNP "\n点取钢筋标注引线的第一点或 [选取点钢筋(X)]<退出>: "))
			)
		)
		(if
			(and p0 (listp p0))
			(setq p1 0 pl (append pl (list p0)))
		)
		(while
			p1
			(setq p1 (&OSNP "\t下一点<结束>: "))
			(if p1 (setq pl (append pl (list p1))))
		)
		(if
			(and (= p0 "X") (not pl))
			(progn
				(setq
					lay (strcase (&LJIG "点钢筋"))
					ss (&DSTR
						"\n选取点钢筋<退出>: "
						(list (cons 0 "LWPOLYLINE") (cons 8 lay))
					)
				)
				(if ss (setq pl (findpt ss)))
				(setq ss nil)
				(if pl (setq p0 (list 0 0)) (setq p0 nil))
			)
		)
		(if
			pl
			(progn
				(&LJIG "细线符号" T)
				(setq
					l (length pl)
					p_l (min_max pl)
					no1 (nth 0 p_l)
					no2 (nth 1 p_l)
					what (nth 2 p_l)
					ss (ssadd)
				)
				(prompt "\n选取标注位置点: ")
				(while
					(not p2)
					(setq
						p (grread T 3)
						p3 (nth 1 p)
						i 0
						pp p0
						flag (nth 0 p)
					)
					(if (/= flag 5) (if (/= flag 3) (setq p3 pp)))
					(if
						(> (distance p3 pp) &sp)
						(progn
							(redraw)
							(setq
								pmin (nth no1 pl)
								pmax (nth no2 pl)
								pmid (&N2S pmin pmax)
								pmin_x (nth 0 pmin)
								pmin_y (nth 1 pmin)
								pmax_x (nth 0 pmax)
								pmax_y (nth 1 pmax)
							)
							(if
								(= what "x")
								(progn
									(if
										(>= (nth 1 p3) (max pmin_y pmax_y))
										(progn
											(setq
												p1 (polar p3 0 1000)
												pmi (polar pmin (* 0.25 pi) 1000)
												pmi (inters p1 p3 pmin pmi nil)
												pma (polar pmax (* 0.75 pi) 1000)
												pma (inters p1 p3 pmax pma nil)
												no 1
											)
											(grdraw pmi pma 7)
											(while
												(< i l)
												(if
													(>= (nth 0 (setq pm (nth i pl))) (nth 0 pmid))
													(setq
														pn (polar pm (* 0.75 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
													(setq
														pn (polar pm (* 0.25 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
												)
												(grdraw pm pn 7)
												(setq i (+ i 1))
											)
										)
										(progn
											(setq
												p1 (polar p3 0 1000)
												pmi (polar pmin (* 1.75 pi) 1000)
												pmi (inters p1 p3 pmin pmi nil)
												pma (polar pmax (* 1.25 pi) 1000)
												pma (inters p1 p3 pmax pma nil)
												no 2
											)
											(grdraw pmi pma 7)
											(while
												(< i l)
												(if
													(>= (nth 0 (setq pm (nth i pl))) (nth 0 pmid))
													(setq
														pn (polar pm (* 1.25 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
													(setq
														pn (polar pm (* 1.75 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
												)
												(grdraw pm pn 7)
												(setq i (+ i 1))
											)
										)
									)
								)
								(progn
									(if
										(>= (nth 0 p3) (max pmin_x pmax_x))
										(progn
											(setq
												p1 (polar p3 (* 0.5 pi) 1000)
												pmi (polar pmin (* 0.25 pi) 1000)
												pmi (inters p1 p3 pmin pmi nil)
												pma (polar pmax (* 1.75 pi) 1000)
												pma (inters p1 p3 pmax pma nil)
												no 3
											)
											(grdraw pmi pma 7)
											(while
												(< i l)
												(if
													(>= (nth 1 (setq pm (nth i pl))) (nth 1 pmid))
													(setq
														pn (polar pm (* 1.75 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
													(setq
														pn (polar pm (* 0.25 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
												)
												(grdraw pm pn 7)
												(setq i (+ i 1))
											)
										)
										(progn
											(setq
												p1 (polar p3 (* 0.5 pi) 1000)
												pmi (polar pmin (* 0.75 pi) 1000)
												pmi (inters p1 p3 pmin pmi nil)
												pma (polar pmax (* 1.25 pi) 1000)
												pma (inters p1 p3 pmax pma nil)
												no 4
											)
											(grdraw pmi pma 7)
											(while
												(< i l)
												(if
													(>= (nth 1 (setq pm (nth i pl))) (nth 1 pmid))
													(setq
														pn (polar pm (* 1.25 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
													(setq
														pn (polar pm (* 0.75 pi) 1000)
														pn (inters p1 p3 pm pn nil)
													)
												)
												(grdraw pm pn 7)
												(setq i (+ i 1))
											)
										)
									)
								)
							)
						)
					)
					(if (= (nth 0 p) 3) (setq p2 p3))
				)
				(setq i 0 p3 nil p_l nil st nil)
				(if
					(= flag 3)
					(progn
						(princ "\n输入钢筋文字<无>: ")
						(setq st (getstring) st (@rtxt st))
						(cond
							(
								(= no 1)
								(while
									(< i l)
									(if
										(>= (nth 0 (setq pm (nth i pl))) (nth 0 pmid))
										(setq
											pn (polar pm (* 0.75 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
										(setq
											pn (polar pm (* 0.25 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
									)
									(command ".line" pm pn "")
									(ssadd (entlast) ss)
									(setq i (+ i 1) p_l (append p_l (list pn)))
								)
								(setq pl (min_max p_l) str "BC")
								(command
									".line"
									(setq pmi (nth (nth 0 pl) p_l))
									(setq pma (nth (nth 1 pl) p_l))
									""
								)
								(ssadd (entlast) ss)
								(setq pm (&N2S pmi pma))
								(if
									st
									(progn
										(&LJIG "文字" T)
										(command ".text" "J" "BR" (list 0 0) zg 90 st)
										(setq s nil s (ssadd))
										(ssadd (entlast) s)
										(ssadd (entlast) ss)
										(setq
											dis (distance
												(car (textbox (list (cons 1 st))))
												(cadr (textbox (list (cons 1 st))))
											)
										)
										(setq flag (&END "\n点取引出文字标注的位置<不标注>: " s (list 0 0)))
										(if
											flag
											(progn
												(setq
													p (cdr (assoc 11 (entget (entlast))))
													p (polar p (* 0.25 pi) zj)
													p1 (polar p (* 0.5 pi) 1000)
													p1 (inters p p1 pma pmi nil)
												)
												(&LJIG "细线符号" T)
												(if
													(equal (angle p1 p) (* 0.5 pi) 0.01)
													(command ".line" p p1 "")
													(command ".line" (polar p (* 1.5 pi) (* 1.1 dis)) p1 "")
												)
												(ssadd (entlast) ss)
												(if
													(> (nth 0 p1) (nth 0 pma))
													(command ".line" p1 pma "")
												)
												(if
													(< (nth 0 p1) (nth 0 pmi))
													(command ".line" p1 pmi "")
												)
												(ssadd (entlast) ss)
											)
										)
									)
								)
							)
							(
								(= no 2)
								(while
									(< i l)
									(if
										(>= (nth 0 (setq pm (nth i pl))) (nth 0 pmid))
										(setq
											pn (polar pm (* 1.25 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
										(setq
											pn (polar pm (* 1.75 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
									)
									(command ".line" pm pn "")
									(ssadd (entlast) ss)
									(setq i (+ i 1) p_l (append p_l (list pn)))
								)
								(setq pl (min_max p_l) str "TC" an 0)
								(command
									".line"
									(setq pmi (nth (nth 0 pl) p_l))
									(setq pma (nth (nth 1 pl) p_l))
									""
								)
								(ssadd (entlast) ss)
								(setq pm (&N2S pmi pma))
								(if
									st
									(progn
										(&LJIG "文字" T)
										(command ".text" "J" "BL" (list 0 0) zg 90 st)
										(setq s nil s (ssadd))
										(ssadd (entlast) s)
										(ssadd (entlast) ss)
										(setq
											dis (distance
												(car (textbox (list (cons 1 st))))
												(cadr (textbox (list (cons 1 st))))
											)
										)
										(setq flag (&END "\n点取引出文字标注的位置<不标注>: " s (list 0 0)))
										(if
											flag
											(progn
												(setq
													p (cdr (assoc 11 (entget (entlast))))
													p (polar p (* 1.75 pi) zj)
													p1 (polar p (* 0.5 pi) 1000)
													p1 (inters p p1 pma pmi nil)
												)
												(&LJIG "细线符号" T)
												(if
													(equal (angle p1 p) (* 1.5 pi) 0.01)
													(command ".line" p p1 "")
													(command ".line" (polar p (* 0.5 pi) (* 1.1 dis)) p1 "")
												)
												(ssadd (entlast) ss)
												(if
													(> (nth 0 p1) (nth 0 pma))
													(command ".line" p1 pma "")
												)
												(if
													(< (nth 0 p1) (nth 0 pmi))
													(command ".line" p1 pmi "")
												)
												(ssadd (entlast) ss)
											)
										)
									)
								)
							)
							(
								(= no 3)
								(while
									(< i l)
									(if
										(>= (nth 1 (setq pm (nth i pl))) (nth 1 pmid))
										(setq
											pn (polar pm (* 1.75 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
										(setq
											pn (polar pm (* 0.25 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
									)
									(command ".line" pm pn "")
									(ssadd (entlast) ss)
									(setq i (+ i 1) p_l (append p_l (list pn)))
								)
								(setq pl (min_max p_l) str "TC" an 90)
								(command
									".line"
									(setq pmi (nth (nth 0 pl) p_l))
									(setq pma (nth (nth 1 pl) p_l))
									""
								)
								(ssadd (entlast) ss)
								(setq pm (&N2S pmi pma))
								(if
									st
									(progn
										(&LJIG "文字" T)
										(command ".text" "J" "BR" (list 0 0) zg 0 st)
										(setq s nil s (ssadd))
										(ssadd (entlast) s)
										(ssadd (entlast) ss)
										(setq
											dis (distance
												(car (textbox (list (cons 1 st))))
												(cadr (textbox (list (cons 1 st))))
											)
										)
										(setq flag (&END "\n点取引出文字标注的位置<不标注>: " s (list 0 0)))
										(if
											flag
											(progn
												(setq
													p (cdr (assoc 11 (entget (entlast))))
													p (polar p (* 1.75 pi) zj)
													p1 (polar p 0 1000)
													p1 (inters p p1 pma pmi nil)
												)
												(&LJIG "细线符号" T)
												(if
													(equal (angle p1 p) 0 0.01)
													(command ".line" p p1 "")
													(command ".line" (polar p pi (* 1.1 dis)) p1 "")
												)
												(ssadd (entlast) ss)
												(if
													(> (nth 1 p1) (nth 1 pma))
													(command ".line" p1 pma "")
												)
												(if
													(< (nth 1 p1) (nth 1 pmi))
													(command ".line" p1 pmi "")
												)
												(ssadd (entlast) ss)
											)
										)
									)
								)
							)
							(
								(= no 4)
								(while
									(< i l)
									(if
										(>= (nth 1 (setq pm (nth i pl))) (nth 1 pmid))
										(setq
											pn (polar pm (* 1.25 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
										(setq
											pn (polar pm (* 0.75 pi) 1000)
											pn (inters p1 p2 pm pn nil)
										)
									)
									(command ".line" pm pn "")
									(setq i (+ i 1) p_l (append p_l (list pn)))
								)
								(setq pl (min_max p_l) str "BC" an 90)
								(command
									".line"
									(setq pmi (nth (nth 0 pl) p_l))
									(setq pma (nth (nth 1 pl) p_l))
									""
								)
								(ssadd (entlast) ss)
								(setq pm (&N2S pmi pma))
								(if
									st
									(progn
										(&LJIG "文字" T)
										(command ".text" "J" "BL" (list 0 0) zg 0 st)
										(setq s nil s (ssadd))
										(ssadd (entlast) s)
										(ssadd (entlast) ss)
										(setq
											dis (distance
												(car (textbox (list (cons 1 st))))
												(cadr (textbox (list (cons 1 st))))
											)
										)
										(setq flag (&END "\n点取引出文字标注的位置<不标注>: " s (list 0 0)))
										(if
											flag
											(progn
												(setq
													p (cdr (assoc 11 (entget (entlast))))
													p (polar p (* 1.25 pi) zj)
													p1 (polar p 0 1000)
													p1 (inters p p1 pma pmi nil)
												)
												(&LJIG "细线符号" T)
												(if
													(equal (angle p1 p) pi 0.01)
													(command ".line" p p1 "")
													(command ".line" (polar p 0 (* 1.1 dis)) p1 "")
												)
												(ssadd (entlast) ss)
												(if
													(> (nth 1 p1) (nth 1 pma))
													(command ".line" p1 pma "")
												)
												(if
													(< (nth 1 p1) (nth 1 pmi))
													(command ".line" p1 pmi "")
												)
												(ssadd (entlast) ss)
											)
										)
									)
								)
							)
						)
					)
				)
				(&DGAR ss)
				(setq s nil ss nil p1 nil p2 nil p3 nil pl nil p_l nil)
			)
		)
	)
	(&TSTY)
)
 (setfunhelp "c:lcgjbzh" "Tswd" "lcgjbzh")
 (defun c:lcgjbzh
	(/ a1 an ang p p0 p1 st zg s ss)
	(if (< (&UTXT 2) 0) (exit))
	(setq zg (&INTS "钢筋文字") p0 1)
	(while
		p0
		(setq p0 (&OSNP "\n点取钢筋标注引线的起始点<退出>: ") ss nil)
		(if
			(and p0 (not (= p0 "G")))
			(progn
				(setq p1 (&OSNP p0 "\t结束点<退出>: "))
				(if
					p1
					(progn
						(setq ang (angle p0 p1) ss (ssadd) s (ssadd))
						(princ "\n输入钢筋文字<无>: ")
						(setq st (getstring))
						(cond
							(
								(or
									(and (>= ang (* 0.25 pi)) (< ang (* 0.75 pi)))
									(and (>= ang (* 1.25 pi)) (< ang (* 1.75 pi)))
								)
								(setq
									ang (* 1.5 pi)
									an 90
									p (&N2S p0 p1)
									p0 (polar
										p
										(* 0.5 pi)
										(setq dis (* 0.5 (abs (- (nth 1 p0) (nth 1 p1)))))
									)
									p1 (polar p (* 1.5 pi) dis)
									a1 0
								)
							)
							(T
								(setq
									ang 0
									an 0
									p (&N2S p0 p1)
									p0 (polar
										p
										pi
										(setq dis (* 0.5 (abs (- (nth 0 p0) (nth 0 p1)))))
									)
									p1 (polar p 0 dis)
									a1 (* 1.5 pi)
								)
							)
						)
						(&LJIG "粗线符号" T)
						(command
							".pline"
							p0
							"w"
							0
							(* 30 &sc)
							(polar p0 ang (* 200 &sc))
							""
						)
						(ssadd (entlast) ss)
						(command
							".pline"
							p1
							"w"
							0
							(* 30 &sc)
							(polar p1 (+ ang pi) (* 200 &sc))
							""
						)
						(ssadd (entlast) ss)
						(&LJIG "细线符号" T)
						(command ".line" p0 p1 "")
						(ssadd (entlast) ss)
						(&LJIG "文字" T)
						(command ".text" "J" "BC" (list 0 0) zg an (@rtxt st))
						(ssadd (entlast) s)
						(ssadd (entlast) ss)
						(if
							(/= st "")
							(&END
								"\n点取引出文字标注的位置: "
								s
								(polar (list 0 0) a1 (* 0.3 zg))
							)
						)
						(&DGAR ss)
						(setq s nil ss nil)
					)
				)
			)
		)
	)
	(&TSTY)
)
 (setfunhelp "c:lcycbzh" "Tswd" "lcycbzh")
 (defun c:lcycbzh
	(/ an ang en1 en2 flag len p p0 p1 p2 pp s ss st str zg zj)
	(if (< (&UTXT 2) 0) (exit))
	(setq zg (&INTS "钢筋文字") p0 1 zj (* 0.3 zg))
	(while
		p0
		(setq p0 (&OSNP "\n点取钢筋标注引线的起始点<退出>: ") ss nil)
		(if
			(and p0 (not (= p0 "G")))
			(progn
				(setq p1 (&OSNP p0 "\t结束点<退出>: "))
				(if
					p1
					(progn
						(setq ang (angle p0 p1) ss (ssadd) s (ssadd))
						(princ "\n输入钢筋文字<无>: ")
						(setq st (getstring))
						(cond
							(
								(or
									(and (>= ang (* 0.25 pi)) (< ang (* 0.75 pi)))
									(and (>= ang (* 1.25 pi)) (< ang (* 1.75 pi)))
								)
								(setq
									ang (* 1.5 pi)
									p (&N2S p0 p1)
									p0 (polar
										p
										(* 0.5 pi)
										(setq dis (* 0.5 (abs (- (nth 1 p0) (nth 1 p1)))))
									)
									p1 (polar p (* 1.5 pi) dis)
								)
								(if
									(/= st "")
									(progn
										(&LJIG "文字" T)
										(command ".text" "J" "BC" (list 0 0) zg 0 (@rtxt st))
										(ssadd (entlast) s)
										(ssadd (entlast) ss)
										(setq flag (&END "\n点取引出文字标注的位置<不标注>: " s (list 0 0)))
										(if
											flag
											(progn
												(setq
													p2 (cdr (assoc 11 (entget (entlast))))
													p2 (polar p2 (* 1.5 pi) zj)
													p (inters p0 p1 p2 (polar p2 0 1000) nil)
													p2 (polar
														p2
														(angle p p2)
														(+
															&sp
															(distance
																(cdr (assoc 10 (entget (entlast))))
																(cdr (assoc 11 (entget (entlast))))
															)
														)
													)
												)
												(&LJIG "细线符号" T)
												(command ".line" p p2 "")
												(ssadd (entlast) ss)
												(if
													(> (nth 1 p2) (nth 1 p0))
													(command ".line" p0 p "")
												)
												(if
													(< (nth 1 p2) (nth 1 p1))
													(command ".line" p1 p "")
												)
												(ssadd (entlast) ss)
											)
										)
									)
								)
							)
							(T
								(setq
									ang 0
									an 90
									p (&N2S p0 p1)
									p0 (polar
										p
										pi
										(setq dis (* 0.5 (abs (- (nth 0 p0) (nth 0 p1)))))
									)
									p1 (polar p 0 dis)
								)
								(if
									(/= st "")
									(progn
										(&LJIG "文字" T)
										(command ".text" "J" "BC" (list 0 0) zg 90 (@rtxt st))
										(ssadd (entlast) s)
										(ssadd (entlast) ss)
										(setq flag (&END "\n点取引出文字标注的位置<不标注>: " s (list 0 0)))
										(if
											flag
											(progn
												(setq
													p2 (cdr (assoc 11 (entget (entlast))))
													p2 (polar p2 0 zj)
													p (inters p0 p1 p2 (polar p2 (* 0.5 pi) 1000) nil)
													p2 (polar
														p2
														(angle p p2)
														(+
															&sp
															(distance
																(cdr (assoc 10 (entget (entlast))))
																(cdr (assoc 11 (entget (entlast))))
															)
														)
													)
												)
												(&LJIG "细线符号" T)
												(command ".line" p p2 "")
												(ssadd (entlast) ss)
												(if
													(> (nth 0 p2) (nth 0 p1))
													(command ".line" p1 p "")
												)
												(if
													(< (nth 0 p2) (nth 0 p0))
													(command ".line" p0 p "")
												)
												(ssadd (entlast) ss)
											)
										)
									)
								)
							)
						)
						(&LJIG "粗线符号" T)
						(command
							".pline"
							p0
							"w"
							0
							(* 30 &sc)
							(polar p0 ang (* 200 &sc))
							""
						)
						(ssadd (entlast) ss)
						(command
							".pline"
							p1
							"w"
							0
							(* 30 &sc)
							(polar p1 (+ ang pi) (* 200 &sc))
							""
						)
						(ssadd (entlast) ss)
						(&LJIG "细线符号" T)
						(command ".line" p0 p1 "")
						(ssadd (entlast) ss)
						(&DGAR ss)
						(setq s nil ss nil)
					)
				)
			)
		)
	)
	(&TSTY)
)
 
