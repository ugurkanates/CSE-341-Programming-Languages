; 151044012 Ugurkan Ates
; Hocam sizin icin test inputları yazdım


;; TEST INPUTLARI ve sonuclari
; 1 - (deffun (x) y)
;Input = (test '(("operator" "(") ("keyword" "deffun") ("operator" "(") ("identifier" "x") ("operator" ")") ("identifier" "y") ("operator" ")")))
;Output = EXPI -> (deffun ID IDLIST EXPLISTI)

; 2 - (+ 1 2)
;Input =  (test '(("operator" "(") ("operator" "+" ) ("value" "1") ("value" "2") ("operator" ")"))
;Output = EXPI -> (+ EXPI EXPI) 

; 3-  (if (t) a b)
;Input = (test '(("operator" "(") ("keyword" "if") ("operator" "(") ("identifier" "t") ("operator" ")") ("identifier" "a") ("identifier" "b") ("operator" ")") ))
;Output = if EXPB EXPLISTI EXPLISTI



; ----------------------
; (parser liste) seklinde calistirin

; ornek  =  (parser '(("operator" "(") ("operator" "+" ) ("value" "1") ("value" "2") ("operator" ")"))

; proje2 - Common Lisp Parse Tree implementation in Common Lisp.

;START -> INPUT

;INPUT -> EXPI | EXPLISTI 

;EXPI -> (set ID EXPI) = +
;EXPI -> (+ EXPI EXPI) = +
;EXPI -> (- EXPI EXPI) = +
;EXPI -> (* EXPI EXPI) = +
;EXPI -> (/ EXPI EXPI) = +
;EXPI -> ID | (ID EXPLISTI) | VALUES 
;EXPI -> (deffun ID IDLIST EXPLISTI) = +
;EXPI -> (ID EXPLISTI) 
;EXPI -> (defvar ID EXPI) = +
;EXPI -> (if EXPB EXPLISTI)  = both
;EXPI -> (if EXPB EXPLISTI EXPLISTI)  both could be null explist so why twice?
;EXPI -> (while (EXPB) EXPLISTI) = +
;EXPI -> (for (ID EXPI EXPI) EXPLISTI) = +

;EXPB -> (and EXPB EXPB) = +
;EXPB -> (or EXPB EXPB) = +
;EXPB -> (not EXPB)  = +
;EXPB -> (equal EXPB EXPB) = +
;EXPB -> (equal EXPI EXPI) = +
;EXPB -> BinaryValue  eh

;EXPLISTI -> (concat EXPLISTI EXPLISTI) | (append EXPI EXPLISTI) | null | ‘( VALUES ) | ‘() 
;EXPLISTI -> EXPI 

;VALUES -> VALUES IntegerValue | IntegerValue 

;IDLIST -> ID | (IDLIST) | ID IDLIST 

;null için ("keyword" "null")
;'() (boş liste) için ("keyword" "'()")







(defun firstchecker (theWord str)


	(cond 
  	( (not(string-equal "(" theWord))
  		(setq *valuebir* 1)
  		(cond 
  			((string-equal theWord "identifier")
  				(format str "EXPI -> EXPB -> BinaryValue ")
  		)
  			;((string-equal theWord "BinaryValue")
  			(t
  				(format str "EXPI -> ID | (ID EXPLISTI) | VALUES  ")

  		)

  		 )  ;; burda VALUEyede bakalım
  		 )
	))
(defun secondChecker (theWords str)
	(setq theWordArray (nth 1 theWords))
	(setq theWord (nth 1 theWordArray))
	(cond 

	( (string-equal "set" theWord)
	    (format str "EXPI -> (set ID EXPI)")
		)
	( (string-equal "+" theWord)
	    (format str "EXPI -> (+ EXPI EXPI) ")
		)
	( (string-equal "-" theWord)
	    (format str "EXPI -> (- EXPI EXPI) ")
		)
	( (string-equal "*" theWord)
	    (format str "EXPI -> (* EXPI EXPI) ")
		)
	( (string-equal "/" theWord)
	    (format str "EXPI -> (/ EXPI EXPI) ")
		)
	( (string-equal "deffun" theWord)
	    (format str "EXPI -> (deffun ID IDLIST EXPLISTI) ")
		)
	( (string-equal "defvar" theWord)
	    (format str "EXPI -> (defvar ID EXPI)")
		)
	( (string-equal "while" theWord)
	    (format str "EXPI -> (while (EXPB) EXPLISTI)")
		)
	( (string-equal "for" theWord)
	    (format str "EXPI -> (for (ID EXPI EXPI) EXPLISTI) ")
		)
	( (string-equal "and" theWord)
	    (format str "EXPB -> (and EXPB EXPB) ")
		)
	( (string-equal "or" theWord)
	    (format str "EXPB -> (or EXPB EXPB)")
		)
	( (string-equal "not" theWord)
	    (format str "EXPB -> (not EXPB) ")
		)

	( (string-equal T (and (string-equal "if" theWord)
            (<= (length theWords) 5)))
		    (format str "EXPI -> (if EXPB EXPLISTI) ")

	)

	( (string-equal "if" theWord)
	    (format str "EXPI -> (if EXPB EXPLISTI EXPLISTI) ")
		)


	( (string-equal "identifier" theWord)
	    (format str "EXPI -> ID | (ID EXPLISTI) | VALUES")
		)
	( (string-equal "number" theWord)
	    (format str "EXPI -> ID | (ID EXPLISTI) | VALUES ")
		)
	( (string-equal "null" theWord)
	    (format str "NULL ")
		)
	( (string-equal "()" theWord)
	    (format str "NULL LIST ")
		)





  	( (string-equal "equal" theWord)
  		(cond 
  			((string-equal (nth 0 (nth 2 theWords) "BinaryValue"))
  		(format str "EXPB -> (equal EXPB EXPB) ")
  		)
  			(t
  		(format str "EXPB -> (equal EXPI EXPI) "))

  		)

  		 )



  		   ;; aynıları varsa misal EQB  ve EQI versiyonu var
  		 ) ; cond paranthesis closing




	) ; func closing pranhsis
(defun test(theList)
	(with-open-file (str "151044012.tree"
	                     :direction :output
	                     :if-exists :append
	                     :if-does-not-exist :create)
	  (format str "; DIRECTIVE: identify ~%")

	  (format str "; Incoming input : ~%")
	  (format str "~A~%" theList)
  	  (format str ";--------------------- ~%")
	  (format str "START ~%")
	  (format str "INPUT ~%")
	  (setq ortakListe (list))
	  (setq *valuebir* 0)
	  (firstchecker (nth 1(nth 0 theList)) str ) ; bu dorumu kontrol et

	  ;; buraya cond atıcaz
	 (cond 
  	( (= *valuebir* 0)
  		(secondChecker theList str ) ;; kontrol edelim

  		) ;; BURASI tum kontrollerin sonu value 0 oluncaki kontrller

  		   ;; burda VALUEyede bakalım
  	   (
  	   	(= *valuebir* 1)
  	   		;do nothing FIRST checker already control etti.
  	   		;(print "test purposes valuebir 1 ")
  		 )
  	   )
	  ;;if changed value should 1 
	  ;; if so exit
	  ;;if first gives away already.
	  (close str)
)


	)
(defun input_handle(user)
	(cond 
		((null user) nil)
		(t 
			(test (car user))
			(input_handle (user))
			)


		)
	)

(defun parser(user)
	(test user)
	;(test '(("operator" "(") ("keyword" "deffun") ("operator" "(") ("identifier" "x") ("operator" ")") ("identifier" "y") ("operator" ")")))
	;(test '(("operator" "(") ("operator" "+" ) ("value" "1") ("value" "2") ("operator" ")"))
	;(test '(("operator" "(") ("keyword" "if") ("operator" "(") ("identifier" "t") ("operator" ")") ("identifier" "a") ("identifier" "b") ("operator" ")") ))

	)		
;(parser )


;; TEST INPUTLARI ve sonuclari
; 1 - (deffun (x) y)
;Input = (test '(("operator" "(") ("keyword" "deffun") ("operator" "(") ("identifier" "x") ("operator" ")") ("identifier" "y") ("operator" ")")))
;Output = EXPI -> (deffun ID IDLIST EXPLISTI)

; 2 - (+ 1 2)
;Input =  (test '(("operator" "(") ("operator" "+" ) ("value" "1") ("value" "2") ("operator" ")"))
;Output = EXPI -> (+ EXPI EXPI) 

; 3-  (if (t) a b)
;Input = (test '(("operator" "(") ("keyword" "if") ("operator" "(") ("identifier" "t") ("operator" ")") ("identifier" "a") ("identifier" "b") ("operator" ")") ))
;Output = if EXPB EXPLISTI EXPLISTI