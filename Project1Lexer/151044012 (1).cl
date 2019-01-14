;; G++ language tokenizer with DFA ugurkan ates Programming languages 341 course
;; if anything wrong call (lexer) 
;;it should ask for file name : "a.txt" on same file directory.

(defun DFAtokenize (string)
  (loop
      for start = 0 then (unless (null end) (+ end 1))
      for end = (unless (null start) (position #\space string :start start))
      while start collect (subseq string start end)))
(defun fromFile(fileName)
(setf *kek* (with-open-file (stream fileName :direction :input) ;;browntxt yerine file name koyyom
                 (loop
                     for line = (read-line stream nil)
                     while line
                     append (DFAtokenize line)))))
;; DFA tokenize
;;• START -> INPUT (filename)
;; • INPUT -> EXPI(expressionFinder DFA) | EXPLISTI(expressionfindr)
(defun all-tokens (string)
  (do (;; initial start value is 0
       (start 0)
       ;; initial token stack is nil
       (tokens))

      ;; loop until start is nil, then return the reverse of tokens
      ((not start) (nreverse tokens))

    ;; advance state
    (multiple-value-setq (string start tokens)
      (next-token string start tokens))))

(defun parenthesisp (c)
  (find c "()"))

(defun next-token (string start token-stack)
  (let ((search (position-if #'parenthesisp string :start start)))
    (typecase search
      (number
       ;; token from start to parenthesis
       (when (> search start)
         (push (subseq string start search) token-stack))
       ;; parenthesis
       (push (subseq string search (1+ search)) token-stack)
       ;; next state
       (values string (1+ search) token-stack))
      (null
       ;; token from start to end of string
       (when (< start (1- (length string)))
         (push (subseq string start) token-stack))
       ;; next-state
       (values string nil token-stack)))))

;; dfa state values
; EXPI -> (if EXPB EXPLISTI)
; EXPI -> (if EXPB EXPLISTI EXPLISTI)
; EXPI -> (while (EXPB) EXPLISTI)
; EXPI -> (for (Id EXPI EXPI) EXPLISTI)

(defun isKeywordBinary(gelenString )
	(cond 
	( (string-equal "and" gelenString)
		(setf *ortakliste*(append *ortakliste* (list "Keyword =" gelenString)))
		)
	( (string-equal "or" gelenString)
		(setf *ortakliste* (append *ortakliste* (list "Keyword =" gelenString)))
		)

	( (string-equal "not" gelenString)
		(setf *ortakliste*(append *ortakliste* (list "Keyword =" gelenString)))
		)

	( (string-equal "equal" gelenString)
		(setf *ortakliste*(append *ortakliste* (list "Keyword =" gelenString)))
		)

	( (string-equal "append" gelenString)
		(setf *ortakliste*(append *ortakliste* (list "Keyword =" gelenString)))
		)

	( (string-equal "concat" gelenString)
		(setf *ortakliste* (append *ortakliste* (list "Keyword =" gelenString)))
		)

	( (string-equal "set" gelenString)
		(setf *ortakliste* (append *ortakliste* (list "Keyword =" gelenString)))
		)

	( (string-equal "deffun" gelenString)
		(setf *ortakliste*(append *ortakliste* (list "Keyword =" gelenString)))
		)

	( (string-equal "for" gelenString)
		(setf *ortakliste* (append ortakListe (list "Keyword =" gelenString)))
		)

	( (string-equal "while" gelenString)
		(setf *ortakliste* (append *ortakliste* (list "Keyword =" gelenString)))
		)

	( (string-equal "if" gelenString)
		(setf *ortakliste* (append *ortakliste* (list "Keyword =" gelenString)))
		)

	( (string-equal "exit" gelenString)
		(setf *ortakliste* (append *ortakliste* (list "Keyword =" gelenString)))
		)

	( (string-equal "false" gelenString)
		(setf *ortakliste* (append ortakListe (list "BinaryValue =" gelenString)))
		)

	( (string-equal "true" gelenString)
			(setf *ortakliste* (append ortakListe (list "BinaryValue =" gelenString)))
			)

	)


	) ;; TESTED AND WORKING ! 


(defun isOperator(gelenString)
(cond 

( (string-equal T (and (string-equal "+" gelenString)
            (= (length gelenString) 1)))
		(setf *ortakliste* (append *ortakListe* (list "Operator =" gelenString)))

)

( (string-equal T (and (string-equal "-" gelenString)
            (= (length gelenString) 1)))
		(setf *ortakliste* (append *ortakliste* (list "Operator =" gelenString)))

)

( (string-equal T (and (string-equal "*" gelenString)
            (= (length gelenString) 1)))
		(setf *ortakliste* (append *ortakListe* (list "Operator =" gelenString)))

)

( (string-equal T (and (string-equal "/" gelenString)
            (= (length gelenString) 1)))
		(setf *ortakliste* (append *ortakListe* (list "Operator =" gelenString)))

)

( (string-equal T (and (string-equal ")" gelenString)
            (= (length gelenString) 1)))
		(setf *ortakliste* (append *ortakListe* (list "Operator =" gelenString)))
         )

( (string-equal T (and (string-equal "(" gelenString)
            (= (length gelenString) 1)))
		(setf *ortakliste* (append *ortakListe* (list "Operator =" gelenString)))
         )

( (string-equal T (and (string-equal "**" gelenString)
            (= (length gelenString) 2)))
		(setf *ortakliste* (append *ortakListe* (list "Operator =" gelenString)))

)
	)


) ;; tested and working


(defun isNumber (gelenString )
  (setq controlValue 0) ;; if -1 not numberanymore.
  (loop for i from 0 while (< i (length gelenString))
  	;; -0 olmasın onu kontrol et en son !!
	do
  	(cond 
  	( (string-equal T (or
            (string-equal "0" (char gelenString i))
            (string-equal "1" (char gelenString i))
            (string-equal "2" (char gelenString i))
            (string-equal "3" (char gelenString i))
            (string-equal "4" (char gelenString i))
            (string-equal "5" (char gelenString i))
            (string-equal "6" (char gelenString i))
			(string-equal "7" (char gelenString i))
			(string-equal "8" (char gelenString i))
			(string-equal "9" (char gelenString i))
			(string-equal  "-" (char gelenString 0))) ;; bunun -ise and length buyuk 1 olmasi lazim op ile karismasin

            )
  				(setf *ortakliste* (append *ortakListe* (list "Number =" gelenString))

  	)
  		 (setq controlValue 1)))
   ))
;; TESTED and working.

(defun lexer (fileName)
(fromFile fileName)
(setq *ortakListe* nil)
(setq gelenListe (mapcan #'all-tokens *kek*))
(loop for j from 0 while (< j (length gelenListe))

	do
	(setq degismedenOnce (length *ortakListe*)) ;;if value not changed length means identifer
	(isKeywordBinary (nth j gelenListe) )
	(isOperator (nth j gelenListe) )
	(isNumber (nth j gelenListe) )
	(if (= degismedenOnce (length *ortakListe*))
		(setf *ortakListe* (append *ortakListe* (list "Identifier = " (nth j gelenListe))))
		)
	)
 (list *ortakListe*)
)