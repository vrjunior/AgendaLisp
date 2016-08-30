(defun insereFim (contato numero)
	(cond
		( (atom contato) (cons numero 'nil) ) 
		( (equal (car contato) numero) contato)
		( 't (cons (car contato) (insereFim (cdr contato) numero)) )
	)
)

(defun adicionaNumero (agenda nome numero)
	(cond
		( (atom agenda) (cons (cons nome (insereFim 'nil numero)) 'nil) )
		( 
			(equal (car (car agenda)) nome) 
			(cons 
				(cons nome (insereFim (cdr (car agenda)) numero)) 
				(cdr agenda) 
			)
		)
		( 't (cons (car agenda) (adicionaNumero (cdr agenda) nome numero)) )
	)
)

(defun removeContato (agenda nome)
	(cond	((atom agenda) 'NIL)
			((equal (caar agenda) nome) (cdr agenda))
			('t (cons (car agenda) (removeContato (cdr agenda) nome)))
	)
)

(defun buscaContato (agenda nome)
	(cond	((atom agenda) 'NIL)
			((equal (caar agenda) nome) (car agenda))
			('t (buscaContato (cdr agenda) nome))
	)
)

(defun checaContatosSemNumero( agenda )
	(cond 	( (atom agenda) 'NIL )
			( (equal (cdr(car agenda)) 'nil) (cdr agenda) )
			('t (cons (car agenda) (checaContatosSemNumero (cdr agenda))) )
	)
)

(defun removeInformacaoContato ( contato info )
	(cond
		( (atom contato) 'nil )
		( (equal (car contato) info) (cdr contato) ) 
		('t (cons (car contato) (removeInformacaoContato (cdr contato) info)) )
	)
) 

(defun removeNumero (agenda nome numero) 
	(cond 
		( (atom agenda) 'NIL )
		( (equal (caar agenda) nome) (checaContatosSemNumero (cons (removeInformacaoContato (car agenda) numero) (cdr agenda))) )
		(  't (checaContatosSemNumero ( cons (car agenda) (removeNumero (cdr agenda) nome numero)) )  )
	)
)

(setq agend (adicionanumero 'nil 'hu3br '123))
(setq agend (adicionanumero agend 'guilhermezera '666))
(setq agend (adicionanumero agend 'vjunior '777))
(setq agend (removeContato agend 'guilhermezera))
(setq agend (adicionanumero agend 'vjunior '8321))
(setq agend (removeNumero agend 'hu3br '123))

(print agend)
