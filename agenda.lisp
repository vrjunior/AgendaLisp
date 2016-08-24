(defun insereFim (contato numero)
	(cond
		( (atom contato) (cons numero 'nil) ) 
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

(adicionanumero 'nil 'hu3br '123)
(adicionanumero (cons (cons 'gg (cons '44 'nil)) 'nil) 'hu3br '123)
