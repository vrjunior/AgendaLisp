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

(setq agenda 'nil)
(setq agenda (adicionanumero agenda 'hu3br '123))
(setq agenda (adicionanumero agenda 'guilhermezera '666))
(setq agenda (adicionanumero agenda 'vjunior '777))
(setq agenda (removeContato agenda 'guilhermezera))
(print (buscaContato agenda 'vjunior))
