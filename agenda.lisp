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
			(equal (caar agenda) nome) 
			(cons 
				(cons nome (insereFim (cdar agenda) numero)) 
				(cdr agenda) 
			)
		)
		( 't (cons (car agenda) (adicionaNumero (cdr agenda) nome numero)) )
	)
)

(defun incluir(agenda contato)
	(adicionaNumero agenda (car contato) (cadr contato))
)

(defun removeContato (agenda nome)
	(cond	((atom agenda) 'NIL)
			((equal (caar agenda) nome) (cdr agenda))
			('t (cons (car agenda) (removeContato (cdr agenda) nome)))
	)
)

(defun Telefones (agenda nome)
	(cond	((atom agenda) 'INEXISTENTE)
			((equal (caar agenda) nome) (cdar agenda))
			('t (Telefones (cdr agenda) nome))
	)
)

(defun checaContatosSemNumero( agenda )
	(cond 	( (atom agenda) 'NIL )
			( (equal (cdar agenda) 'nil) (cdr agenda) )
			('t (cons (car agenda) (checaContatosSemNumero (cdr agenda))) )
	)
)

(defun removeElementoDaLista ( lista elemento )
	(cond
		( (atom lista) 'nil )
		( (equal (car lista) elemento) (cdr lista) ) 
		('t (cons (car lista) (removeElementoDaLista (cdr lista) elemento)) )
	)
) 

(defun removeNumeroDeContato (contato numero)
	(cons (car contato) (removeElementoDaLista (cdr contato) numero))
)

(defun removeNumero (agenda nome numero) 
	(cond 
		( (atom agenda) 'NIL )
		( 
			(equal (caar agenda) nome) 
			(checaContatosSemNumero (cons 
				(removeNumeroDeContato (car agenda) numero) 
				(cdr agenda)
			)) 
		)
		(  't (cons (car agenda) (removeNumero (cdr agenda) nome numero))  )
	)
)

(defun excluir (agenda contato)
	(removeNumero agenda (car contato) (cadr contato))
)

(setq agendao (adicionanumero 'nil 'hu3br '123))
(setq agendao (adicionanumero agendao 'guilhermezera '666))
(setq agendao (adicionanumero agendao 'vjunior '777))
(setq agendao (removeContato agendao 'guilhermezera))
(setq agendao (adicionanumero agendao 'vjunior '8321))
(setq agendao (removeNumero agendao 'hu3br '123))
(setq agendao (removeNumero agendao 'vjunior 'vjunior))

(print agendao)
