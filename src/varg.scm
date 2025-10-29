;(module (varg)
;	(
;		varg
;	)

(import scheme)
(import (chicken base))
(import (chicken keyword))
(import (chicken format))
(import (chicken string))

(define (varg . |>with-value,without-value,literal,explicit,enable-unknown,before-error<|)
	(define errtag "[varg]")
	(define inerr "internal logic error, please contact maintainer")
	(let ((<> |>with-value,without-value,literal,explicit,enable-unknown,before-error<|))
		(cond ((not (list? <>)) (error errtag (sprintf "argument is not a list ~S" <>)))))

	(define (:varg
			<>
			<with-value> <without-value>
			<literal> <explicit>
			enable-unknown
			before-error
		)
		(define (::varg
				::<>
				::>with-value< ::<without-value>
				::<literal>
			)
			(let
				(
					(error (lambda (^ . ...)
						(cond ((procedure? before-error) (before-error)))
						(apply error (cons errtag (cons ^ ...)))))
				)
				(cond
					((null? ::<>)
						(cond ((> (length ::<literal>) (length <literal>))
							(cond ((not enable-unknown)
								(error "unknown arguments"
									(list-tail (reverse ::<literal>) (length <literal>)))))))
						(cond ((< (length ::<literal>) (length <literal>))
							(error "missing literal arguments"
								(list-tail (reverse <literal>)
									(- (- (length <literal>) (length ::<literal>)) 1)))))
						(let*
							(
								(got-with-value (map car ::>with-value<))
								(missing (foldl
									(lambda (done todo)
										(cond
											((member todo got-with-value) done)
											(else (cons todo done)
										)))
									'()
									<explicit>))
							)
							(cond
								((not (null? missing))
									(error "missing with-value arguments" missing)))
						)
						(list
							(cons #:with-value ::>with-value<)
							(cons #:without-value ::<without-value>)
							(cons #:literal (reverse ::<literal>))
						)
					)
					((keyword? (car ::<>))
						(cond
							((member (car ::<>) <without-value>)
								(::varg (cdr ::<>)
									::>with-value< (cons (car ::<>) ::<without-value>)
									::<literal>
								))
							(else
								(::varg (cdr ::<>)
									::>with-value< ::<without-value>
									(cons (car ::<>) ::<literal>)
							))
						)
					)
					((pair? (car ::<>))
						(cond
							((member (car (car ::<>)) <with-value>)
								(::varg (cdr ::<>)
									(cons (car ::<>) ::>with-value<) ::<without-value>
									::<literal>
								))
							(else
								(::varg (cdr ::<>)
									::>with-value< ::<without-value>
									(cons (car ::<>) ::<literal>)
							))
						)
					)
					(else
						(::varg (cdr ::<>)
							::>with-value< ::<without-value>
							(cons (car ::<>) ::<literal>)
						)
					)
				)
			)
		)
		(::varg <> '() '() '())
	)
	(let*
		(
			(assoc* (lambda (if-not-in key alist)
				(cond ((assoc key alist) (cdr (assoc key alist))) (else if-not-in))))
			(meta-arg (:varg
					|>with-value,without-value,literal,explicit,enable-unknown,before-error<|
					'(
						#:with-value
						#:without-value
						#:literal
						#:explicit
						#:before-error
					) ;<with-value>
					'(
						#:enable-unknown
					) ;<without-value>
					'(args) ;<literal>
					'() ;<explicit>
					#f ;enable-unknown
					void ;before-error
				))
		)
		(let*
			(
				(before-error (assoc* void #:before-error (assoc* '() #:with-value meta-arg)))
				(error (lambda (^ . ...)
					(cond ((procedure? before-error) (before-error)))
					(apply error (cons errtag (cons ^ ...)))))
				(args (assoc* '() #:literal meta-arg))
			)
			(cond
				((not (list? args)) (error inerr))
				((null? args) (error "No arguments to parse"))
				((not (list? (car args)))
					(error "arguments to parse is not a list" (car args)))
			)
			(let
				(
				)
				meta-arg
			)
		)
	)
)

(varg
	'(#:with-value #:a #:b)
	'(#:without-value #:c #:d)
	'(#:explicit #:b)
	'((#:a . 1) #:c #:enable-unknown)
)

;)
