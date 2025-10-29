(import scheme)
(import (chicken base))
(import (chicken format))
(import (chicken keyword))
(import (chicken string))

(define (varg . |>with-value,without-value,literal,explicit,enable-unknown,before-abort<|)
	(define errtag "[varg]")
	(define inerr "internal logic error, please contact maintainer")
	(let ((<> |>with-value,without-value,literal,explicit,enable-unknown,before-abort<|))
		(cond ((not (list? <>)) (error errtag (sprintf "argument is not a list ~S" <>)))))
	(define before-abort
		(foldl
			(lambda (done todo)
				(cond
					((pair? todo)
						(cond
							((eq? (car todo) #:before-abort) (cdr todo))
							(else done)))
						(else done)))
			void
			|>with-value,without-value,literal,explicit,enable-unknown,before-abort<|))
	(cond ((not (procedure? before-abort)) (error "#:before-abort is not a procedure" before-abort)))

	(define (:varg
			<>
			<with-value> <without-value>
			<literal> <explicit>
			enable-unknown
		)
		(define (::varg
				::<>
				::>with-value< ::<without-value>
				::<literal>
			)
			(let
				(
					(error (lambda (^ . ...)
						(before-abort)
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
						;(error (map car ::>with-value<))
						(let*
							(
								(missing
									(foldl
										(lambda (done todo)
											(cond
												((member todo (map car ::>with-value<)) done)
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
					|>with-value,without-value,literal,explicit,enable-unknown,before-abort<|
					'(
						#:with-value
						#:without-value
						#:literal
						#:explicit
						#:before-abort
					) ;<with-value>
					'(
						#:enable-unknown
					) ;<without-value>
					'(args) ;<literal>
					'() ;<explicit>
					#f ;enable-unknown
				))
		)
		(let*
			(
				(error (lambda (^ . ...)
					(before-abort)
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
					(args (car args))
					(with-value (assoc* '() #:with-value (assoc* '() #:with-value meta-arg)))
					(without-value (assoc* '() #:without-value (assoc* '() #:with-value meta-arg)))
					(literal (assoc* '() #:literal (assoc* '() #:with-value meta-arg)))
					(explicit (assoc* '() #:explicit (assoc* '() #:with-value meta-arg)))
					(enable-unknown (member #:enable-unknown (assoc* '() #:without-value meta-arg)))
				)
				(:varg
					args
					with-value
					without-value
					literal
					explicit
					enable-unknown
				)
			)
		)
	)
)

;; test code
;(varg
;	'(#:with-value #:a #:b)
;	'(#:without-value #:c #:d)
;	#:enable-unknown
;	'(#:literal test)
;	'(#:explicit #:b)
;	;`(#:before-abort . ,(lambda () (error "error")))
;	'((#:b . 1) #:c "here" "there")
;)
