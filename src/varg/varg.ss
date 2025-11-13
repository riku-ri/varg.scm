(module (varg varg) (varg)

(import scheme)
(import (chicken base))
(import (chicken format))
(import (chicken keyword))
(import (chicken string))
(import (chicken condition))

(define (varg . |>with-value,without-value,literal,explicit,enable-unknown<|)
(let-syntax
	(
		(abort (syntax-rules ()
			((abort condition ...) (let ()
				(print-call-chain (current-error-port))(newline (current-error-port))
				(abort condition ...)))))
	)
	(define inerr "internal logic error, please contact maintainer")
	(define sserr
		"scheme syntax should not get this error, maybe a chicken scheme bug")
	(let ((<> |>with-value,without-value,literal,explicit,enable-unknown<|))
		(cond
			((not (list? <>))
				(abort (condition
					`(varg
							message ,(string-append
								"(define (.)) does not get list argument, "
								sserr)))))))
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
			(cond
				((null? ::<>)
					(cond ((> (length ::<literal>) (length <literal>))
						(cond ((not enable-unknown)
							(abort (condition `(varg
								message ,(sprintf "unknown arguments:\n~S"
								(list-tail (reverse ::<literal>) (length <literal>)))))))
						)))
					(cond ((< (length ::<literal>) (length <literal>))
						(abort (condition `(varg
							message ,(sprintf "missing literal arguments:\n~S"
								(list-tail (reverse <literal>)
									(- (- (length <literal>) (length ::<literal>)) 1))))))))
					(let*
						(
							(missing
								(foldl
									(lambda (l r)
										(cond
											((member r (map car ::>with-value<)) l)
											(else (cons r l)
										)))
									'()
									<explicit>))
						)
						(cond
							((not (null? missing))
								(abort (condition `(varg
									message ,(sprintf "missing with-value arguments:\n~S"
										missing))))))
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
		(::varg <> '() '() '())
	)
	(let*
		(
			(assoc* (lambda (if-not-in key alist)
				(cond ((assoc key alist) (cdr (assoc key alist))) (else if-not-in))))
			(meta-arg (:varg
					|>with-value,without-value,literal,explicit,enable-unknown<|
					'(
						#:with-value
						#:without-value
						#:literal
						#:explicit
					) ;<with-value>
					'(
						#:enable-unknown
					) ;<without-value>
					'("necessary 'arguments-to-parse' to (varg ...)") ;<literal>
					'() ;<explicit>
					#f ;enable-unknown
				))
		)
		(let*
			(
				(args (assoc* '() #:literal meta-arg))
			)
			(cond
				((not (list? args)) (abort (condition `(varg message ,inerr))))
				((null? args) (abort (condition `(varg message ,inerr))))
				((not (list? (car args)))
					(abort (condition `(varg
						message ,(sprintf
							"arguments to parse is not a list:\n~A" (car args))))))
			)
			(map
				(lambda (?)
					(let ((to-check (assoc* '() ? (assoc* '() #:with-value meta-arg))))
						(cond ((not (list? to-check))
							(abort (condition `(varg
								message ,(sprintf
									"value of ~S is not a list:\n~S" ?  to-check))))))))
				'(#:with-value #:without-value #:literal #:explicit))
			(let
				(
					(args (car args))
					(with-value (assoc* '() #:with-value (assoc* '() #:with-value meta-arg)))
					(without-value (assoc* '() #:without-value (assoc* '() #:with-value meta-arg)))
					(literal (assoc* '() #:literal (assoc* '() #:with-value meta-arg)))
					(explicit (assoc* '() #:explicit (assoc* '() #:with-value meta-arg)))
					(enable-unknown (member #:enable-unknown (assoc* '() #:without-value meta-arg)))
				)
				(map (lambda (?) (cond ((not (keyword? ?))
					(abort (condition `(varg
						message ,(sprintf
							"non keyword value in #:without-value:\n~S"
							? )))))))
					without-value)
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
))

)
