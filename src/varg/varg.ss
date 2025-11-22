(module (varg varg) (varg)

(import scheme)
(import (chicken base))
(import (chicken format))
(import (chicken keyword))
(import (chicken string))
(import (chicken condition))

(define (varg . varg...)
(let-syntax
	(
		(vcondition (syntax-rules ()
			((vcondition message others ...)
				(condition (list 'varg (string->symbol "message") message) others ...))
			((vcondition message)
				(condition (list 'varg (string->symbol "message") message)))
		))
	)
	(define inerr "internal logic error, please contact maintainer")
	(define sserr
		"scheme syntax should not get this error, maybe a chicken scheme bug")
	(cond
		((not (list? varg...))
			(abort (vcondition
				(string-append
					"(define (.)) does not get list argument, "
					sserr)))))
	(define (:varg
			<>
			<with-value> <without-value>
			<literal> <explicit>
			enable-unknown verbose
		)
		(define (::varg
				::<>
				::>with-value< ::<without-value>
				::<literal>
			)
			(cond
				((null? ::<>)
					(cond ((> (length ::<literal>) (length <literal>))
						(cond
							((not enable-unknown)
								(abort (vcondition
									(sprintf "unknown arguments:\n~S"
									(list-tail (reverse ::<literal>) (length <literal>))))))
							(verbose
								(display verbose (current-error-port))
								(display
									(string-append
										"Note that #:enable-unknown was set, "
										"unknown arguments will not abort here"
									)
									(current-error-port)
								)
								(newline (current-error-port))
							)
						)))
					(cond ((< (length ::<literal>) (length <literal>))
						(abort (vcondition
							(sprintf "missing literal arguments:\n~S"
								(list-tail (reverse <literal>)
									(- (- (length <literal>) (length ::<literal>)) 1)))))))
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
								(abort (vcondition
									(sprintf "missing with-value arguments:\n~S"
										missing)))))
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
					varg...
					'(
						#:with-value
						#:without-value
						#:literal
						#:explicit
					) ;<with-value>
					'(
						#:enable-unknown
						#:verbose
					) ;<without-value>
					'("necessary 'arguments-to-parse' to (varg ...)") ;<literal>
					'() ;<explicit>
					#f ;enable-unknown
					#f ;verbose
				))
		)
		(let*
			(
				(args (assoc* '() #:literal meta-arg))
			)
			(cond
				((not (list? args)) (abort (vcondition inerr)))
				((null? args) (abort (vcondition inerr)))
				((not (list? (car args)))
					(abort (vcondition
						(sprintf
							"arguments to parse is not a list:\n~S" (car args)))))
			)
			(map
				(lambda (?)
					(let ((to-check (assoc* '() ? (assoc* '() #:with-value meta-arg))))
						(cond ((not (list? to-check))
							(abort (vcondition
								(sprintf
									"value of ~S is not a list:\n~S" ?  to-check)))))))
				'(#:with-value #:without-value #:literal #:explicit))
			(let
				(
					(args (car args))
					(with-value (assoc* '() #:with-value (assoc* '() #:with-value meta-arg)))
					(without-value (assoc* '() #:without-value (assoc* '() #:with-value meta-arg)))
					(literal (assoc* '() #:literal (assoc* '() #:with-value meta-arg)))
					(explicit (assoc* '() #:explicit (assoc* '() #:with-value meta-arg)))
					(enable-unknown (member #:enable-unknown (assoc* '() #:without-value meta-arg)))
					(verbose (member #:verbose (assoc* '() #:without-value meta-arg)))
				)
				(map (lambda (?) (cond ((not (keyword? ?))
					(abort (vcondition
						(sprintf
							"non keyword value in #:without-value:\n~S"
							? ))))))
					without-value)
				(:varg
					args
					with-value
					without-value
					literal
					explicit
					enable-unknown
					verbose
				)
			)
		)
	)
))

)
