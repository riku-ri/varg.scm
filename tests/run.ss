(import varg)
(import test)

(define empty-result '((#:with-value) (#:without-value) (#:literal)))
(define (assoc* if-not-in key alist)
	(cond ((assoc key alist) (cdr (assoc key alist))) (else if-not-in)))

(test empty-result (varg '(#:with-value) '()))
(test-error (varg '(#:with-value) '() "error-here"))

(test-group "#:with-value"
	(test-error (varg (cons #:with-value 'error-here) '()))
	(test-error (varg '(#:with-value . 1) '()))
	(test empty-result (varg '(#:with-value . 'quote) '())) ; Note that (quote #:with-value . 'quote) will not treat . as cons symbol
	(test
		`((#:with-value ,(cons #:a 1)) (#:without-value) (#:literal))
		(varg '(#:with-value #:a) '((#:a . 1))))
	(test
		`((#:with-value ,(cons #:a 1)) (#:without-value) (#:literal))
		(varg '(#:with-value #:a) '((#:a . 1))))
	(test
		(list (cons #:a 1) (cons #:b 2))
		(assoc* #f #:with-value (varg '(#:with-value #:a #:b) '((#:b . 2) (#:a . 1)))))
	(test
		1
		(assoc* #f #:a (assoc* #f #:with-value (varg '(#:with-value #:a #:b) '((#:b . 2) (#:a . 1))))))
)

(test-group "#:without-value"
	(test-error (varg (cons #:without-value 'error-here) '()))
	(test-error (varg '(#:without-value . 1) '()))
	(test-error (varg '(#:without-value . 'quote) '())) ; #:wthout-value also check if . and 'quote is keyword
	(test
		empty-result
		(varg '(#:without-value #:a) '()))
	(test
		'((#:with-value) (#:without-value #:a) (#:literal))
		(varg '(#:without-value #:a) '(#:a)))
)

(test-group "#:literal"
	(test-error (varg (cons #:literal 'error-here) '()))
	(test-error (varg '(#:literal . 1) '()))
	(test-error (varg '(#:literal 1) '()))
	(test-error (varg '(#:literal 1 more) '("here")))
	(test
		"here"
		(list-ref (assoc* #f #:literal (varg '(#:literal 1 more) (list "here" 'another))) 0))
	(test
		(quote another)
		(list-ref (assoc* #f #:literal (varg '(#:literal 1 more) (list "here" 'another))) 1))
)

(test-group "#:explicit"
	(test-error (varg (cons #:explicit 'error-here) '()))
	(test-error (varg '(#:explicit "to-set") '()))
	(test-error (varg '(#:explicit "to-set") '(("to-set" . 0))))
	(test
		(quote should-be-this)
		(assoc* #f "to-set"
			(assoc* #f #:with-value
				(varg '(#:with-value "to-set") '(#:explicit "to-set") '(("to-set" . should-be-this))))))
)

(test-group "#:before-abort"
	(test-error (varg '(#:before-abort 1) '()))
	(test-error (varg '(#:before-abort (lambda () ())) '()))
	(test empty-result (varg `(#:before-abort . ,(lambda () '())) '()))
	(test-error (varg '(#:before-abort . void) '()))
	(test empty-result (varg `(#:before-abort . ,void) '()))
)

(test-group "#:enable-unknown"
	(test-error (varg '("error-here")))
	(test
		(quote this-unknown-is-enabled)
		(car (assoc* #f #:literal (varg #:enable-unknown '(this-unknown-is-enabled)))))
	(test
		(quote this-unknown-is-enabled)
		(list-ref
			(assoc* #f #:literal
				(varg '(#:literal this-is-in-literal) #:enable-unknown
					'(this-is-in-literal this-unknown-is-enabled)))
			1))
)
