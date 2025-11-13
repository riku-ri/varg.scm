(module varg ()

(import scheme (chicken module))

(define-syntax importrec (syntax-rules ()
	((importrec lib ...) (begin
		(import lib ...)(reexport lib ...)))))

(importrec (varg varg))

)
