# varg.ss

```lisp
(import varg)
```

This is a chicken scheme egg,
section structure will be like:
- [http://wiki.call-cc.org/eggs%20tutorial#sections](http://wiki.call-cc.org/eggs%20tutorial#sections).

## Authors

- [riku-ri@outlook.com](riku-ri@outlook.com)

## Repository

[https://github.com/riku-ri/varg.ss/](https://github.com/riku-ri/varg.ss/)

## Requirements

## API

### Exception

Exceptions is supposed to be compliant with
the module `(chicken condition)` and SRFI-12
- http://wiki.call-cc.org/man/5/Module%20(chicken%20condition)

Find more details about exception in
the specific procedure below

#### non-continuable

The non-continuable conditions expand system conditions from:
- http://wiki.call-cc.org/man/5/Module%20(chicken%20condition)#system-conditions

More specifically, they would be:
- be with composite kind `(exn varg)`
  - for a condition from a specific procedure `<p>`,
    the composite kind would be `(exn varg <p>)`
- in the `exn` field, it will contain properties listed below:
  - `'message`
  - `'call-chain`

### `(varg . |args|)`

#### Return

`(varg |args|)` will return a list that contain:
- `(cons #:with-value |list-of-key-value-pair|)`
  - `|list-of-key-value-pair|` is a "association list"
    - *e.g.* `(cons #:with-value '(("key" . "value"))`
- `(cons #:without-value |list-of-keyword|)`
  - *e.g.* `(cons #:without-value '(#:a #:b))`
- `(cons #:literal |any-list|)`
  - `|any-list|` is a list

#### Parameters

`|args|` in `(varg |args|)` should be a sequence that
contain 1 or more element listed below,
order is not sensitive:
- `#:with-value`
  - *format*
    - `(cons #:with-value |list-of-keyword|)`
      - Where `|list-of-keyword|` should be a list of keyword
  - *description*
    - Arguments that are with value.
      They may present in the necessary parameter
      `|arguments-to-parse|`(see below)
      as a pair.
      If a with-value parameter is necessary for your self-defined function,
      set the keyword in `#:explict`
  - *if-necessary*:
    - No
  - *if-not-set*:
    - In the return value, `#:with-value` will follow a empty list
  - *e.g.*
    - `(varg '(#:with-value #:a #:b) '())`
    - `(varg '(#:with-value #:a #:b) '(#:explict #:a) '((#:a . "value of a")))`
    - `(varg '(#:with-value #:a #:b) '(#:explict #:a) '())`
      > `varg` will abort a condition regarding `#:a` is missing because
      > `#:a` is in `#:explict` but
      > not in `'()`(the last parameter of `varg` here)
- `#:without-value`
  - *format*
    - `(cons #:without-value |list-of-keyword|)`
      - Where `|list-of-keyword|` should be a list of keyword
  - *description*
    - Arguments that are without value.
      They may present in the necessary parameter
      `|arguments-to-parse|`(see below)
      They are like options in command line, set or not.
  - *if-necessary*:
    - No
  - *if-not-set*:
    - In the return value, `#:without-value` will follow a empty list
  - *e.g.*
    - `(varg '(#:without-value #:c #:d) '(#:c))`
- `#:literal`
  - *format*
    - `(cons #:literal |any-list|)`
      - Where `|any-list|` should be a list
  - *description*
    - Literal parameters.
      They **must** present in the necessary parameter
      `|arguments-to-parse|`(see below).
      > Details of `|any-list|`
      > make no sense for `varg`,
      > `varg` only need to know number of them.
      > So `varg` does not check types of elements in
      > `|any-list|`,
      > but it is recommended make all elements to be scheme quoted symbol
  - *if-necessary*:
    - No
  - *if-not-set*:
    - In the return value, `#:literal` will follow a empty list
  - *e.g.*
    - `(varg '(#:literal 1st 2nd) '("1" "2"))`
      > This will return
      > `'((#:with-value) (#:without-value) (#:lteral "1" "2"))`
    - `(varg '(#:literal 1st 2nd) '("1"))`
      > `varg` will abort a condition regarding `2nd` is missing here
- `#:explict`
  - *format*
    - `(cons #:explict |list-of-keyword|)`
      - Where `|list-of-keyword|` should be a list of keyword
  - *description*
    - If a argument listed in `#:with-value` is necessary,
      put the keyword in `#:explict` too.

      `#:explict` only check keywords in `#:with-value`.
      Because `#:without-value` is like boolean value(set or not),
      and `#:literal` is always necessary
      (unless `#:enable-unknown`) is set.
      > If a keyword presented in `#:explict` but not in `#:with-value`,
      > `varg` will **abort forever**.
  - *if-necessary*:
    - No
  - *if-not-set*:
    - All keyword after the parameter `#:with-value` can be omitted
  - *e.g.*
    - `(varg '(#:with-value #:a #:b) '(#:explict #:a) '((#:b . 1)))`
      > `varg` will abort a condition regarding missing `#:a`
- `#:enable-unknown`
  - *format*
    - Set or not
  - *description*
    - If `#:enable-unknown` is set,
      `varg` will append unknown arguments to
      `#:literal` in result but not report error.
  - *if-necessary*:
    - No
  - *if-not-set*:
    - Abort but not append to `#:literal`
  - *e.g.*
    - `(varg #:enable-unknown '(#:literal only-one) '(1 2 #:a-keyword))`
      > This will return
      > `'((#:with-value) (#:without-value) (#:literal 1 2 #:a-keyword))`.
      > If `#:enable-unknown` is not set,
      > `varg` will abort a condition regarding `2 #:a-keyword` is unknown.
- `#:verbose`
  - *format*
    - Set or not
  - *description*
    - If set, `varg` will output more information to `(current-error-port)`.
      Usually used in debug
  - *if-necessary*:
    - No
  - *if-not-set*:
    - Less infomation output to `(current-error-port)`
- arguments to parse
  - *format*
    - `|arguments-to-parse|`
      - should be a list
  - *description*
    - `varg` will iterate on it and group each element by settings in
      `#:with-value` `#:witout-value`, etc parameters
  - *e.g.*
    - `(varg)` will abort a condition regarding missing this argument
    - `(varg #())` will abort a condition regarding this is not a list
    > Content of `|arguments-to-parse|` may lead to abort
    > according to other arguments, see above

## Examples

Bascially *varg* filter the input list by conditions,
and group them to 3 classes `#:with-value` `withou-value` `#:literal`.
for example
```lisp
(import varg)
(define (fun . args)
	(set! varg-output (varg
		'(#:with-value #:wi1 #:wi2)
		'(#:without-value #:wo1 #:wo2)
		'(#:literal #:li1 #:li2)
		args))
	; After the call of `fun` at the bottom,
	; varg-output should be a list:
	'(
		(#:with-value (#:wi1 . 1))
		; #:wi2 does not appear
		; because the call of `fun` at the bottom did not set it
		(#:without-value #:wo2)
		(#:literal "non-keyword1" "non-keyword2")
	)

	; Hence get values like this:
	(let
		(
			(with-value (cdr (assoc #:with-value varg-output)))
			(without-value (cdr (assoc #:without-value varg-output)))
			(literal (cdr (assoc #:literal varg-output)))
		)
		(print (cdr (assoc #:wi1 with-value))) ; this will be 1
		(print (member #:wo2 without-value)) ; this will be equal to #t
		(print (member #:wo1 without-value)) ; this will be #f
		(print (list-ref literal 0)) ; this will "non-keyword1"
		(print (list-ref literal 1)) ; this will "non-keyword2"
	)
)
(fun
	'(#:wi1 . 1)
	#:wo2
	"non-keyword1" "non-keyword2"
)
```

For another example, a procedure that copy file to another path named `cp`
> Just showing how to use `varg`, no copy implementation in `cp`

```lisp
(define (cp . cp-args)
	(varg
		'(#:with-value #:mode)
		'(#:without-value #:force)
		'(#:literal src dest)
		cp-args
	)
... ...
)
```

`cp` would be called like:
- `(cp (cons #:mode #o777) "/tmp/a" "/tmp/b")`  
  Which means copy `/tmp/a` to `/tmp/b`, and set `/tmp/b` mode to `0777`
  > In the definition of `cp`, `varg` will return
  > `'((#:with-value (#:mode . 511)) (#:without-value) (#:literal "/tmp/a" "/tmp/b"))`
  > And `cp` can implement the real copy process based on it.
  > > `511` is the octal value of `#o777`
- `(cp #:force "/tmp/a" "/tmp/b")`  
  Which means copy `/tmp/a` to `/tmp/b`, and replace `/tmp/b` if exists
  > `varg` will return `'((#:with-value) (#:without-value #:force) (#:literal "/tmp/a" "/tmp/b"))`

## License

MIT
