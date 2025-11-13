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

### <code>(varg <i><b>args</b></i>)</code>

#### Return

<code>(varg <i><b>args</b></i>)</code> will
return a list that contain:
- <code>(cons #:with-value <i><b>list-of-key-value-pair</b></i>)</code>
  - <code><i><b>list-of-key-value-pair</b></i></code> is a "association list"
    - *e.g.* `(cons #:with-value '(("key" . "value"))`
- <code>(cons #:without-value <i><b>list-of-keyword</b></i>)</code>
  - *e.g.* `(cons #:without-value '(#:a #:b))`
- <code>(cons #:literal <i><b>any-list</b></i>)</code>
  - <code><i><b>any-list</b></i></code> is a list

#### Exception

Exception of <code>(varg <i><b>args</b></i>)</code>
is supposed to be compliant with the module (chicken condition)
and SRFI-12
- http://wiki.call-cc.org/man/5/Module%20(chicken%20condition)

All except kind must be `(quote varg)`,
and contain at least a property `(quote message)`

Exception details is listed in the section #Arguments# below.

#### Arguments

- <code><i><b>args</b></i></code> in <code>(varg <i><b>args</b></i>)</code>  
  <code><i><b>args</b></i></code> should be a sequence of them:
  - <code>(cons #:with-value <i><b>list-of-keyword</b></i>)</code>
    > Arguments that are with value.
    > They may present in the necessary parameter
    > <code><i><b>a-procedure-without-argument</b></i></code>
    > as a pair.
    > If a with-value parameter is necessary for your self-defined function,
    > set the keyword in `#:explict`
    - abort:
      - if <code><i><b>list-of-keyword</b></i></code> is not a list
      - may abort by `#:explict`(see below)
    - *e.g.*
      - `(varg '(#:with-value #:a #:b) '())`
      - `(varg '(#:with-value #:a #:b) '(#:explict #:a) '((#:a . "value of a")))`
      - `(varg '(#:with-value #:a #:b) '(#:explict #:a) '())`
        > `varg` will abort a condition regarding `#:a` is missing because
        > `#:a` is in `#:explict` but
        > not in `'()`(the last parameter of `varg` here)
  - <code>(cons #:without-value <i><b>list-of-keyword</b></i>)</code>
    > Arguments that are without value.
    > They may present in the necessary parameter
    > <code><i><b>a-procedure-without-argument</b></i></code>
    > They are like options in command line, set or not.
    - abort:
      - if any element of <code><i><b>list-of-keyword</b></i></code>
        is not a keyword
      - if <code><i><b>list-of-keyword</b></i></code> is not a list
    - *e.g.*
      - `(varg '(#:without-value #:c #:d) '(#:c))`
  - <code>(cons #:literal <i><b>any-list</b></i>)</code>
    > Literal parameters.
    > They **must** present in the necessary parameter
    > <code><i><b>a-procedure-without-argument</b></i></code>.
    > > Details of <code><i><b>any-list</b></i></code>
    > > make no sense for `varg`,
    > > `varg` only need to know number of them.
    > > So `varg` does not check types of elements in
    > > <code><i><b>any-list</b></i></code>,
    > > but it is recommended make all elements to be scheme quoted symbol
    - abort:
      - if <code><i><b>list-of-keyword</b></i></code> is not a list
      - if the necessary parameter
        <code><i><b>a-procedure-without-argument</b></i></code>(see below)
        did not contain enough element that match to
        <code><i><b>any-list</b></i></code>
    - *e.g.*
      - `(varg '(#:literal 1st 2nd) '("1" "2"))`
        > This will return
        > `'((#:with-value) (#:without-value) (#:lteral "1" "2"))`
      - `(varg '(#:literal 1st 2nd) '("1"))`
        > `varg` will abort a condition regarding `2nd` is missing here
  - <code>(cons #:explict <i><b>list-of-keyword</b></i>)</code>
    > If a argument listed in `#:with-value` is necessary,
    > put the keyword in `#:explict` too.
    >
    > `#:explict` only check keywords in `#:with-value`.
    > Because `#:without-value` is like boolean value(set or not),
    > and `#:literal` is always necessary
    > (unless `#:enable-unknown`) is set.
    - abort:
      - if <code><i><b>list-of-keyword</b></i></code> is not a list
      - for each element ***k*** of <code><i><b>list-of-keyword</b></i></code>,
        if the necessary parameter
        <code><i><b>a-procedure-without-argument</b></i></code>(see below)
        did not contain a pair that `car` is ***k***
        > If a keyword presented in `#:explict` but not in `#:with-value`,
        > `varg` will **abort forever**.
    - *e.g.*
      - `(varg '(#:with-value #:a #:b) '(#:explict #:a) '((#:b . 1)))`
        > `varg` will abort a condition regarding missing `#:a`
  - `#:enable-unknown`
    > If `#:enable-unknown` is set,
    > `varg` will append unknown arguments to
    > `#:literal` in result but not report error.
    - *e.g.*
      - `(varg #:enable-unknown '(#:literal only-one) '(1 2 #:a-keyword))`
        > This will return
        > `'((#:with-value) (#:without-value) (#:literal 1 2 #:a-keyword))`.
        > If `#:enable-unknown` is not set,
        > `varg` will abort a condition regarding `2 #:a-keyword` is unknown.
  - [necessary]<code>(list <i><b>arguments-to-parse</b></i>)</code>
    - abort:
      - if this is missing
      - if this is not a list

  Each part listed above can be omitted except item marked by "[necessary]".
  And order is not sensitive.

## Examples

For example a procedure that copy file to another path named `cp`
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
  > And `cp` can implement the rela copy process based on it.
  > > `511` is the octal value of `#o777`
- `(cp #:force "/tmp/a" "/tmp/b")`  
  Which means copy `/tmp/a` to `/tmp/b`, and replace `/tmp/b` if exists
  > `varg` will return `'((#:with-value) (#:without-value #:force) (#:literal "/tmp/a" "/tmp/b"))`

## License

MIT

## Version History
