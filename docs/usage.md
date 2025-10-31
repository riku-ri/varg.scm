# varg.scm

```lisp
(import varg)
```

This is a chicken scheme egg,
section structure will be like:
- [http://wiki.call-cc.org/eggs%20tutorial#sections](http://wiki.call-cc.org/eggs%20tutorial#sections).

## Authors

- [riku-ri@outlook.com](riku-ri@outlook.com)

## Repository

[https://github.com/riku-ri/varg.scm/](https://github.com/riku-ri/varg.scm/)

## Requirements

## API

<code>(varg <i><b>args</b></i>)</code>

- input
  - <code><i><b>args</b></i></code>  
    <code><i><b>args</b></i></code> should be a sequence of them:
    - <code>(cons #:with-value <i><b>list-of-keyword</b></i>)</code>
      > Arguments that are with value.
      > They may present in the necessary parameter
      > <code><i><b>a-procedure-without-argument</b></i></code>
      > as a pair.
      > If a with-value parameter is necessary for your self-defined function,
      > set the keyword in `#:explict`
      - *e.g.*
        - `(varg '(#:with-value #:a #:b) '())`
        - `(varg '(#:with-value #:a #:b) '(#:explict #:a) '((#:a . "value of a")))`
        - `(varg '(#:with-value #:a #:b) '(#:explict #:a) '())`
          > `varg` will report an error that `#:a` is missing because
          > `#:a` is in `#:explict` but
          > not in `'()`(the last parameter of `varg` here)
    - <code>(cons #:without-value <i><b>list-of-keyword</b></i>)</code>
      > Arguments that are without value.
      > They may present in the necessary parameter
      > <code><i><b>a-procedure-without-argument</b></i></code>
      > They are like options in command line, set or not.
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
      - *e.g.*
        - `(varg '(#:literal 1st 2nd) '("1" "2"))`
          > This will return
          > `'((#:with-value) (#:without-value) (#:lteral "1" "2"))`
        - `(varg '(#:literal 1st 2nd) '("1"))`
          > `varg` will report `2nd` is missing here
    - <code>(cons #:explict <i><b>list-of-keyword</b></i>)</code>
      > If a argument listed in `#:with-value` is necessary,
      > put the keyword in `#:explict` too.
      >
      > `#:explict` only check keywords in `#:with-value`.
      > Because `#:without-value` is like boolean value(set or not),
      > and `#:literal` is always necessary
      > (unless `#:enable-unknown`(see below) is set.
      >
      > If a keyword presented in `#:explict` but not in `#:with-value`,
      > `varg` will report error forever.
      - *e.g.*
        - `(varg '(#:with-value #:a #:b) '(#:explict #:a) '((#:b . 1)))`
          > `varg` will report the missing `#:a` error
    - <code>(cons #:before-abort <i><b>a-procedure-without-argument</b></i>)</code>
      > `varg` will abort the program if it found "error",
      > for example a keyword is listed in `#:example` but missing.
      > Sometimes we may want to do somthing before aborting,
      > such as closing file, network, etc.
      > `#:before-abort` is the parameter to do that.
      > `varg` will try to run the
      > <code><i><b>a-procedure-without-argument</b></i></code>
      > before it end the program.
      > > Note that if aborting is from chicken scheme system but not `varg`,
      > > this is not valid.
    - `#:enable-unknown`
      > If `#:enable-unknown` is set,
      > `varg` will append unknown arguments to
      > `#:literal` in result but not report error.
      - *e.g.*
        - `(varg #:enable-unknown '(#:literal only-one) '(1 2 #:a-keyword))`
          > This will return
          > `'((#:with-value) (#:without-value) (#:literal 1 2 #:a-keyword))`.
          > If `#:enable-unknown` is not set,
          > `varg` will report error that `2 #:a-keyword` is unknown.
    - [necessary]<code>(list <i><b>arguments-to-parse</b></i>)</code>

    Each part listed above can be omitted except item marked by "[necessary]".
    And order is not sensitive.
- output  
  <code>(varg <i><b>args</b></i>)</code> will
  return a list that contain:
  - <code>(cons #:with-value <i><b>list-of-key-value-pair</b></i>)</code>
  - <code>(cons #:without-value <i><b>list-of-keyword</b></i>)</code>
  - <code>(cons #:literal <i><b>any-list</b></i>)</code>

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
