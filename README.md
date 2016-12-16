# CURRY-COMPOSE-READER-MACROS

Reader macros for concise expression of function partial application
and composition.

These reader macros expand into the `curry`, `rcurry` and `compose`
functions from the Alexandria library.  The contents of curly brackets
are curried and the contents of square brackets are composed.  The `_`
symbol inside curly brackets changes the order of arguments with
`rcurry`.

The following examples demonstrate usage.

    ;; partial application `curry'
    (mapcar {+ 1} '(1 2 3 4)) ; => (2 3 4 5)

    ;; alternate order of arguments `rcurry'
    (mapcar {- _ 1} '(1 2 3 4)) ; => (0 1 2 3)

    ;; function composition
    (mapcar [#'list {* 2}] '(1 2 3 4)) ; => ((2) (4) (6) (8))

Additionally, if optional utf8 support is enabled, special brackets
may be used to split arguments amongst a list of functions and collect
the results.  The first element of the `«»`-delimited list is the
"join" function.  Incoming arguments are split out to the remaining
functions in the `«»`-delimited list, and their results are then
passed to the join function.

    ;; function split and join
    (mapcar «list {* 2} {* 3}» '(1 2 3 4)) ; => ((2 3) (4 6) (6 9) (8 12))
    (mapcar «and {< 2} #'evenp» '(1 2 3 4)) ; => (NIL NIL NIL T)
    (mapcar «+ {* 2} {- _ 1}» '(1 2 3 4)) ; => (2 5 8 11)

`enable-curry-compose-reader-macros` is a macro which wraps itself in
`eval-when` to ensure that reader macros are defined for both
compilation and execution.

    (enable-curry-compose-reader-macros)

Or to load utf8 support as well (which is probably going too far).

    (enable-curry-compose-reader-macros :include-utf8)

To load support for working with the series library.

    (enable-curry-compose-reader-macros nil :include-series)

This will enable the following three reader macros.

\#M
:   *Map* a function over a series.  This is a very slight variation on
    #M in the series library.  This macro expands into an appropriate
    call to the `series:map-fn` function enabling forms using like the following.

        (#M [{+ 1} {* 2}] #Z(1 2 3 4)) ; => #Z(3 5 7 9)

\#U 
:   Take from a series *until* a function matches.

        (#U [{= 4} {+ 1}] (scan-range)) ; => #Z(0 1 2)

\#C
:   *Choose* from a series when a function matches.

        (#C [#'oddp {+ 1}] #Z(1 2 3 4)) ; => #Z(2 4)

Emacs users may easily treat `{}`'s, `[]`'s and `«»`'s as parenthesis
for paredit commands and SEXP movement with the following
configuration.

    ;; Syntax table
    (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
    (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
    (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
    (modify-syntax-entry ?\} "){" lisp-mode-syntax-table)
    ;; optional UTF8 characters
    (modify-syntax-entry ?\« "(»" lisp-mode-syntax-table)
    (modify-syntax-entry ?\» ")«" lisp-mode-syntax-table)

    ;; Paredit keys
    (eval-after-load "paredit"
      '(progn
        (define-key paredit-mode-map "[" 'paredit-open-parenthesis)
        (define-key paredit-mode-map "]" 'paredit-close-parenthesis)
        (define-key paredit-mode-map "(" 'paredit-open-bracket)
        (define-key paredit-mode-map ")" 'paredit-close-bracket)
        (define-key paredit-mode-map "{" 'paredit-open-curly)
        (define-key paredit-mode-map "}" 'paredit-close-curly)))
