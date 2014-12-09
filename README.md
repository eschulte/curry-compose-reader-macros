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
the results.

    ;; function split and join
    (mapcar «{* 2} {* 3}» '(1 2 3 4)) ; => ((2 3) (4 6) (6 9) (8 12))

To use call `enable-curry-compose-reader-macros` from within
`eval-when` to ensure that reader macros are defined for both
compilation and execution.

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-curry-compose-reader-macros))

Or to load utf8 support as well (which is probably going too far).

    (eval-when (:compile-toplevel :load-toplevel :execute)
      (enable-curry-compose-reader-macros :include-utf8))

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
