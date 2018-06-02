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

Additionally special brackets may be used to split arguments amongst a
list of functions and collect the results.  The first element of the
`«»`-delimited list is the "join" function.  Incoming arguments are
split out to the remaining functions in the `«»`-delimited list, and
their results are then passed to the join function.

    ;; function split and join
    (mapcar «list {* 2} {* 3}» '(1 2 3 4)) ; => ((2 3) (4 6) (6 9) (8 12))
    (mapcar «and {< 2} #'evenp» '(1 2 3 4)) ; => (NIL NIL NIL T)
    (mapcar «+ {* 2} {- _ 1}» '(1 2 3 4)) ; => (2 5 8 11)

Load CURRY-COMPOSE-READER-MACROS at the REPL with the following

    (ql:quickload :curry-compose-reader-macros)
    (ql:quickload :named-readtables)
    (use-package 'named-readtables)
    (in-readtable :curry-compose-reader-macros)

Use CURRY-COMPOSE-READER-MACROS in source by adding NAMED-READTABLES
and CURRY-COMPOSE-READER-MACROS to your ASDF file and package and then
including the following in source files which use these reader macros.

    (in-readtable :curry-compose-reader-macros)

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
