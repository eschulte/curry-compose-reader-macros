;;; curry-compose-reader-macros --- partial application and composition

;; Copyright (C) Eric Schulte 2013

;; Placed in the public domain.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary

;; Reader macros for concise expression of function partial
;; application and composition.
;;
;; These reader macros expand into the `curry', `rcurry' and `compose'
;; functions from the Alexandria library.  The contents of curly
;; brackets are curried and the contents of square brackets are
;; composed.  The `_' symbol inside curly brackets changes the order
;; of arguments with `rcurry'.
;;
;; The following examples demonstrate usage.
;;
;;     ;; partial application `curry'
;;     (mapcar {+ 1} '(1 2 3 4)) ; => (2 3 4 5)
;;
;;     ;; alternate order of arguments `rcurry'
;;     (mapcar {- _ 1} '(1 2 3 4)) ; => (0 1 2 3)
;;
;;     ;; function composition
;;     (mapcar [#'list {* 2}] '(1 2 3 4)) ; => ((2) (4) (6) (8))
;;
;; Additionally, special brackets may be used to split arguments amongst a
;; list of functions and collect the results.  The first element of the
;; `«»`-delimited list is the "join" function.  Incoming arguments are
;; split out to the remaining functions in the `«»`-delimited list, and
;; their results are then passed to the join function.  (Emacs users can
;; type `«` and `»` with `C-x 8 <` and `C-x 8 >` respectively)
;;
;;     ;; function split and join
;;     (mapcar «list {* 2} {* 3}» '(1 2 3 4)) ; => ((2 3) (4 6) (6 9) (8 12))
;;     (mapcar «and {< 2} #'evenp» '(1 2 3 4)) ; => (NIL NIL NIL T)
;;     (mapcar «+ {* 2} {- _ 1}» '(1 2 3 4)) ; => (2 5 8 11)
;;
;; Finally, and this is probably too much, even more special brackets can
;; be useful for generating `cond`/`case` forms.  This can be useful for
;; processing lists of heterogeneous types.  For `case` (not `cond`)
;; forms, the first element of the `‹›`-delimited list is the case
;; "keyform."  The other elements are two-element `cond`/`case` clauses
;; in which the first is the guard, and the second is the value.
;; Elements of the guard *are evaluated at read time* to determine if
;; they are functions which should be applied, or literals which should
;; be included verbatim--this could be dangerous in some cases so beware.
;;
;;     ;; concise case expressions
;;     (mapcar ‹typecase (number #'1+) (string :str)› '(1 "this" 2 "that"))
;;                                             ; => (2 :STR 3 :STR)
;;     (mapcar ‹cond (#'evenp {+ 100}) (#'oddp {+ 200})› '(1 2 3 4))
;;                                             ; => (201 102 203 104)
;;     ;; also if/when/unless support
;;     (mapcar ‹if #'evenp #'1+ #'1-› '(1 2 3 4)) ; => (0 3 2 5)
;;
;; Load CURRY-COMPOSE-READER-MACROS at the REPL with the following
;;
;;     (ql:quickload :curry-compose-reader-macros)
;;     (ql:quickload :named-readtables)
;;     (use-package 'named-readtables)
;;     (in-readtable :curry-compose-reader-macros)
;;
;; Use CURRY-COMPOSE-READER-MACROS in source by adding
;; NAMED-READTABLES and CURRY-COMPOSE-READER-MACROS to your ASDF file
;; and package and then including the following in source files which
;; use these reader macros.
;;
;;     (in-readtable :curry-compose-reader-macros)
;;
;; Emacs users may easily treat {}'s, []'s, «»'s, and ‹›'s as
;; parenthesis for paredit commands and SEXP movement with the
;; following configuration.
;;
;;     ;; Syntax table
;;     (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\} "){" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\« "(»" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\» ")«" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\‹ "(›" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\› ")‹" lisp-mode-syntax-table)
;;
;;     ;; Paredit keys
;;     (eval-after-load "paredit"
;;       '(progn
;;         (define-key paredit-mode-map "[" 'paredit-open-parenthesis)
;;         (define-key paredit-mode-map "]" 'paredit-close-parenthesis)
;;         (define-key paredit-mode-map "(" 'paredit-open-bracket)
;;         (define-key paredit-mode-map ")" 'paredit-close-bracket)
;;         (define-key paredit-mode-map "«" 'paredit-open-special)
;;         (define-key paredit-mode-map "»" 'paredit-close-special)
;;         (define-key paredit-mode-map "‹" 'paredit-open-special)
;;         (define-key paredit-mode-map "›" 'paredit-close-special)))
;;
;; Also, Emacs will insert `«` with `C-x 8 <` and `»` with `C-x 8 >`.
;; Emacs can be instructed to define similar key-bindings for `‹` with
;; the following:
;;
;;     (global-set-key (kbd "\C-x 8 (") (lambda () (interactive) (insert-char #x2039)))
;;     (global-set-key (kbd "\C-x 8 )") (lambda () (interactive) (insert-char #x203A)))
;;
;; which will cause Emacs to insert `‹` with `C-x 8 (` and `›` with
;; `C-x 8 )` respectively.


;;; Code:
(in-package :curry-compose-reader-macros)

(defun lcurly-brace-reader (stream inchar)
  (declare (ignore inchar))
  (let ((spec (read-delimited-list #\} stream t)))
    (if (eq (cadr spec) '_)
        `(the function (rcurry (function ,(car spec)) ,@(cddr spec)))
        `(the function (curry (function ,(car spec)) ,@(cdr spec))))))

(defun lsquare-brace-reader (stream inchar)
  (declare (ignore inchar))
  (list 'the 'function (cons 'compose (read-delimited-list #\] stream t))))

(defun langle-quotation-reader (stream inchar)
  (declare (ignore inchar))
  (let ((contents (read-delimited-list #\» stream t))
        (args (gensym "langle-quotation-reader")))
    `(lambda (&rest ,args)
       (,(car contents)                 ; Join function (or macro).
         ,@(mapcar (lambda (fun) `(apply ,fun ,args)) (cdr contents))))))

(defun lsingle-pointing-angle-quotation-mark-reader (stream inchar)
  (declare (ignore inchar))
  (flet ((function-p (form) (functionp (ignore-errors (eval form)))))
    (let ((contents (read-delimited-list #\› stream t))
          (arg (gensym "lsingle-pointing-angle-quotation-mark-reader")))
      `(lambda (,arg)
         (,(car contents)               ; Case form.
           ,@(case (car contents)       ; If/when/unless guard.
               ((if when unless)
                `((funcall ,(cadr contents) ,arg)))
               (cond nil)
               (t (list arg)))
           ,@(if (member (car contents) '(if when unless)) ; Clauses.
                 (mapcar (lambda (clause)
                           (if (function-p clause)
                               `(funcall ,clause ,arg)
                               clause))
                         (cddr contents))
                 (mapcar (lambda (clause)
                           `(,(if (function-p (car clause))
                                  `(funcall ,(car clause) ,arg)
                                  (car clause))
                              ,(if (function-p (cadr clause))
                                   `(funcall ,(cadr clause) ,arg)
                                   (cadr clause))))
                         (cdr contents))))))))

(defreadtable :curry-compose-reader-macros
  (:merge :current)

  (:macro-char #\{ #'curry-compose-reader-macros::lcurly-brace-reader)
  (:macro-char #\} (get-macro-character #\) ))

  (:macro-char #\[ #'curry-compose-reader-macros::lsquare-brace-reader)
  (:macro-char #\] (get-macro-character #\) ))

  (:macro-char #\« #'curry-compose-reader-macros::langle-quotation-reader)
  (:macro-char #\» (get-macro-character #\) ))

  (:macro-char #\‹ #'curry-compose-reader-macros::lsingle-pointing-angle-quotation-mark-reader)
  (:macro-char #\› (get-macro-character #\) )))
