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
;;     ;; function split and join (with the `include-utf8' option)
;;     (mapcar «list {* 2} {* 3}» '(1 2 3 4)) ; => ((2 3) (4 6) (6 9) (8 12))
;;     (mapcar «and {< 2} #'evenp» '(1 2 3 4)) ; => (NIL NIL NIL T)
;;     (mapcar «+ {* 2} {- _ 1}» '(1 2 3 4)) ; => (2 5 8 11)
;;
;; Load CURRY-COMPOSE-READER-MACROS at the REPL with the following
;;
;;     (ql:quickload :curry-compose-reader-macros)
;;     (ql:quickload :named-readtables)
;;     (use-package 'named-readtables)
;;     (in-readtable curry-compose-reader-macros:syntax)
;;
;; Use CURRY-COMPOSE-READER-MACROS in source by adding
;; NAMED-READTABLES and CURRY-COMPOSE-READER-MACROS to your ASDF file
;; and package and then including the following in source files which
;; use these reader macros.
;;
;;     (in-readtable :curry-compose-reader-macros)
;;
;; Emacs users may easily treat {}'s, []'s and «»'s as parenthesis
;; for paredit commands and SEXP movement with the following
;; configuration.
;;
;;     ;; Syntax table
;;     (modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\} "){" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\« "(»" lisp-mode-syntax-table)
;;     (modify-syntax-entry ?\» ")«" lisp-mode-syntax-table)
;;
;;     ;; Paredit keys
;;     (eval-after-load "paredit"
;;       '(progn
;;         (define-key paredit-mode-map "[" 'paredit-open-parenthesis)
;;         (define-key paredit-mode-map "]" 'paredit-close-parenthesis)
;;         (define-key paredit-mode-map "(" 'paredit-open-bracket)
;;         (define-key paredit-mode-map ")" 'paredit-close-bracket)
;;         (define-key paredit-mode-map "{" 'paredit-open-curly)
;;         (define-key paredit-mode-map "}" 'paredit-close-curly)))

;;; Code:
(in-package :curry-compose-reader-macros)

(defun lcurly-brace-reader (stream inchar)
  (declare (ignore inchar))
  (let ((spec (read-delimited-list #\} stream t)))
    (if (eq (cadr spec) '_)
        `(rcurry (function ,(car spec)) ,@(cddr spec))
        `(curry (function ,(car spec)) ,@(cdr spec)))))

(defun lsquare-brace-reader (stream inchar)
  (declare (ignore inchar))
  (cons 'compose (read-delimited-list #\] stream t)))

(defun langle-quotation-reader (stream inchar)
  (declare (ignore inchar))
  (let ((contents (read-delimited-list #\» stream t))
        (args (gensym "langle-quotation-reader")))
    `(lambda (&rest ,args)
       (,(car contents)                 ; Join function (or macro).
         ,@(mapcar (lambda (fun) `(apply ,fun ,args)) (cdr contents))))))

(defreadtable :curry-compose-reader-macros
  (:merge :current)

  (:macro-char #\{ #'curry-compose-reader-macros::lcurly-brace-reader)
  (:macro-char #\} (get-macro-character #\) ))

  (:macro-char #\[ #'curry-compose-reader-macros::lsquare-brace-reader)
  (:macro-char #\] (get-macro-character #\) ))

  (:macro-char #\« #'curry-compose-reader-macros::langle-quotation-reader)
  (:macro-char #\» (get-macro-character #\) )))
