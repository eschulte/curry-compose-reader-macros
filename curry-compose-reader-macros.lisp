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
;;     (mapcar «{* 2} {* 3}» '(1 2 3 4)) ; => ((2 3) (4 6) (6 9) (8 12))
;;
;; `enable-curry-compose-reader-macros' is a macro which wraps itself
;; in `eval-when' to ensure that reader macros are defined for both
;; compilation and execution.
;;
;; Or to load utf8 support as well (which is probably going too far).
;;
;;     (enable-curry-compose-reader-macros :include-utf8)
;;
;; To load support for working with the series library.
;;
;;     (enable-curry-compose-reader-macros nil :include-series)
;;
;; This will enable the following three reader macros.
;;
;; \#M
;; :   *Map* a function over a series.  This is a very slight variation on
;;     #M in the series library.  This macro expands into an appropriate
;;     call to the `series:map-fn` function enabling forms using like
;;     the following.
;;
;;         (#M [{+ 1} {* 2}] #Z(1 2 3 4)) ; => #Z(3 5 7 9)
;;
;; \#U 
;; :   Take from a series *until* a function matches.
;;
;;         (#U [{= 4} {+ 1}] (scan-range)) ; => #Z(0 1 2)
;;
;; \#C
;; :   *Choose* from a series when a function matches.
;;
;;         (#C [#'oddp {+ 1}] #Z(1 2 3 4)) ; => #Z(2 4)
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

(defvar *previous-readtables* nil)

(defmacro enable-curry-compose-reader-macros (&optional include-utf8 include-series)
  "Enable concise syntax for Alexandria's `curry', `rcurry' and `compose'."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (push *readtable* *previous-readtables*)
     ;; partial application with {} using Alexandria's `curry' and `rcurry'
     (set-syntax-from-char #\{ #\( )
     (set-syntax-from-char #\} #\) )

     (defun lcurly-brace-reader (stream inchar)
       (declare (ignore inchar))
       (let ((spec (read-delimited-list #\} stream t)))
         (if (eq (cadr spec) '_)
             `(rcurry (function ,(car spec)) ,@(cddr spec))
             `(curry (function ,(car spec)) ,@(cdr spec)))))

     (set-macro-character #\{ #'lcurly-brace-reader)
     (set-macro-character #\} (get-macro-character #\) ))

     ;; composition with [] using Alexandria's `compose'
     (set-syntax-from-char #\[ #\( )
     (set-syntax-from-char #\] #\) )

     (defun lsquare-brace-reader (stream inchar)
       (declare (ignore inchar))
       (cons 'compose (read-delimited-list #\] stream t)))

     (set-macro-character #\[ #'lsquare-brace-reader)
     (set-macro-character #\] (get-macro-character #\) ))

     (when ,include-utf8
       ;; inform lisp that source code is encoded in UTF-8
       #+sbcl (setf sb-impl::*default-external-format* :UTF-8)

       ;; list split collection with «»
       (set-syntax-from-char #\« #\( )
       (set-syntax-from-char #\» #\) )

       (defun langle-quotation-reader (stream inchar)
         (declare (ignore inchar))
         (let ((funcs (cons 'list (read-delimited-list #\» stream t))))
           `(compose (curry #'mapcar #'funcall ,funcs)
                     (curry #'make-list ,(length funcs) :initial-element))))

       (set-macro-character #\« #'langle-quotation-reader)
       (set-macro-character #\» (get-macro-character #\))))

     (when ,include-series ; Taken from the #M macro in the series package.

       ;; Mapping.
       (defun mapit (fn type args)
         (if (not (symbolp fn))
             `(series:map-fn ',type ,fn ,@args)
             (cl:let ((vars (do ((a args (cdr a))
                                 (l nil (cons (gensym "V-") l)))
                                ((null a) (return l)))))
               `(series:map-fn
                 ',type (function (lambda ,vars (,fn ,@ vars))) ,@args))))

       (defmacro mapit-1 (fn &rest args) (mapit fn t args))
       (defmacro mapit-2 (fn &rest args) (mapit fn '(values t t) args))
       (defmacro mapit-3 (fn &rest args) (mapit fn '(values t t t) args))
       (defmacro mapit-4 (fn &rest args) (mapit fn '(values t t t t) args))
       (defmacro mapit-5 (fn &rest args) (mapit fn '(values t t t t t) args))

       (defun series-abbrev-map-reader (stream subchar arg)
         (declare (ignorable stream subchar))
         (case arg
           ((1 nil) 'mapit-1)
           (2 'mapit-2)
           (3 'mapit-3)
           (4 'mapit-4)
           (5 'mapit-5)
           (:otherwise
            (error
             "The numeric argument to #M must be between 1 and 5 inclusive"))))

       (set-dispatch-macro-character #\# #\M #'series-abbrev-map-reader)

       ;; Until.
       (defmacro untilit (fn &rest args)
         (if (not (symbolp fn))
             `(series:until-if ,fn ,@ args)
             (cl:let ((vars (do ((a args (cdr a))
                                 (l nil (cons (gensym "V-") l)))
                                ((null a) (return l)))))
               `(series:until-if (function (lambda ,vars (,fn ,@ vars))) ,@ args))))

       (defun series-abbrev-until-reader (stream subchar arg)
         (declare (ignorable stream subchar arg))
         'untilit)

       (set-dispatch-macro-character #\# #\U #'series-abbrev-until-reader)

       ;; Removal.
       (defmacro chooseit (fn &rest args)
         (if (not (symbolp fn))
             `(series:choose-if ,fn ,@ args)
             (cl:let ((vars (do ((a args (cdr a))
                                 (l nil (cons (gensym "V-") l)))
                                ((null a) (return l)))))
               `(series:choose-if (function (lambda ,vars (,fn ,@ vars))) ,@ args))))

       (defun series-abbrev-choose-reader (stream subchar arg)
         (declare (ignorable stream subchar arg))
         'chooseit)

       (set-dispatch-macro-character #\# #\C #'series-abbrev-choose-reader))))

(defmacro disable-curry-compose-reader-macros ()
  '(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf *readtable* (pop *previous-readtables*))))
