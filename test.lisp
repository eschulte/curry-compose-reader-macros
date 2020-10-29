(defpackage :curry-compose-reader-macros/test
  (:use :cl :curry-compose-reader-macros
        :named-readtables)
  (:export :test))
(in-package :curry-compose-reader-macros/test)

(defparameter *r* (find-readtable :curry-compose-reader-macros))

(defparameter *tests* nil)

(eval-when (:compile-toplevel :load-toplevel)
  (defmacro deftest (name form &body expected-values)
    `(progn
       (pushnew ',name *tests*)
       (defun ,name ()
         (let ((*package* (find-package :curry-compose-reader-macros))
               (*readtable* (find-readtable :curry-compose-reader-macros)))
           (let ((vals (multiple-value-list ,form)))
             (equal vals ',expected-values)))))))

;;; Do tests manually to avoid circular dependency
;;; if a test framework decides to use c-c-r-m

(defun test ()
  (let ((passed nil)
        (failed nil))
    (flet ((%t (f)
             (handler-case (if (funcall f)
                               (push f passed)
                               (push f failed))
               (error () (push f failed)))))
      (mapc #'%t (reverse *tests*)))
    (setf failed (reverse failed))
    (when failed
      (error "Failed tests: ~a" failed))
    (reverse passed)))


(defun er (str)
  (eval (read-from-string str)))

(deftest curry.1
  (funcall (er "{list 1}") 2)
  (1 2))

(deftest curry.2
  (mapcar (er "{+ 1}") '(1 2 3 4))
  (2 3 4 5))

(deftest curry.fixed-arity
  (funcall (er "{1 list 1}") 2)
  (1 2))

(deftest curry.fixed-arity-error.1
  (locally (declare (optimize safety))
    (let ((fn (er "{1 list 1}")))
      (handler-case (progn (funcall fn) nil)
        (error () t))))
  t)

(deftest curry.fixed-arity-error.2
  (locally (declare (optimize safety))
    (let ((fn (er "{1 list 1}")))
      (handler-case (progn (funcall fn 'a 'b) nil)
        (error () t))))
  t)

(deftest rcurry.1
  (funcall (er "{list _ 1}") 2)
  (2 1))

(deftest rcurry.2
  (mapcar (er "{- _ 1}") '(1 2 3 4))
  (0 1 2 3))

(deftest compose.1
  (funcall (er "[{* 3} #'1+]") 1)
  6)

(deftest compose.2
  (funcall (er "['1+ '1+]") 1)
  3)

(deftest compose.3
  (funcall (er "[#'1+]") 1)
  2)

(deftest compose.4
  (funcall (er "[#'values]") 1 2 3)
  1 2 3)

(deftest join.1
  (funcall (er "«list {* 2} {* 3}»") 4)
  (8 12))

(deftest join.2
  (mapcar (er "«and {< 2} 'evenp (constantly t)»") '(1 2 3 4))
  (nil nil nil t))

(deftest typecase-bracket
  (mapcar (er "‹typecase (number #'1+) (string :str)›")
          '(1 "this" 2 "that"))
  (2 :str 3 :str))

(deftest cond-bracket
  (mapcar (er "‹cond (#'evenp {+ 100}) (#'oddp {+ 200})›") '(1 2 3 4))
  (201 102 203 104))

(deftest if-bracket
  (mapcar (er "‹if #'evenp {list :a} {list :b}›")
          '(1 2 3 4))
  ((:b 1) (:a 2) (:b 3) (:a 4)))

(deftest when-bracket
  (mapcar (er "‹when 'evenp {+ 4}›") '(1 2 3 4))
  (nil 6 nil 8))

(deftest unless-bracket
  (mapcar (er "‹unless 'evenp {+ 4}›") '(1 2 3 4))
  (5 nil 7 nil))

