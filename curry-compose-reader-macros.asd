(defsystem :curry-compose-reader-macros
  :description
  "reader macros for concise function partial application and composition"
  :author "Eric Schulte <schulte.eric@gmail.com>"
  :version "1.0.0"
  :licence "Public Domain"
  :depends-on (alexandria named-readtables)
  :in-order-to ((test-op (load-op "curry-compose-reader-macros/test")))
  :perform (test-op (o c) (symbol-call :curry-compose-reader-macros/test '#:test))
  :components
  ((:file "package")
   (:file "curry-compose-reader-macros" :depends-on ("package"))))

(defsystem :curry-compose-reader-macros/test
  :description
  "Tests for curry-compose-reader-macros"
  :depends-on (curry-compose-reader-macros)
  :components ((:file "test")))

