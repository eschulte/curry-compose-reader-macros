(defsystem :curry-compose-reader-macros
  :description
  "reader macros for concise function partial application and composition"
  :author "Eric Schulte <schulte.eric@gmail.com>"
  :version "1.0.0"
  :licence "Public Domain"
  :depends-on (alexandria)
  :components
  ((:file "package")
   (:file "curry-compose-reader-macros" :depends-on ("package"))))
