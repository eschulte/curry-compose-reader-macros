(defsystem :curry-compose-reader-macros
  :description "reader macros for concise partial application and composition"
  :version "1.0.0"
  :licence "GPL V3"
  :depends-on (alexandria)
  :components
  ((:file "package")
   (:file "curry-compose-reader-macros" :depends-on ("package"))))
