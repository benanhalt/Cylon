;;;; cylon.asd

(asdf:defsystem #:cylon
  :serial t
  :description "Describe cylon here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (:lisp-unit)
  :components ((:file "package")
               (:file "lexer")
               (:file "lexer-tests")))

