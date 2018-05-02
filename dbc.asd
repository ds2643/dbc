(defsystem :dbc
  :description "Attempt at contract design system"
  :version "0.0.0"
  :depends-on ("alexandria" "lisp-unit")
  :components
  ((:file "package") (:file "core" :depends-on ("package"))))
