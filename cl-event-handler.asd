;;;; cl-event-handler.asd

(asdf:defsystem #:cl-event-handler
  :description "A simple object oriented event driven framework."
  :author "June Tate-Gans <june@theonelab.com>"
  :license "Simplified BSD License"
  :serial t
  :depends-on (#:chanl #:log4cl)
  :components ((:file "package")
               (:file "event")
               (:file "handler")))
