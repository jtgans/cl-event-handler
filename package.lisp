;;;; package.lisp

(defpackage #:cl-event-handler
  (:use #:cl #:chanl #:log4cl)
  (:documentation "A simple library to create object-oriented event handlers,
built on top of chanL."))
