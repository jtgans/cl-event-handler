;;;; package.lisp

(defpackage #:cl-event-handler
  (:use #:cl #:chanl)
  (:documentation "A simple library to create object-oriented event handlers,
built on top of chanL.")
  (:export event
           ping-event
           pong-event
           handler
           post-event
           handle-event
           start-handler))
