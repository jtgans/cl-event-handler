(in-package :cl-event-handler)

(defclass event ()
  ((sender :initarg :sender
           :type handler
           :reader sender))
  (:documentation "An event. SENDER contains the HANDLER to use for
responses."))

(defmethod print-object ((object event) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (sender) object
      (format stream ":SENDER ~a" sender))))

(defclass ping-event (event)
  ()
  (:documentation "An event used to ping a handler for aliveness."))

(defclass pong-event (event)
  ()
  (:documentation "An event used as a reply to PING-EVENT events."))

(export '(event
          ping-event
          pong-event))
