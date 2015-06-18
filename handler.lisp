(in-package :cl-event-handler)

(defclass handler ()
  ((name
    :initform nil
    :type string
    :initarg :name
    :documentation "The name of this handler.")
   (event-channel
    :initform (make-instance 'unbounded-channel)
    :initarg :event-channel
    :documentation "The channel to use for listening for inbound events.")
   (event-task
    :initform nil
    :type thread
    :documentation "The task that runs the event loop.")
   (start-immediately
    :initform t
    :initarg :start-immediately
    :type boolean
    :documentation "Whether or not to immediately start the event loop or not.
Defaults to T."))
  (:documentation "A handler is an event handling object that spawns a thread to
dispatch events placed on its queue. Events are sent to the handler via the
`post-event' generic method, and are executed via the `handle-event' generic
method."))

;;;; Interface

(defgeneric post-event (handler event)
  (:documentation "Posts EVENT to HANDLER's queue."))
(defgeneric handle-event (handler event)
  (:documentation "Handles an EVENT on HANDLER's queue from HANDLER's
event-thread context. The default method does nothing."))
(defgeneric start-handler (handler)
  (:documentation "Starts HANDLER's event loop if it is not running already."))
(defgeneric handler-alive-p (handler)
  (:documentation "Returns T if the given HANDLER instance is running, NIL
otherwise."))
(defgeneric handler-runloop (handler)
  (:documentation "Handles each event on the event queue by popping the end of
calling `handle-event' on each event in the queue."))

;;;; Default methods and functions

(defmethod print-object ((object handler) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name event-channel event-task) object
      (format stream ":NAME ~a :EVENT-cHANNEL ~a :EVENT-TASK ~a"
              name event-channel event-task))))

(defmethod post-event ((handler handler) (event event))
  "Posts an event to the given HANDLER. Note: this is blocking."
  (with-slots (event-channel) handler
    (send event-channel event)))

(defmethod handle-event ((handler handler) (event event))
  "General catch-all HANDLE-EVENT method to warn when an unhandled event is fired."
  (log:warn "Unhandled event for ~a: ~a" handler event)
  (values))

(defmethod handle-event ((handler handler) (event ping-event))
  "Event handler for incoming PING-EVENTs. If the SENDER slot in the event is
set to a valid HANDLER instance, this method will reply with a PONG-EVENT
instance."
  (with-slots (sender) event
    (when (not (null sender))
      (log:info "PING from ~a" sender)
      (post-event sender (make-instance 'pong-event :sender handler))))
  (values))

(defmethod handle-event ((handler handler) (event pong-event))
  "General event handler for PONG-EVENT instances. Simply logs the PONG occurred."
  (with-slots (sender) event
    (log:info "PONG from ~a" sender)))

(defmethod handler-runloop ((handler handler))
  "Handles each event on the event queue by popping the end of calling `handle-event' on each event
in the queue."
  (with-slots (event-channel) handler
    (loop
       (let ((event (recv event-channel)))
         (handle-event handler event)))))

(defmethod start-handler ((handler handler))
  "Starts a handler listening for events."
  (unless (handler-alive-p handler)
    (with-slots (name event-channel event-task) handler
      (setf event-task (pexec () (handler-runloop handler)))
      (log:info "Event loop started for handler ~a in task ~a" handler event-task))))

(defmethod handler-alive-p ((handler handler))
  "Returns T if the given HANDLER instance is running, NIL otherwise."
  (with-slots (event-task) handler
    (if (not (null event-task))
        (eq :alive (task-status event-task))
        nil)))

(defmethod initialize-instance :after ((handler handler) &key)
  (with-slots (start-immediately) handler
    (when start-immediately (start-handler handler))))

(export '(handler
          post-event
          handle-event
          start-handler
          handler-alive-p
          handler-runloop))
