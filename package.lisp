;;;; package.lisp

(defpackage :net.phorni.uwsgi
  (:use :cl)
  (:nicknames :uwsgi)
  (:export :*encoding*)
  (:export :start
	   :stop
	   :<uwsgi-server>
	   :make-uwsgi-server
	   :<uwsgi-request>
	   :<uwsgi-response>
	   :send-response
	   :client-socket
	   :status-code
	   :status-message
	   :headers
	   :push-header
	   :body))


