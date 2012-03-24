;;;; uWSGI Server

(in-package :net.phorni.uwsgi)

(defconstant +crlf+ (coerce '(13 10) '(vector (unsigned-byte 8))))
(defvar *encoding* :utf-8)


(defmacro while (test &body body)
  `(loop while ,test do (progn ,@body)))

(defclass <uwsgi-server> ()
  ((event-base
    :accessor uwsgi-event-base
    :initform (make-instance 'iolib.multiplex:event-base))
   (server
    :accessor uwsgi-server-socket
    :initform nil
    :initarg :socket)))

(defun make-uwsgi-server (&key (port 9999) (reuse-address t))
  (let ((sock (iolib.sockets:make-socket :address-family :ipv4
					   :type :stream
					   :connect :passive
					   :local-host "0.0.0.0"
					   :local-port port
					   :reuse-address reuse-address)))
    (make-instance '<uwsgi-server> :socket sock)))

(defclass <uwsgi-request> ()
  ((headers :accessor headers
	    :initform nil)
   (socket :accessor client-socket
	   :initarg :socket
	   :initform (error ""))))

(defclass <uwsgi-response> ()
  ((status-code :accessor status-code :initform 200)
   (status-message :accessor status-message :initform "")
   (headers :accessor headers :initform nil)
   (body :accessor body :initform nil)))

(defun make-header (key val)
  (cons key val))
(defmethod push-header ((response <uwsgi-response>) key val)
  (push (make-header key val) (headers response)))
(defmethod push-header ((request <uwsgi-request>) key val)
  (push (make-header key val) (headers request)))

(defun send-response (client response)
  (let ((body (body response)))
    (when (listp body)
      (setf body (apply #'concatenate 'string body)))
    (let ((octets (babel:string-to-octets body :encoding *encoding*)))
      (unless (assoc "Conetnt-Length" (headers response) 
		     :test #'string-equal)
	(push-header response "Content-Length" (length octets)))
      ;; send status line
      (format client "~A ~A ~A"
	      "HTTP/1.1"
	      (status-code response)
	      (or (status-message response) ""))
      (iolib.sockets:send-to client +CRLF+)
      ;; send headers
      (loop :for (k . v) in (headers response)
	 :do (format client "~A: ~A" k v)
	 (iolib.sockets:send-to client +CRLF+))
      (iolib.sockets:send-to client +CRLF+)
      ;; send body
      (iolib.sockets:send-to client octets))))

(defun uwsgi-modifier1 (uwsgi-header)
  (aref uwsgi-header 0))
(defun uwsgi-data-size (uwsgi-header)
  (arr-u16-le uwsgi-header 1))
(defun uwsgi-modifier2 (uwsgi-header)
  (aref uwsgi-header 3))

(defun start (request-handler response-handler
	      &key (port 9999) (reuse-address t))
  (let ((uwsgi-server (make-uwsgi-server
		       :port port
		       :reuse-address reuse-address)))
    (bordeaux-threads:make-thread
     (lambda ()
       (start-uwsgi-server
	uwsgi-server
	request-handler
	response-handler)))
    uwsgi-server))

(defun stop (uwsgi-server)
  (iolib.multiplex:exit-event-loop
   (uwsgi-event-base uwsgi-server)))

(defun start-uwsgi-server
    (uwsgi-server request-handler response-handler)
  (let ((event-base (uwsgi-event-base uwsgi-server))
	(server (uwsgi-server-socket uwsgi-server)))
    (iolib.sockets:listen-on server)
    (iolib.multiplex:set-io-handler
     event-base
     (iolib.streams:fd-of server)
     :read (make-server-socket-handler event-base
				       request-handler
				       response-handler))
    (unwind-protect
	   (iolib.multiplex:event-dispatch event-base)
      (iolib.multiplex:remove-fd-handlers
       event-base
       (iolib.streams:fd-of server))
      (iolib.sockets:shutdown server :read t :write t)
      (close server)
      (close event-base))))


(defun make-server-socket-handler (event-base request-handler response-handler)
  (lambda (fd event exception)
    (let* ((server (iolib.sockets:make-socket-from-fd fd :connect :passive))
	   (client (iolib.sockets:accept-connection server)))
      (iolib.multiplex:set-io-handler
       event-base
       (iolib.streams:fd-of client)
       :read (make-client-socket-handler event-base
					 client
					 request-handler
					 response-handler)))))


(defun make-client-socket-handler (event-base client request-handler response-handler)
  (lambda (fd event exception)
    (unwind-protect
	 (handler-case
	     (funcall
	      response-handler client
	      (funcall request-handler (make-uwsgi-request client)))
	   (iolib.sockets:socket-connection-reset-error ()
	     nil)
	   (iolib.sockets:socket-not-connected-error ()
	     nil))
      (ignore-errors 
	(iolib.multiplex:remove-fd-handlers event-base fd)
	(iolib.sockets:shutdown client :read t :write t)
	(close client)))))

(defun make-uwsgi-request (client)
  (let* ((req (make-instance '<uwsgi-request> :socket client))
	 (nbytes 0)
	 (uwsgi-header (make-array 4 :element-type '(unsigned-byte 8))))
    (multiple-value-setq
	(uwsgi-header nbytes)
	(iolib.sockets:receive-from client :buffer uwsgi-header))
    (let* ((data-size (uwsgi-data-size uwsgi-header))
	   (buf (make-array data-size :element-type '(unsigned-byte 8)))
	   (i 0)
	   (key-size 0)
	   (val-size 0)
	   (key "")
	   (val ""))
      (multiple-value-setq
	  (buf nbytes)
	  (iolib.sockets:receive-from client :buffer buf))
      (while (< i data-size)
	(setf key-size (arr-u16-le buf i))
	(setf key (babel:octets-to-string buf
					  :encoding *encoding*
					  :start (+ i 2)
					  :end (+ i 2 key-size)))
	(setf val-size (arr-u16-le buf (+ i 2 key-size)))
	(setf val (babel:octets-to-string buf
					  :encoding *encoding*
					  :start (+ i 4 key-size)
					  :end (+ i 4 key-size val-size)))
	(push-header req key val)
	(incf i (+ 4 key-size val-size)))
      req)))

(defun arr-u16-le (arr start)
  (+ (aref arr start)
     (* 256 (aref arr (1+ start)))))


