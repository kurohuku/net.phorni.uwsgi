;; example

(asdf:load-system :net.phorni.uwsgi)

(defun request-handler (req)
  req)

(defun response-handler (client r)
  (let ((res (make-instance 'net.phorni.uwsgi:<uwsgi-response>)))
    (setf (net.phorni.uwsgi:status-code res) 200)
    (setf (net.phorni.uwsgi:status-message res) "OK")
    (net.phorni.uwsgi:push-header res "Content-Type" "text/plain; charset=utf-8")
    (net.phorni.uwsgi:push-header res "Server" "AAA")
    (push (format nil "hello, world~%") (net.phorni.uwsgi:body res))
    (loop :for (k . v) in (net.phorni.uwsgi:headers r)
       :do (push (format nil "~A, ~A~%" k v) (net.phorni.uwsgi:body res)))
    (setf (net.phorni.uwsgi:body res)
	  (reverse (net.phorni.uwsgi:body res)))
    (net.phorni.uwsgi:send-response client res)))

(defvar s
  (net.phorni.uwsgi:start #'request-handler #'response-handler))

;; (net.phorni.uwsgi:stop s)
