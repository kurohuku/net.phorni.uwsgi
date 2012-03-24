;;;; net.phorni.uwsgi.asd

(asdf:defsystem :net.phorni.uwsgi
  :description "uWSGI Server"
  :depends-on (:iolib :bordeaux-threads :babel)
  :components ((:file "package")
	       (:file "uwsgi")))

