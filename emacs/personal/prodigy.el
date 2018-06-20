;; Prodigy config

(prelude-require-package 'prodigy)

(prodigy-define-status :id 'starting :face 'prodigy-yellow-face)

(prodigy-define-service
  :name "runserver"
  :command "pipenv"
  :args '("run" "python" "manage.py" "runserver")
  :cwd "~/proj/jellyfish"
  :path '("~/proj/jellyfish")
  :port 8000
  :tags '(work python)
  :stop-signal 'sigint
  :on-output (lambda (&rest args)
               (let ((output (plist-get args :output))
                     (service (plist-get args :service)))
                 ;;
                 (cond ((s-matches? "Performing system checks..." output)
                        (prodigy-set-status service 'starting))
                       ((s-matches? "Quit the server with CONTROL-C." output)
                        (prodigy-set-status service 'running))
                       ((or (s-matches? "Traceback (most recent call last):" output)
                            (s-matches? "Error:" output))
                        (prodigy-set-status service 'failed)))))
  :kill-process-buffer-on-stop nil
  :url "http://localhost:8000")

(prodigy-define-service
  :name "celery"
  :command "pipenv"
  :args '("run" "python" "manage.py" "celery")
  :cwd "~/proj/jellyfish"
  :path '("~/proj/jellyfish")
  :tags '(work python)
  :stop-signal 'sigint
  :on-output (lambda (&rest args)
               (let ((output (plist-get args :output))
                     (service (plist-get args :service)))
                 (cond ((s-matches? ": INFO/MainProcess] celery@[a-z-_]+ ready." output)
                        (prodigy-set-status service 'running))
                       ((s-matches? "Reloading celery worker..." output)
                        (prodigy-set-status service 'starting))
                       ((s-matches? "Traceback (most recent call last):" output)
                        (prodigy-set-status service 'failed)))))
  :kill-process-buffer-on-stop nil)

(prodigy-define-service
  :name "jupyter notebook"
  :command "pipenv"
  :args '("run" "python" "manage.py" "shell_plus" "--notebook" "--no-browser")
  :cwd "~/proj/jellyfish"
  :path '("~/proj/jellyfish")
  :tags '(work python)
  :stop-signal 'sigkill
  :on-output (lambda (&rest args)
               (let* ((output (plist-get args :output))
                      (service (plist-get args :service))
                      (jupyter-matches (s-match "\\(http://localhost:[0-9]\\{4\\}\\)/\\?token=\\([A-Za-z0-9]+\\)" output)))
                 (cond (jupyter-matches (let ((jupyter-url (cadr jupyter-matches))
                                              (jupyter-token (caddr jupyter-matches)))
                                          (message "Logging into %s with token %s" jupyter-url jupyter-token)
                                          (prodigy-set-status service 'ready)
                                          (ein:notebooklist-login jupyter-url jupyter-token)))
                       ((s-matches? "The Jupyter Notebook is running at:" output)
                        (prodigy-set-status service 'running))
                       ((s-matches? "received signal [0-9]+, stopping" output)
                        (prodigy-set-status service 'stopping))
                       ((s-contains-p "Serving notebooks from local directory:" output)
                        (prodigy-set-status service 'starting)))))
  :kill-process-buffer-on-stop nil)

(prodigy-define-service
  :name "avexno demo"
  :command "python2"
  :args '("-m" "SimpleHTTPServer" "8080")
  :cwd "~/Dropbox/jelly/Avexno/admin_html"
  :path '("~/Dropbox/jelly/Avexno/admin_html")
  :tags '(work python)
  :stop-signal 'sigint
  :ready-message "Serving HTTP on 0.0.0.0 port 8080 ..."
  :port 8080
  :url "http://localhost:8080"
  :kill-process-buffer-on-stop nil)

;; (prodigy-define-service
;;   :name "skipper"
;;   :command "pipenv"
;;   :args '("run" "flask" "run")
;;   :cwd "~/proj/jellyfish"
;;   :path '("~/proj/jellyfish")
;;   :tags '(work python)
;;   :stop-signal 'sigint
;;   :on-output (lambda (&rest args)
;;                (let ((output (plist-get args :output))
;;                      (service (plist-get args :service)))
;;                  (cond ((s-matches? ": INFO/MainProcess] celery@[a-z-_]+ ready." output)
;;                         (prodigy-set-status service 'running))
;;                        ((s-matches? "Reloading celery worker..." output)
;;                         (prodigy-set-status service 'starting))
;;                        ((s-matches? "Traceback (most recent call last):" output)
;;                         (prodigy-set-status service 'failed)))))
;;   :kill-process-buffer-on-stop nil)
