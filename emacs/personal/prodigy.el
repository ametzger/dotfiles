;; Prodigy config

(prelude-require-package 'prodigy)

(prodigy-define-service
  :name "runserver"
  :command "pipenv"
  :args '("run" "python" "manage.py" "runserver")
  :cwd "~/proj/jellyfish"
  :path '("~/proj/jellyfish")
  :port 8000
  :tags '(work python)
  :stop-signal 'sigint
  :ready-message "Quit the server with CONTROL-C."
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
                        (prodigy-set-status service 'ready))
                       ((s-matches? "Reloading celery worker..." output)
                        (prodigy-set-status service 'stopping)))))
  :kill-process-buffer-on-stop nil)

;; (let (token (cdr ein-token-matches))
;;   ;; If we see the notebook's token, set up ein
;;   (ein:notebooklist-login "8888" token)
;;   (prodigy-set-status service 'ready))

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
                 (cond (jupyter-matches (let ((jupyter-port (cadr jupyter-matches))
                                              (jupyter-token (caddr jupyter-matches)))
                                          (prodigy-set-status service 'ready)
                                          (ein:notebooklist-login jupyter-port jupyter-token)))
                       ((s-matches? "The Jupyter Notebook is running at:" output)
                        (prodigy-set-status service 'ready))
                       ((s-matches? "received signal [0-9]+, stopping" output)
                        (prodigy-set-status service 'stopping)))))
  :kill-process-buffer-on-stop nil)
