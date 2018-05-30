;; Prodigy config

(prelude-require-package 'prodigy)

(prodigy-define-service
  :name "Django Runserver"
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
  :name "Django Celery Worker"
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
