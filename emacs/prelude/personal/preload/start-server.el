(require 'server)


;; suppress warning about unsafe directory when running
;; as windows admin -- I LIVE LIFE ON THE EDGE

(when (eq system-type 'windows-nt)
  (when (and (>= emacs-major-version 23)
             (equal window-system 'w32))
    (defun server-ensure-safe-dir (dir) "Noop" t)))

(unless (server-running-p)
  (server-start))
