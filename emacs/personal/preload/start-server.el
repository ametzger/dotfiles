;; suppress warning about unsafe directory when running
;; as windows admin -- I LIVE LIFE ON THE EDGE
(require 'server)
(when (and (>= emacs-major-version 23)
           (equal window-system 'w32))
  (defun server-ensure-safe-dir (dir) "Noop" t))

(server-start)
