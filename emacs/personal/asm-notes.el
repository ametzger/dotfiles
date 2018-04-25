;;; asm-notes.el --- Convenience functions for manipulating org mode notes

;; Author: Alex Metzger <asm@asm.io>

;; (defvar notes-default-directory (expand-file-name "~/notes"))

;; (defun asm-notes-create-filename (dirty-slug)
;;   (let ((file-slug (if (= (length dirty-slug) 0)
;;                        ""
;;                      (concat
;;                       "-"
;;                       (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" dirty-slug))))))
;;     (concat (format-time-string "%Y-%m-%d") file-slug ".org")))

;; (defun asm-notes-open-file (file-name)
;;   (find-file (concat (file-name-as-directory notes-default-directory) file-name))
;;   (org-mode))

;; (defun asm-notes-create (file-slug)
;;   (interactive "sEnter slug: ")
;;   (let ((note-file-name (asm-notes-create-filename file-slug)))
;;     (asm-notes-open-file note-file-name)))

;; (defun asm-notes-open-todo ()
;;   (interactive)
;;   (asm-notes-open-file "todo.org"))

;; (defun asm-notes-open-debris ()
;;   (interactive)
;;   (asm-notes-open-file "debris.org"))

;; (define-key global-map (kbd "C-c M-n") 'asm-notes-create)
;; (define-key global-map (kbd "C-c M-t") 'asm-notes-open-todo)
;; (define-key global-map (kbd "C-c M-d") 'asm-notes-open-debris)
