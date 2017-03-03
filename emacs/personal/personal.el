;;; Alex Metzger emacs config
;;; for use with bbatsov's Prelude
;;; https://github.com/bbatsov/prelude

;; disable auto save
(setq auto-save-default nil)
;; disable guru (warnings when arrow keys are used)
(setq prelude-guru nil)
;; disable emacs lisp linting
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq prelude-flyspell nil)

;; initialize packages
(prelude-require-packages '(visual-regexp solarized-theme csharp-mode json-mode hlinum ag powerline))

;; custom file extension mappings
(add-to-list 'auto-mode-alist '("\\.cake\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-to-list 'auto-mode-alist '("\\.linq\\'" . csharp-mode))

;; add line numbers
;; based on https://www.emacswiki.org/emacs/LineNumbers

;; click on a line to select
;; (eval-after-load 'linum
;;   '(progn
;;      (defvar *linum-mdown-line* nil)

;;      (defun line-at-click ()
;;        (save-excursion
;;          (let ((click-y (cdr (cdr (mouse-position))))
;;                (line-move-visual-store line-move-visual))
;;            (setq line-move-visual t)
;;            (goto-char (window-start))
;;            (next-line (1- click-y))
;;            (setq line-move-visual line-move-visual-store)
;;            ;;; If you are using tabbar substitute the next line with
;;            ;;; (line-number-at-pos))))
;;            (1+ (line-number-at-pos)))))

;;      (defun md-select-linum ()
;;        (interactive)
;;        (goto-line (line-at-click))
;;        (set-mark (point))
;;        (setq *linum-mdown-line*
;;              (line-number-at-pos)))

;;      (defun mu-select-linum ()
;;        (interactive)
;;        (when *linum-mdown-line*
;;          (let (mu-line)
;;            ;;; (goto-line (line-at-click))
;;            (setq mu-line (line-at-click))
;;            (goto-line (max *linum-mdown-line* mu-line))
;;            (set-mark (line-end-position))
;;            (goto-line (min *linum-mdown-line* mu-line))
;;            (setq *linum-mdown*
;;                  nil))))

;;      (global-set-key (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
;;      (global-set-key (kbd "<left-margin> <mouse-1>") 'mu-select-linum)
;;      (global-set-key (kbd "<left-margin> <drag-mouse-1>") 'mu-select-linum)))

;; ;; include a line between the numbers and the buffer
;; (setq linum-format "%4d \u2502")
;; (global-linum-mode t)
;; (hlinum-activate)

;; when org-mode starts, expand all nodes
(setq org-startup-folded nil)

;; configure Input Mono font with fallbacks
(let (font-face font-size)
  (setq font-face (cond
                   ((find-font (font-spec :name "Input"))
                    "Input")
                   ((find-font (font-spec :name "Input Mono"))
                    "Input Mono")
                   ((find-font (font-spec :name "Monaco"))
                    "Monaco")
                   ((find-font (font-spec :name "Consolas"))
                    "Consolas")))
  (setq font-size (if (eq system-type 'darwin) "12" "10"))
  (if
      (not (eq system-type 'gnu/linux))
      (set-frame-font (concat font-face "-" font-size))))

;; duplicate current line
(defun duplicate-current-line (&optional n)
  "duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
    	  (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
    	(insert "\n"))
      
      ;; now insert as many time as requested
      (while (> n 0)
    	(insert current-line)
    	(decf n)))))

(global-set-key (kbd "\C-c\C-k") 'duplicate-current-line)

;; simple title
(setq frame-title-format '(""
                           "emacs - "
                           (:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "%b"))))

;; 4 spaces for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq standard-indent 4)

;; json indent to four spaces
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 4)))

;; map left super key on Winders
(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super))

;; remap super => control on mac
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'control))

;; live regex search
(define-key global-map (kbd "C-c C-r") 'vr/replace)
(define-key global-map (kbd "C-c C-q") 'vr/query-replace)
(define-key global-map (kbd "C-c C-t") 'vr/mc-mark)

;; disable whitespace mode
(setq prelude-whitespace nil)


;; disable scrach message on startup
(setq initial-scratch-message nil)

;; use powerline
(powerline-default-theme)
