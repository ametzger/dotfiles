;;; Alex Metzger emacs config
;;; for use with bbatsov's Prelude
;;; https://github.com/bbatsov/prelude

; disable auto save
(setq auto-save-default nil)
; disable guru (warnings when arrow keys are used)
(setq prelude-guru nil)
; disable emacs lisp linting
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
(setq prelude-flyspell nil)

; initialize packages
(prelude-require-package 'solarized-theme)
(prelude-require-package 'csharp-mode)
(prelude-require-package 'hlinum)
(prelude-require-package 'ag)

; custom file extension mappings
(add-to-list 'auto-mode-alist '("\\.cake\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . shell-script-mode))

; add line numbers
; based on https://www.emacswiki.org/emacs/LineNumbers

; click on a line to select
(eval-after-load 'linum
  '(progn
     (defvar *linum-mdown-line* nil)

     (defun line-at-click ()
       (save-excursion
         (let ((click-y (cdr (cdr (mouse-position))))
               (line-move-visual-store line-move-visual))
           (setq line-move-visual t)
           (goto-char (window-start))
           (next-line (1- click-y))
           (setq line-move-visual line-move-visual-store)
           ;; If you are using tabbar substitute the next line with
           ;; (line-number-at-pos))))
           (1+ (line-number-at-pos)))))

     (defun md-select-linum ()
       (interactive)
       (goto-line (line-at-click))
       (set-mark (point))
       (setq *linum-mdown-line*
             (line-number-at-pos)))

     (defun mu-select-linum ()
       (interactive)
       (when *linum-mdown-line*
         (let (mu-line)
           ;; (goto-line (line-at-click))
           (setq mu-line (line-at-click))
           (goto-line (max *linum-mdown-line* mu-line))
           (set-mark (line-end-position))
           (goto-line (min *linum-mdown-line* mu-line))
           (setq *linum-mdown*
                 nil))))

     (global-set-key (kbd "<left-margin> <down-mouse-1>") 'md-select-linum)
     (global-set-key (kbd "<left-margin> <mouse-1>") 'mu-select-linum)
     (global-set-key (kbd "<left-margin> <drag-mouse-1>") 'mu-select-linum)))

; include a line between the numbers and the buffer
(setq linum-format "%4d \u2502")
(global-linum-mode t)
(hlinum-activate)

; remap super => control on mac
(if (eq system-type 'darwin)
    (setq mac-command-modifier 'control))

; when org-mode starts, expand all nodes
(setq org-startup-folded nil)

; configure Input Mono font with fallbacks
(cond
 ((find-font (font-spec :name "Input"))
  (set-frame-font "Input-12"))
 ((find-font (font-spec :name "Input Mono"))
  (set-frame-font "Input Mono-12"))
 ((find-font (font-spec :name "Monaco"))
  (set-frame-font "Monaco-12"))
 ((find-font (font-spec :name "Consolas"))
  (set-frame-font "Consolas-12")))

; C-c C-k to pull current line to kill ring
(defun copy-line (&optional arg)
  "Do a kill-line but copy rather than kill.  This function directly calls
    kill-line, so see documentation of kill-line for how to use it including prefix
    argument and relevant variables.  This function works by temporarily making the
    buffer read-only."
  (interactive "P")
  (let ((buffer-read-only t)
        (kill-read-only-ok t))
    (kill-line arg)))

(global-set-key (kbd "\C-c\C-k") 'copy-line)
