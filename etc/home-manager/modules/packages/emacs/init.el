;; packaging
(require 'package)
(setq package-enable-at-startup nil
      load-prefer-newer t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; emacs 27+ will throw "Warning (package): Unnecessary call to
;; ‘package-initialize’ in init file" error if this is called, but
;; older versions require it.
(when (version< emacs-version "27.0")
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq-default use-package-enable-imenu-support t
              use-package-verbose nil)
(require 'use-package)

;; straight.el

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))


;; (use-package auto-package-update
;;   :config
;;   (setq-default auto-package-update-delete-old-versions t
;;                 auto-package-update-hide-results t)
;;   (auto-package-update-maybe))

;; (use-package paradox
;;   ;;   :config
;;   (paradox-enable))

;; make sure ASDF stuff gets picked up
(setenv "ASDF_DIR" (concat (getenv "HOME") "/.asdf"))
(let ((path (getenv "PATH")))
  (if (not (cl-search ".asdf/shims" path))
      (setenv "PATH" (concat (expand-file-name "~/.asdf/shims") ":" path))))
(setq exec-path (append exec-path (list (expand-file-name "~/.asdf/shims"))))

;; vanity
(setq user-full-name    "Alex Metzger"
      user-mail-address "asm@asm.io"
      user-login-name   "asm")

;; emacs baseline + annoyances
(setq custom-file                         (make-temp-file "")
      line-spacing                        0
      ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
      gc-cons-threshold                   100000000
      large-file-warning-threshold        50000000
      read-process-output-max             (* 1024 1024)
      save-interprogram-paste-before-kill t)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c k")
                (lambda ()
                  (interactive)
                  (kill-this-buffer)))
(global-set-key (kbd "C-c C-k")
                (lambda ()
                  (interactive)
                  (kill-this-buffer)
                  (asm/delete-windows-and-rebalance)))

(defconst asm/savefile-dir
  (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p asm/savefile-dir)
  (make-directory asm/savefile-dir))

;; Disable visual cruft
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(blink-cursor-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Disable menu bar on Linux and in terminal, enable on grapical OS X.
(defun asm/menubar-config (&optional frame)
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                       (if (and (display-graphic-p frame)
                                (memq window-system '(mac ns)))
                           1 0)))
(add-hook 'after-make-frame-functions 'asm/menubar-config)

(defun display-startup-echo-area-message ()
  (message "Howdy!"))

(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      initial-scratch-message (format "Welcome to Emacs %s (started %s, startup took %s)\n\n"
                                      emacs-version
                                      (current-time-string)
                                      (emacs-init-time))
      scroll-margin 3
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      auto-window-vscroll nil
      frame-resize-pixelwise t
      initial-major-mode 'text-mode)

;; indent on RET
(global-set-key (kbd "RET") #'newline-and-indent)

;; window management
(use-package winner
  :init
  (winner-mode))

(defun asm/window-max ()
  (interactive)
  (toggle-frame-maximized))

;; maximize the window when starting emacs
(add-hook 'after-init-hook #'asm/window-max)

(defun asm/split-window-vertically ()
  (interactive)
  (split-window-vertically)
  (balance-windows)
  (other-window 1))

(defun asm/split-window-horizontally ()
  (interactive)
  (split-window-horizontally)
  (balance-windows)
  (other-window 1))

(defun asm/delete-windows-and-rebalance ()
  (interactive)
  (unless (one-window-p)
    (delete-window)
    (balance-windows)))

(global-set-key (kbd "C-x 2") #'asm/split-window-vertically)
(global-set-key (kbd "C-x 3") #'asm/split-window-horizontally)
(global-set-key (kbd "C-x 0") #'asm/delete-windows-and-rebalance)

(defun asm/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c |") #'asm/toggle-window-split)

(setq help-window-select t)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)

;; Bind C-S-SPC to mark the whole line (similar to
;; `evil-visual-line')
(defun asm/select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position))
  (forward-line))
(global-set-key (kbd "C-S-SPC") #'asm/select-current-line)

;; font config
(let* ((font-candidates '("Operator Mono Medium"
                          "Go Mono"
                          "SF Mono"
                          "IBM Plex Mono Medium"
                          "Cascadia Code"))
       (font-name (seq-find #'x-list-fonts font-candidates nil))
       (font-size (if (eq system-type 'darwin) 18 13)))
  (set-frame-font (format "%s %d" font-name font-size) t t))

;; italics for comments, keywords. Prettiest in Operator Mono.
(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#6d7a96" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "#6d7a96" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "#81A1C1" :slant italic)))))

;; indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              sh-basic-offset 2
              fill-column 100)

(setq require-final-newline t)

(delete-selection-mode t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-auto-revert-mode t)

(set-charset-priority 'unicode)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)
      locale-coding-system   'utf-8)

(defun asm/occur-dwim ()
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (thing-at-point 'symbol))
        regexp-history)
  (call-interactively 'occur))
(bind-key "M-s o" #'asm/occur-dwim)

(defun asm/comment-sanely ()
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))
(global-set-key (kbd "M-;") #'asm/comment-sanely)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)

(global-set-key (kbd "C-x \\") #'align-regexp)

(defun asm/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-c b") #'asm/switch-to-previous-buffer)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq frame-title-format nil)

;; platform-specific
(if (eq system-type 'darwin)
    (progn
      (setq delete-by-moving-to-trash t
            trash-directory "~/.Trash")
      (put 'ns-print-buffer 'disabled t)
      (put 'suspend-frame 'disabled t)
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . dark))
      (setq-default ns-use-srgb-colorspace t
                    ns-use-proxy-icon nil)
      (global-set-key (kbd "s-n") #'make-frame-command)))

;;; built-in packages
;; disable version control, magit forever
(remove-hook 'find-file-hook 'vc-find-file-hook)
(setq vc-handled-backends ())

;; (use-package server
;;   :defer 2
;;   :init
;;   (server-mode t)
;;   :config
;;   (unless (server-running-p)
;;     (server-start)))

(use-package paren
  :config
  (show-paren-mode +1))

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package abbrev
  :diminish abbrev-mode
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep home clean
        savehist-file (expand-file-name "savehist" asm/savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" asm/savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package dired
  :bind
  ;; ("C-c C-j" . dired-jump)
  (:map dired-mode-map
        ("RET" . dired-find-alternate-file)
        ("^" . (lambda () (interactive) (find-alternate-file ".."))))

  :config
  ;; dired ls config, disabled for now. Using `ls-lisp' instead.
  ;; OS X uses BSD ls by default, `brew install coreutils` puts GNU ls
  ;; as gls. Use that for dired if it's present
  ;; (setq dired-listing-switches "-aBhl")
  ;; (let ((coreutils-ls-path (executable-find "gls")))
  ;;   (if (and (eq system-type 'darwin)
  ;;            coreutils-ls-path)
  ;;       (setq insert-directory-program coreutils-ls-path))))

  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (require 'ls-lisp)
  (setq ls-lisp-dirs-first t
        ls-lisp-use-insert-directory-program nil)

  (put 'dired-find-alternate-file 'disabled nil)

  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dired-dwim-target t))

(use-package dired-x
  :after (dired)
  :config
  (progn
    (setq dired-omit-verbose nil)
    ;; toggle `dired-omit-mode' with C-x M-o
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
          "^\\.?#\\|^.DS_STORE$\\|^.projectile$\\|^.git$\\|^.CFUserTextEncoding$\\|^.Trash$\\|^__pycache__$")))


(use-package dwim-shell-command
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  ;; :config
  ;; (defun my/dwim-shell-command-convert-to-gif ()
  ;;   "Convert all marked videos to optimized gif(s)."
  ;;   (interactive)
  ;;   (dwim-shell-command-on-marked-files
  ;;    "Convert to gif"
  ;;    "ffmpeg -loglevel quiet -stats -y -i <<f>> -pix_fmt rgb24 -r 15 <<fne>>.gif"
  ;;    :utils "ffmpeg"))
  )

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package ibuffer
  :config
  (setq ibuffer-default-sorting-mode 'major-mode))

(defun asm/org-mode-hook ()
  (auto-fill-mode t)
  (visual-line-mode t))

(defun asm/org-newline (arg)
  "Add a new item if we are in a list, otherwise indent to current level"
  (interactive "P")
  (if (and (not arg)
           (org-at-item-p))
      (org-insert-item)
    (org-return t)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind
  ;; ("C-c l" . org-store-link)
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  (:map org-mode-map
        ("C-a"   . crux-move-beginning-of-line)
        ("<RET>" . asm/org-newline))
  :config
  (progn
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((python     . t)
       (emacs-lisp . t)
       ))
    (setq org-directory "~/org"
          org-default-notes-file (concat org-directory "/inbox.org")
          org-agenda-files (mapcar
                            (lambda (path)
                              (concat org-directory "/" path))
                            '("inbox.org"))
          org-capture-templates `(
                                  ("i" "Inbox" entry  (file "inbox.org")
                                   ,(concat "* %?\n"
                                            "/Entered on/ %U")
                                   :prepend t)
                                  ("t" "Todo" entry  (file "inbox.org")
                                   "* TODO %?"
                                   :prepend t)
                                  )
          org-use-speed-commands t
          org-return-follows-link t
          org-confirm-babel-evaluate nil)
    (add-hook 'org-mode-hook #'asm/org-mode-hook)

    ;; support windmove keybinds without breaking heading shift
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)

    ;; Re-enable "<s" and other expansions after org 9.2
    (require 'org-tempo)))

(use-package org-bullets
  :after (org)
  :commands (org-bullets-mode)
  :hook (org-mode . org-bullets-mode))

;; (use-package org-journal
;;   ;;   :defer 2
;;   :init
;;   (defun asm/org-journal-done ()
;;     "Simple convenience function.
;;     Saves the buffer of the current day's entry and kills the window
;;     Similar to org-capture like behavior"
;;     (interactive)
;;     (save-buffer)
;;     (kill-buffer-and-window))
;;   :custom
;;   (org-journal-dir (concat (file-name-as-directory org-directory) "journal"))
;;   (org-journal-file-format "%Y/%m/%Y%m%d")
;;   (org-journal-date-format "%A, %Y-%m-%d")
;;   (org-journal-enable-agenda-integration t)
;;   (org-journal-hide-entries-p nil)
;;   (org-journal-time-format "%R
;;    ")
;;   :bind
;;   (("C-c C-j" . org-journal-new-entry)
;;    :map org-journal-mode-map
;;    ("C-c C-c" . asm/org-journal-done)))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

;; emacs tools
(global-set-key (kbd "C-M-.") #'xref-find-definitions-other-window)

;; (use-package helpful
;;   ;;   :commands (helpful-callable helpful-variable helpful-at-point helpful-command helpful-key)
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ("C-c C-d"                 . helpful-at-point)
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command]  . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key]      . helpful-key))

(use-package s)

;; theme, modeline
(use-package all-the-icons)

(use-package all-the-icons-dired
  :after (all-the-icons)
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package doom-themes
  :after (rainbow-delimiters)
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-neotree-file-icons t
        doom-nord-brighter-comments nil
        doom-nord-region-highlight 'frost
        doom-nord-padded-modeline t
        doom-solarized-light-brighter-comments nil
        doom-solarized-light-brighter-modeline nil
        doom-solarized-light-padded-modeline t)
  (let (
        ;; (active-theme 'doom-solarized-light)
        (active-theme 'doom-nord)
        )
    (load-theme active-theme t))
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-python-executable (expand-file-name "~/.pyenv/shims/python")
        doom-modeline-lsp nil
        doom-modeline-mu4e nil
        doom-modeline-irc nil
        doom-modeline-env-version nil))

;; usability
(use-package avy
  :bind (("s-."   . avy-goto-word-or-subword-1)
         ("s-,"   . avy-goto-char-timer)
         ("C-'"   . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1))
  :config
  (setq avy-background t))

;; (use-package editorconfig
;;   :disabled
;;   ;;   :hook
;;   ((mardown-mode . editorconfig-mode)
;;    (python-mode  . editorconfig-mode)
;;    (web-mode     . editorconfig-mode)
;;    (js2-mode     . editorconfig-mode)
;;    (sh-mode      . editorconfig-mode)
;;    (ruby-mode    . editorconfig-mode))
;;   :config
;;   (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-attr-indent-offset))
;;   (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-attr-value-indent-offset))
;;   (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-code-indent-offset))
;;   (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-css-indent-offset))
;;   (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-markup-indent-offset))
;;   (add-to-list 'editorconfig-indentation-alist '(web-mode web-mode-sql-indent-offset))
;;   (add-to-list 'editorconfig-indentation-alist '(web-mode js2-basic-offset))
;;   (add-to-list 'editorconfig-indentation-alist '(js-mode js-indent-level js2-basic-offset))
;;   (add-to-list 'editorconfig-indentation-alist '(js2-mode js-indent-level js2-basic-offset))
;;   (add-to-list 'editorconfig-indentation-alist '(js2-minor-mode js-indent-level js2-basic-offset))
;;   (add-to-list 'editorconfig-indentation-alist '(nginx-mode nginx-indent-level nginx-indent-level)))

(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)
  (setq magit-repository-directories '(("~/proj/" . 2)))

  ;; use full-screen magit
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (defadvice magit-mode-bury-buffer (after magit-restore-screen activate)
    (jump-to-register :magit-fullscreen))
  )

(use-package git-timemachine
  :bind (("s-g" . git-timemachine)))

(use-package git-link
  :defer t
  :commands (git-link)
  :bind (("C-c g" . git-link))
  :config
  (setq git-link-use-commit t))

(use-package direnv
  :config
  (setq direnv-always-show-summary nil)
  (direnv-mode))

(use-package ripgrep
  :if (executable-find "rg"))

(defun asm/deadgrep-project-root ()
  (if (projectile-project-p)
      (projectile-project-root)
    default-directory))

(use-package deadgrep
  :after (ripgrep projectile)
  :config
  (setq deadgrep-project-root-function #'asm/deadgrep-project-root))

(use-package wgrep)

(use-package pt)

(use-package dumb-jump
  :bind
  ("C-M-g" . dumb-jump-go)
  ("C-c j" . hydra-dumb-jump/body)
  :config
  (setq dumb-jump-selector 'ivy)
  (defhydra hydra-dumb-jump (:color blue
                                    :hint nil)
    "
[dumb-jump]        _j_ump        _b_ack        _p_review        _J_ump in other window        _q_uit        "

    ("j" dumb-jump-go)
    ("b" dumb-jump-back)
    ("p" dumb-jump-quick-look)
    ("J" dumb-jump-go-other-window)
    ("q" nil)))

(use-package projectile
  :diminish projectile-mode
  :after (ivy)
  :init
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map))
  (:map projectile-command-map
        ("F" . projectile-find-file-other-window))
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package browse-kill-ring
  :bind
  ("C-M-y" . browse-kill-ring))

(use-package multiple-cursors
  :bind
  ("C-;"           . mc/mark-all-like-this-dwim)
  ("C->"           . mc/mark-next-like-this)
  ("C-<"           . mc/mark-previous-like-this)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click)
  :config
  ;; unmap return when in multi-cursor
  (define-key mc/keymap (kbd "<return>") nil))

(use-package iedit
  :bind (("C-c ;" . iedit-mode)))

(use-package anzu
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char)
         ("s-z" . (lambda () (interactive) (zop-up-to-char -1)))))

;; imenu
;; recenter around selected imenu items
(defun asm/imenu-select-hook ()
  (recenter scroll-margin))
(add-hook 'imenu-after-jump-hook 'asm/imenu-select-hook)
;; always be scanning
(setq imenu-auto-rescan t
      imenu-auto-rescan-maxout (* 1024 1024)
      imenu--rescan-item '("" . -99))

(use-package imenu-anywhere
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package imenu-list
  :defer t
  :functions (imenu-list-smart-toggle)
  :bind (("C-." . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t))

;; general code-related
;; semantic mode is slow, disable it
(semantic-mode -1)

(use-package hl-todo
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package flycheck
  :after pyenv-mode
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook (lambda ()
                               (flymake-mode -1)
                               (global-flycheck-mode)))
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0.2
        company-show-numbers t
        company-tooltip-limit 10
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-global-modes '(not org-mode
                                   text-mode
                                   fundamental-mode
                                   ein:notebook-mode))
  (global-company-mode t))

(defun asm/yas-comment-start ()
  "A properly spaced comment for yasnippet snips"
  (s-trim comment-start))

;; (use-package yasnippet
;;   :diminish yas-minor-mode
;;   :config
;;   (progn
;;     (setq yas-prompt-functions '(yas-ido-prompt
;;                                  yas-completing-prompt))
;;     (yas-reload-all)
;;     (yas-global-mode)
;;     (defhydra hydra-yas (:color blue
;;                                 :hint nil)
;;       "
;; [yasnippet]        _i_nsert        _n_ew        _v_isit snippet file        _r_eload all        e_x_pand        _?_ list snippets        "
;;       ("i" yas-insert-snippet)
;;       ("n" yas-new-snippet)
;;       ("v" yas-visit-snippet-file)
;;       ("r" yas-reload-all)
;;       ("x" yas-expand)
;;       ("?" yas-describe-tables)
;;       ("q" nil "cancel" :color blue))
;;     (global-set-key (kbd "C-c y") #'hydra-yas/body)
;;     (advice-add 'company-complete-common :before
;;                 (lambda () (setq my-company-point (point))))
;;     (advice-add 'company-complete-common :after
;;                 (lambda ()
;;                   (when (equal my-company-point (point))
;;                     (yas-expand))))))

;; (use-package yasnippet-snippets
;;   :after (yasnippet))

(use-package crux
  :bind (("C-c d"         . crux-duplicate-current-line-or-region)
         ("M-o"           . crux-smart-open-line)
         ("C-c n"         . crux-cleanup-buffer-or-region)
         ("C-c f"         . crux-recentf-find-file)
         ("C-M-z"         . crux-indent-defun)
         ("C-c e"         . crux-eval-and-replace)
         ("C-c w"         . crux-swap-windows)
         ("C-c D"         . crux-delete-file-and-buffer)
         ("C-c r"         . crux-rename-buffer-and-file)
         ("C-c TAB"       . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I"         . crux-find-user-init-file)
         ("s-r"           . crux-recentf-find-file)
         ("s-j"           . crux-top-join-line)
         ("C-^"           . crux-top-join-line)
         ("s-k"           . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o"           . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)))

(use-package which-key
  :diminish which-key-mode
  :custom
  (which-key-sort-order #'which-key-prefix-then-key-order)
  :config
  (setq which-key-idle-delay 0.4)
  (which-key-mode +1))

(use-package discover-my-major
  :bind (("C-h M-m" . discover-my-major)
         ("C-h M-M" . discover-my-mode)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-set-key (kbd "C-/") #'undo-tree-undo)
  (global-set-key (kbd "C-?") #'undo-tree-redo)
  (global-set-key (kbd "C-c u") #'undo-tree-visualize)
  (global-undo-tree-mode))

(use-package prescient)

(use-package ivy-prescient
  :after (prescient ivy)
  :config
  (ivy-prescient-mode))

(defun asm/ivy-sort-by-length (_name candidates)
  (cl-sort (copy-sequence candidates)
           (lambda (f1 f2)
             (< (length f1) (length f2)))))

(use-package ivy
  :diminish ivy-mode
  :demand
  :config
  (setq ivy-count-format ""
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-initial-inputs-alist nil
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-sort-matches-functions-alist '((t)
                                           (counsel-find-file . asm/ivy-sort-by-length)
                                           (projectile-completing-read . asm/ivy-sort-by-length)
                                           )
        ivy-on-del-error-function #'ignore
        ivy-use-selectable-prompt t
        ivy-format-function 'ivy-format-function-arrow)
  (set-face-attribute 'ivy-current-match nil :foreground "#242832")
  (ivy-mode 1)
  :bind
  ("C-c C-r" . ivy-resume))

(use-package swiper
  :bind
  ("C-s"   . swiper)
  ("C-r"   . swiper)
  ("C-S-s" . isearch-forward)
  ("C-S-r" . isearch-backwards))

(defun asm/contextual-switch-buffer ()
  "Switch to projectile buffers if in a counsel project,
  otherwise do a normal `counsel-switch-buffer'."
  (interactive)
  (if (projectile-project-p)
      (counsel-projectile-switch-to-buffer)
    (counsel-switch-buffer)))

(use-package counsel
  :bind
  (("M-x"     . counsel-M-x)
   ("C-x b"   . asm/contextual-switch-buffer)
   ;; ("C-x b"   . counsel-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-c i"   . counsel-imenu)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)
   :map counsel-find-file-map
   ("C-l" . ivy-backward-delete-char)))

(use-package ace-window
  :config
  (global-set-key (kbd "s-w") #'ace-window)
  (global-set-key [remap other-window] #'ace-window))

(use-package neotree
  :defer t
  :commands (neotree-toggle)
  :bind (("C-c t" . neotree-toggle))
  :init
  (progn
    (setq neo-smart-open t
          neo-dont-be-alone t)
    (add-hook 'neotree-mode-hook
              (lambda ()
                (setq-local mode-line-format nil)
                (local-set-key (kbd "C-s") #'isearch-forward)
                (local-set-key (kbd "C-r") #'isearch-backward)))))

                                        ; TODO: this is poorly configured, it's annoying to fight it when a
                                        ; buffer accidentally crosses project boundaries.
(use-package perspective
  :init
  (setq persp-suppress-no-prefix-key-warning t)
  :config
  (persp-mode)

  ;; switch to perspective when exiting ibuffer
  ;; (defun asm/persp-ibuffer-visit-buffer ()
  ;;   (interactive)
  ;;   (let ((buf (ibuffer-current-buffer t))
  ;;         (persp-name (string-remove-prefix "Projectile:" (get-text-property
  ;;                      (line-beginning-position) 'ibuffer-filter-group))))
  ;;     (persp-switch persp-name)
  ;;     (switch-to-buffer buf)))

  ;; (define-key ibuffer-mode-map (kbd "RET") 'asm/persp-ibuffer-visit-buffer)
  )

(use-package persp-projectile
  :after (perspective)
  :bind
  ("C-c x" . hydra-persp/body)
  :config
  (defhydra hydra-persp (:columns 4
                                  :color blue)
    "Perspective"
    ("a" persp-add-buffer "Add Buffer")
    ("i" persp-import "Import")
    ("c" persp-kill "Close")
    ("n" persp-next "Next")
    ("p" persp-prev "Prev")
    ("k" persp-remove-buffer "Kill Buffer")
    ("r" persp-rename "Rename")
    ("A" persp-set-buffer "Set Buffer")
    ("s" persp-switch "Switch")
    ("C-x" persp-switch-last "Switch Last")
    ("b" persp-switch-to-buffer "Switch to Buffer")
    ("P" projectile-persp-switch-project "Switch Project")
    ("q" nil "Quit")))

;; see https://gist.github.com/rksm/8c07d9ccc9e15adf752d3dd73dd9a61e
(defun rk/open-compilation-buffer (&optional buffer-or-name shackle-alist shackle-plist)
  "Helper for selecting window for opening *compilation* buffers."
  ;; find existing compilation window left of the current window or left-most window
  (let ((win (or (loop for win = (if win (window-left win) (get-buffer-window))
                       when (or (not (window-left win))
                                (string-prefix-p "*compilation" (buffer-name (window-buffer win))))
                       return win)
                 (get-buffer-window))))
    ;; if the window is dedicated to a non-compilation buffer, use the current one instead
    (when (window-dedicated-p win)
      (let ((buf-name (buffer-name (window-buffer win))))
        (unless (string-prefix-p "*compilation" buf-name)
          (setq win (get-buffer-window)))))
    (set-window-buffer win (get-buffer buffer-or-name))
    (set-frame-selected-window (window-frame win) win)))

(use-package shackle
  :diminish
  :custom
  (shackle-rules
   '(
     (compilation-mode :custom rk/open-compilation-buffer :select t)
     ("\\*Apropos\\|Help\\|Occur\\|tide-references\\*" :regexp t :same t :select t :inhibit-window-quit t)
     ("\\*magit" :regexp t :same t :select t)
     ("\\*shell.*" :regexp t :same t :select t)
     ("\\*PowerShell.*" :regexp t :same t :select t)
     ("\\*Cargo.*" :regexp t :other t :select nil)
     ("*Messages*" :select nil :other t)
     ("*go-guru-output*" :select t :same t)
     ("*Proced*" :select t :same t)
     ("*Buffer List*" :select t :same t)
     ("\\*Pp Eval" :regexp t :same nil :select t :other t)
     ("*Messages*" :same nil :other t :select t :inhibit-window-quit t)

     ;; slime
     ("*slime-source*" :select nil :same nil :other t)
     ("*slime-description*" :select nil :other t :inhibit-window-quit t)
     ("\\*slime-repl" :regexp t :same nil :select nil :other t)
     ;; ("\\*sldb" :regexp t :other t :inhibit-window-quit t :select t)
     ("\\*slime-compilation" :regexp t :same nil :select nil :other t)
     ("*slime-scratch*" :same nil :select t :other t)

     ;; ert
     ("*ert*" :select nil :same nil :other t)

     ;; clojure
     ("*sesman CIDER browser*" :inhibit-window-quit t :select t :same t)
     ("\\*cider-repl" :regexp t :same nil :other t)
     ))
  (shackle-default-rule nil)
  :config
  (shackle-mode))

;; (use-package zygospore
;;   :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode +1)

  (defadvice kill-region (before smart-cut activate compile)
    (interactive
     (if mark-active (list (region-beginning) (region-end))
       (list (line-beginning-position)
             (line-beginning-position 2))))))

(use-package rainbow-delimiters)

(use-package whitespace
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 100) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing)))

(use-package smartparens
  :diminish smartparens-mode
  :init
  (setq sp-highlight-pair-overlay nil)
  (smartparens-global-mode t)
  (require 'smartparens-config)
  :config
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-pair "'" nil :unless '(sp-point-after-word-p))
  :bind
  (("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-a" . sp-backward-down-sexp)
   ("C-S-d" . sp-beginning-of-sexp)
   ("C-S-a" . sp-end-of-sexp)
   ("C-M-e" . sp-up-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s"   . sp-splice-sexp)
   ("M-r"   . sp-splice-sexp-killing-around)
   ("C-)"   . sp-forward-slurp-sexp)
   ("C-}"   . sp-forward-barf-sexp)
   ("C-("   . sp-backward-slurp-sexp)
   ("C-{"   . sp-backward-barf-sexp)
   ("M-S"   . sp-split-sexp)
   ("M-J"   . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

;; elisp
(use-package emacs-lisp
  :mode ("\\.el'" . emacs-lisp-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-c" . eval-region)))

;; ruby
(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook #'subword-mode))

;; markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

;; tree-sitter
                                        ; TODO(asm,2023-02-21): make sure this is backwards-compatible until Fedora + homebrew is compatible with 29
(use-package treesit
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css "https://github.com/tree-sitter/tree-sitter-css")
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (python "https://github.com/tree-sitter/tree-sitter-python")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping '((python-mode . python-ts-mode)
                     (css-mode . css-ts-mode)
                     (typescript-mode . tsx-ts-mode)
                     (js-mode . js-ts-mode)
                     (css-mode . css-ts-mode)
                     (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars)
  ;; Do not forget to customize Combobulate to your liking:
  ;;
  ;;  M-x customize-group RET combobulate RET
  ;;
  (use-package combobulate
    ;; Optional, but recommended.
    ;;
    ;; You can manually enable Combobulate with `M-x
    ;; combobulate-mode'.
    :hook ((python-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode)
           (css-ts-mode . combobulate-mode)
           (yaml-ts-mode . combobulate-mode)
           (typescript-ts-mode . combobulate-mode)
           (tsx-ts-mode . combobulate-mode))
    ;; Amend this to the directory where you keep Combobulate's source
    ;; code.
    :load-path ("~/proj/vendor/combobulate"))
  )
;; TODO(asm,2022-10-25): lsp-mode is kind of heavy and more opinionated than I would like, it also
;; adds a lot of UI frills that I find unecessary. eglot seems to be more in line with my "I just
;; want xref and imenu to pull from LSP" philosophy, but it doesn't seem to integrate with xref
;; properly when set up this way. I don't have time to hack on it now, but eventually it would be
;; nice to slim down LSP integration.
;; (use-package eglot
;;   ;;   :hook
;;   (python-mode . eglot-ensure)
;;   :config
;;     (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio"))))

;; lsp-mode
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :bind (:map lsp-mode-map
              ("C-S-SPC" . nil))
  :hook
  (typescript-mode . lsp-deferred)
  (javascript-mode . lsp-deferred)
  (js2-mode        . lsp-deferred)
  (lsp-mode        . lsp-enable-which-key-integration)
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; :config
  ;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  )

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; python
(use-package pyenv-mode
  :config
  (add-to-list 'exec-path "~/.pyenv/shims")
  (add-hook 'python-mode-hook 'pyenv-mode)
  (setq-default flycheck-python-pycompile-executable (expand-file-name
                                                      "~/.pyenv/shims/python")
                flycheck-python-flake8-executable (expand-file-name
                                                   "~/.pyenv/shims/flake8")
                flycheck-python-mypy-executable (expand-file-name
                                                 "~/.pyenv/shims/mypy")
                flycheck-flake8rc ".flake8"))

;; (use-package pyenv-mode-auto)

(use-package pipenv)

(defun asm/python-mode-hook ()
  ;; use flat imenu
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (subword-mode +1)
  (setq indent-tabs-mode nil)
  ;; (set (make-local-variable 'company-backends)
  ;;      '(company-jedi
  ;;        company-anaconda))
  )

                                        ; NOTE(asm,2022-08-02): there are some changes that need to be made to `python-mode' to be able to
                                        ; support python 3.10 syntax that can't be made here. To do them, run `M-x find-library RET
                                        ; python-mode' and make the following changes:
                                        ; 1. Add "match" and "case" to the `python-rx' macro:
;;   (defmacro python-rx (&rest regexps)
;;     "Python mode specialized rx macro.
;;   This variant of `rx' supports common Python named REGEXPS."
;;     `(rx-let ((block-start       (seq symbol-start
;;                                       (or "def" "class" "if" "elif" "else" "try"
;;                                           "except" "finally" "for" "while" "with"
;;                                           ; HACK(asm,2022-08-02): added python 3.10 syntax
;;                                           "match" "case"
                                        ; 2. Add "match" and "case" to `python-font-lock-keywords-level-2':
;;   (defvar python-font-lock-keywords-level-2
;;     `(,@python-font-lock-keywords-level-1
;;       ,(rx symbol-start
;;            (or
;;             "and" "del" "from" "not" "while" "as" "elif" "global" "or" "with"
;;             "assert" "else" "if" "pass" "yield" "break" "except" "import" "class"
;;             "in" "raise" "continue" "finally" "is" "return" "def" "for" "lambda"
;;             "try"
;;             ; HACK(asm,2022-08-02): python 3.10 stuff
;;             "match" "case"
                                        ; Hopefully these will be integrated upstream soon, but they are not present in emacs 28.
(use-package python
  :mode ("\\.py'" . python-mode)
  :interpreter ("python" . python-mode)
  :bind (:map python-mode-map
              ("C-c C-j" . nil))
  :config
  (setq-default python-fill-docstring-style 'django)
  (add-hook 'python-mode-hook 'asm/python-mode-hook))

(use-package jedi)

(use-package company-jedi
  :after (company jedi)
  :config
  (setq-default company-jedi-python-bin "~/.pyenv/shims/python"))

;; (use-package anaconda-mode
;;   ;;   :config
;;   (add-hook 'python-mode-hook 'anaconda-mode))

;; (use-package company-anaconda
;;   ;;   :after (company anaconda-mode))

(use-package blacken
  ;; :hook
  ;; ((python-mode . blacken-mode))
  :config
  (setq blacken-executable "${pkgs.black}/bin/black")
  (define-key python-mode-map (kbd "C-c C-b") 'blacken-buffer))

(use-package py-isort
  :config
  (setq py-isort-options '("-l 100" "--multi-line=3" "--trailing-comma")))

(defun asm/toggle-isort ()
  "Toggle isort before-save-hook."
  (interactive)
  (if (member 'py-isort-before-save before-save-hook)
      (progn
        (remove-hook 'before-save-hook 'py-isort-before-save)
        (message "isort disabled"))
    (progn
      (add-hook 'before-save-hook 'py-isort-before-save)
      (message "isort enabled"))))

(use-package flycheck-mypy)

                                        ; TODO(asm,2022-05-11): Convert this to use straight.el to make this
                                        ; more reproducible
(use-package ein
  :commands (ein:login)
  :init
  (setq ein:complete-on-dot -1
        ein:completion-backend 'ein:use-none-backend
        ein:query-timeout 1000
        ein:default-url-or-port "http://localhost:8888"
        ein:worksheet-enable-undo 'full
        ein:notebook-modes '(ein:notebook-python-mode ein:notebook-plain-mode))
  :config
  (cond
   ((eq system-type 'darwin)
    (setq-default ein:console-args
                  '("--gui=osx" "--matplotlib=osx" "--colors=Linux")))
   ((eq system-type 'gnu/linux)
    (setq-default ein:console-args
                  '("--gui=gtk3" "--matplotlib=gtk3" "--colors=Linux"))))

  ;; Not sure if this is necessary - seems to make ein work on OS X.
  (setq-default request--curl-cookie-jar (concat user-emacs-directory
                                                 "request/curl-cookie-jar"))

  (add-hook 'ein:notebook-mode-hook
            (lambda ()
              (visual-line-mode +1)
              (whitespace-mode -1)
              (company-mode nil)
              (flycheck-mode nil)
              (undo-tree-mode t)
              ;; (bind-key "C-/" 'undo-tree-undo)
              (bind-key "C-a" 'crux-move-beginning-of-line)
              )))

;; elixir
(use-package elixir-mode)

(use-package reformatter
  :config
                                        ; Adds a reformatter configuration called "+elixir-format"
                                        ; This uses "mix format -"
  (reformatter-define +elixir-format
    :program "mix"
    :args '("format" "-"))
                                        ; defines a function that looks for the .formatter.exs file used by mix format
  (defun +set-default-directory-to-mix-project-root (original-fun &rest args)
    (if-let* ((mix-project-root (and buffer-file-name
                                     (locate-dominating-file buffer-file-name
                                                             ".formatter.exs"))))
        (let ((default-directory mix-project-root))
          (apply original-fun args))
      (apply original-fun args)))
                                        ; adds an advice to the generated function +elxir-format-region that sets the proper root dir
                                        ; mix format needs to be run from the root directory otherwise it wont use the formatter configuration
  (advice-add '+elixir-format-region :around #'+set-default-directory-to-mix-project-root)
                                        ; Adds a hook to the major-mode that will add the generated function +elixir-format-on-save-mode
                                        ; So, every time we save an elixir file it will try to find a .formatter.exs and then run mix format from
                                        ; that file's directory
  (add-hook 'elixir-mode-hook #'+elixir-format-on-save-mode))

;; golang
(use-package go-projectile)

(use-package go-mode
  :defer t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (add-hook 'go-mode-hook 'electric-pair-mode)
  (defun asm/go-mode-hook ()
    ;; call gofmt before saving
    (add-hook 'before-save-hook 'gofmt-before-save)
    (add-to-list 'exec-path "~/proj/go/bin")
    ;; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go vet"))
    (local-set-key (kbd "C-c C-c") 'compile)
    (set (make-local-variable 'company-backends) '(company-go))

    (setenv "GOPATH" (expand-file-name "~/proj/go")))

  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'asm/go-mode-hook))

(use-package company-go
  :after go
  :config
  (setq tab-width 4)
  (setq company-go-gocode-command (expand-file-name "~/proj/go/bin/gocode"))
  ;; (setq company-go-insert-arguments -1)
  (setq company-go-show-annotation t)
  :bind (:map go-mode-map
              ("M-." . godef-jump)))

(use-package go-eldoc
  :after go
  :hook
  (go-mode . go-eldoc-setup))

(use-package go-guru
  :after go
  :hook
  (go-mode . go-guru-hl-identifier-mode))

(use-package gorepl-mode
  :after go
  :hook
  (go-mode . gorepl-mode))

;; rust
(use-package rust-mode
  :defer t
  :mode "\\.rs$"
  :config
  (setq rust-format-on-save t))

(use-package racer
  :after rust-mode
  :config
  (setq racer-rust-src-path (expand-file-name "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/library")
        racer-cmd (expand-file-name "~/.cargo/bin/racer")))

(use-package flycheck-rust
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'electric-pair-mode)

;; misc languages
(use-package json-mode
  :defer t
  :mode "\\.json$"
  :bind
  (:map json-mode-map
        ("C-c C-b" . json-pretty-print-buffer)
        ("C-c C-j" . counsel-jq))
  :init
  (setq js-indent-level 2))

(use-package yaml-mode
  :defer t
  :mode "\\.yaml$")

(use-package toml-mode
  :defer t
  :mode (("\\.toml$" . toml-mode)
         ("Pipfile$" . toml-mode)))

(use-package sdlang-mode
  :defer t
  :mode (("\\.kdl$" . sdlang-mode)
         ("\\.sdl$" . sdlang-mode)))

(use-package nix-mode
  :defer t
  :mode "\\.nix\\'")

(use-package web-mode
  :defer t
  :mode "\\.html$"
  :config
  (setq-default web-mode-markup-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-css-indent-offset 2
                web-mode-sql-indent-offset 2
                web-mode-enable-auto-indentation nil)
  (add-hook 'web-mode-hook (lambda () (web-mode-set-engine "django"))))

(use-package js2-mode
  :defer t
  :mode "\\.js$"
  :config
  (setq-default js2-basic-indent 4
                js2-basic-offset 4
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "setTimeout" "clearTimeout" "setInterval"
                                         "clearInterval" "location" "console" "JSON"
                                         "jQuery" "$"))

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

(use-package typescript-mode
  :config
  (setq-default typescript-indent-level 4)

  :mode (("\\.ts\\'" . typescript-mode))
  :mode (("\\.tsx\\'" . typescript-mode)))

;; (defun asm/setup-tide-mode ()
;;   (interactive)
;;   (defun tide-imenu-index () nil)
;;   (tide-setup)
;;   (tide-hl-identifier-mode +1))

;; (use-package tide
;;   ;;   :disabled ; NOTE(asm,2021-05-25): this seems to throw a lot of
;;             ; errors, disabling for now
;;   :config
;;   (progn
;;     (add-hook 'typescript-mode-hook #'asm/setup-tide-mode)
;;     (add-hook 'js-mode-hook #'asm/setup-tide-mode)
;;     (add-hook 'js2-mode-hook #'asm/setup-tide-mode)
;;     (add-hook 'rjsx-mode-hook #'asm/setup-tide-mode)))

(use-package terraform-mode
  :mode "\\.tf$"
  :hook
  (terraform-mode . company-mode)
  ;; :config
  ;; (set-face-foreground terraform--resource-name-face "#B58DAE")
  ;; (set-face-foreground terraform--resource-type-face "#B58DAE")
  )

(use-package company-terraform)

(defun asm/terraform-mode-hook ()
  (subword-mode +1)
  (terraform-format-on-save-mode t)
  (set (make-local-variable 'company-backends)
       '(company-terraform)))
(add-hook 'terraform-mode-hook #'asm/terraform-mode-hook)

;; ; TODO(asm,2022-05-11): hack to work around transient + docker dependencies
;; (use-package transient
;;   :straight (transient :type git
;;                        :host github
;;                        :repo "magit/transient"
;;                        :ref "6fc09a663e408ade0d1b88f47701c96a9b051e34"))

(use-package vterm)

;; (use-package docker
;;   ;;   :config
;;   (setq docker-show-messages nil
;;         docker-show-status nil)
;;   :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :defer t)

(use-package just-mode)

(use-package jinja2-mode
  :mode ((".*\\.jinja" . jinja2-mode)
         (".*\\.jinja2" . jinja2-mode)))

(use-package clojure-mode)

(use-package cider)

(use-package restclient
  :defer t
  :commands (restclient-mode)
  :mode ("\\.\\(http\\|rest\\)$" . restclient-mode))

(defun asm/open-init-file ()
  (interactive)
  ;; (projectile-persp-switch-project "~/proj/emacs.d")
  (find-file (expand-file-name "~/proj/emacs.d/init.el")))

(defun asm/empty-buffer ()
  (interactive)
  (command-execute 'asm/split-window-horizontally)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save nil)
    buf))

(defun asm/org-open-file ()
  (interactive)
  (let ((file-to-open
         (read-file-name
          "Open org file: "
          (expand-file-name "~/org/"))))
    (find-file file-to-open)))

(defun asm/yank-filename ()
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (replace-regexp-in-string ".*/proj/" "" (buffer-file-name)))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun asm/toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun asm/point-in-string-p (pt)
  "Returns t if PT is in a string"
  (eq 'string (syntax-ppss-context (syntax-ppss pt))))

(defun asm/beginning-of-string ()
  "Moves to the beginning of a syntactic string"
  (interactive)
  (unless (asm/point-in-string-p (point))
    (error "You must be in a string for this command to work"))
  (while (asm/point-in-string-p (point))
    (forward-char -1))
  (point))

(defun asm/swap-quotes ()
  "Swaps the quote symbols around a string"
  (interactive)
  (save-excursion
    (let ((bos (save-excursion
                 (asm/beginning-of-string)))
          (eos (save-excursion
                 (asm/beginning-of-string)
                 (forward-sexp)
                 (point)))
          (replacement-char ?\'))
      (goto-char bos)
      ;; if the following character is a single quote then the
      ;; `replacement-char' should be a double quote.
      (when (eq (following-char) ?\')
        (setq replacement-char ?\"))
      (delete-char 1)
      (insert replacement-char)
      (goto-char eos)
      (delete-char -1)
      (insert replacement-char))))

(global-set-key
 (kbd "C-z")
 (defhydra ctrl-z-hydra (:color blue
                                :columns 4)
   "Shorties"
   ("b" asm/empty-buffer "empty buffer")
   ("e" flycheck-list-errors "list errors")
   ("f" asm/yank-filename "yank filename")
   ("i" asm/open-init-file "open init")
   ("l" counsel-bookmark "bookmarks")
   ("n" ein:login "EIN")
   ("o" asm/org-open-file "find org file")
   ("p" projectile-persp-switch-project "open project")
   ("r" anzu-replace-at-cursor-thing "replace at point")
   ("s" counsel-rg "ripgrep")
   ("w" ace-window "ace window")
   ("C-s" deadgrep "deadgrep")
   ("q" asm/swap-quotes "toggle quotes around string")
   ("z" asm/toggle-maximize-buffer "zoom")))

;;; init.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars noruntime)
;; End:
