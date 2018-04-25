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
(prelude-require-packages '(visual-regexp
                            solarized-theme
                            csharp-mode
                            json-mode
                            hlinum
                            ag
                            powerline
                            neotree
                            dash-at-point
                            avy
                            ace-window
                            pyenv-mode
                            helm-swoop
                            multiple-cursors))

;; custom file extension mappings
(add-to-list 'auto-mode-alist '("\\.cake\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-to-list 'auto-mode-alist '("\\.linq\\'" . csharp-mode))
(add-to-list 'auto-mode-alist '("\\.tt\\'" . csharp-mode))

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
(setq linum-format "%4d \u2502")
(require 'linum-off)
(global-linum-mode t)
;; (hlinum-activate)

;; org-mode
;; when org-mode starts, expand all nodes
(setq org-startup-folded t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   (ruby . t)))
(setq org-confirm-babel-evaluate nil)
(setq org-default-notes-file "~/org/main.org")

(global-set-key (kbd "s-t") (lambda ()
                              (interactive)
                              (find-file org-default-notes-file)))

;; configure Input Mono font with fallbacks
(defun asm/setup-font ()
  (let ((font-face (cond
                    ((find-font (font-spec :name "Roboto Mono Medium for Powerline"))
                     "Roboto Mono Medium for Powerline")
                    ((find-font (font-spec :name "Operator Mono"))
                     "Operator Mono")
                    ((find-font (font-spec :name "Input"))
                     "Input")
                    ((find-font (font-spec :name "Input Mono"))
                     "Input Mono")
                    ((find-font (font-spec :name "Monaco"))
                     "Monaco")
                    ((find-font (font-spec :name "Consolas"))
                     "Consolas")))
        (font-size (if (eq system-type 'darwin) "16" "10")))

    (if (not (eq system-type 'gnu/linux))
        (set-frame-font (concat font-face "-" font-size) t t)))) ;; keep size, font for new frames

(asm/setup-font)

;; simple title
(setq frame-title-format '(""
                           "emacs :: "
                           (:eval (if (buffer-file-name)
                                      (abbreviate-file-name (buffer-file-name))
                                    "%b"))))

;; start new frames default maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq indent-tabs-mode nil)
(setq tab-width 2)
(setq standard-indent 2)
(setq js-indent-level 2)

;; map left super key on Winders
(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super))

;; remap super => control on mac
;; (if (eq system-type 'darwin)
;;     (setq mac-command-modifier 'control))

;; live regex search
(define-key global-map (kbd "C-c C-r") 'vr/replace)
(define-key global-map (kbd "C-c C-q") 'vr/query-replace)
(define-key global-map (kbd "C-c C-t") 'vr/mc-mark)

;; disable whitespace mode
;; (setq prelude-whitespace nil)


;; disable scrach message on startup
(setq initial-scratch-message nil)

;; use powerline
(powerline-default-theme)

(require 'prelude-helm-everywhere)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(global-set-key [f8] 'neotree-toggle)

(setq mouse-wheel-scroll-amount '(1))
                                        ; (setq mouse-wheel-progressive-speed nil)

;(setq nord-uniform-mode-lines t)
(setq nord-region-highlight "frost")
(setq nord-comment-brightness 15)

(setq confirm-kill-emacs 'y-or-n-p)

;; auto-switch to new panes
(global-set-key "\C-x2" (lambda ()
                          (interactive)
                          (split-window-vertically)
                          (other-window 1)))
(global-set-key "\C-x3" (lambda ()
                          (interactive)
                          (split-window-horizontally)
                          (other-window 1)))
;; auto-switch to new help panes
(setq help-window-select t)

(show-paren-mode 1)

;; yasnippet
;; (setq yas-snippet-dirs
;;       '("~/.emacs.d/snippets"                 ;; personal snippets
;;         "~/proj/vendor/yasnippet-django"))
;; (yas-global-mode)

;; use RipGrep w prelude
(setq projectile-enable-caching t)

;; https://github.com/BurntSushi/ripgrep
(when (executable-find "rg")
  (progn
    (defconst asm/rg-arguments
      `("--line-number"                     ; line numbers
        "--smart-case"
        "--follow"                          ; follow symlinks
        "--mmap")                           ; apply memory map optimization when possible
      "Default rg arguments used in the functions in `projectile' package.")

    (defun asm/advice-projectile-use-rg ()
      "Always use `rg' for getting a list of all files in the project."
      (mapconcat 'identity
                 (append '("\\rg") ; used unaliased version of `rg': \rg
                         asm/rg-arguments
                         '("--null" ; output null separated results,
                           "--files")) ; get file names matching the regex '' (all files)
                 " "))

    (advice-add 'projectile-get-ext-command :override #'asm/advice-projectile-use-rg)
    (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
    (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))))

(scroll-bar-mode -1)

;; django stuff
;; (require 'djangonaut)
;; (global-djangonaut-mode)
;; (setq python-shell-extra-pythonpaths '("~/proj/jellyfish"))
(setq python-shell-process-environment '("DJANGO_SETTINGS_MODULE=jellyfish.settings.dev"))
(setenv "DJANGO_SETTINGS_MODULE" "jellyfish.settings.dev")

;; disable ein auto-completion
(setq ein:use-auto-complete nil)

(require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-horizontally)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)

;; (helm-migemo-mode 1)
;; If you would like to use migemo, enable helm's migemo feature

;; (setq desktop-dirname             "~/tmp/emacs-desktop/"
;;       desktop-base-file-name      "emacs.desktop"
;;       desktop-base-lock-name      "lock"
;;       desktop-path                (list desktop-dirname)
;;       desktop-save                t
;;       desktop-files-not-to-save   "^$" ;reload tramp paths
;;       desktop-load-locked-desktop nil
;;       desktop-auto-save-timeout   30)
;; (desktop-save-mode 1)


(avy-setup-default)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "s-l") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)
(global-set-key (kbd "C-c C-j") 'avy-resume)


;; disable background in terminal emacs
;; TODO this don't work
(defun on-frame-open (frame)
  (progn
    (unless (display-graphic-p frame)
      (set-face-background 'default "unspecified-bg" frame))))
(add-hook 'after-make-frame-functions 'on-frame-open)

(defun on-after-init ()
  (progn
    (unless (display-graphic-p (selected-frame))
      (set-face-background 'default "unspecified-bg" (selected-frame)))))
(add-hook 'window-setup-hook 'on-after-init)



;; (require 'pyenv-mode)
;; (pyenv-mode)
(require 'pipenv)
(pipenv-mode)
(add-hook 'python-mode-hook #'pipenv-mode)

;; projectile
(setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (eq project "jellyfish")
        (pyenv-mode-set "jellyfish-3.4.3"))

    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)

;; diminishment
(require 'diminish)
(diminish 'editorconfig-mode)
(diminish 'guru-global-mode)
(diminish 'guru-mode)
(diminish 'which-key-mode)
(diminish 'auto-revert-mode)
(diminish 'helm-mode)
(diminish 'company-mode)
(diminish 'pipenv-mode)
(diminish 'anaconda-mode)
(diminish 'subword-mode)
(diminish 'smartparens-mode)
(diminish 'whitespace-mode)
(diminish 'eldoc-mode)
(diminish 'prelude-mode)
(diminish 'beacon-mode)

(global-set-key (kbd "s-d") 'dash-at-point)
(add-to-list 'dash-at-point-mode-alist '(python-mode . "asmdj"))

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

(global-set-key (kbd "C-x |") 'asm/toggle-window-split)

(setq vc-follow-symlinks t)

;; multi-cursors
(require 'multiple-cursors)
;(define-key mc/keymap (kbd "<return>") nil)
(global-set-key (kbd "C-M-S-s-l") 'mc/edit-lines)
(global-set-key (kbd "H-l") 'mc/edit-lines)
(global-set-key (kbd "C-;") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

; bind M-click to add multi-cursor
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)

(global-set-key (kbd "C-S-z") 'zap-up-to-char)
(global-set-key (kbd "s-z") 'zap-up-to-char)
(global-unset-key (kbd "C-z"))

;; Unbind Command-N new-frame
;(global-set-key (kbd "s-n") nil)

;; C-return in isearch selects current match
(defun asm/isearch-exit-mark-match ()
  "Exit isearch and mark the current match."
  (interactive)
  (isearch-exit)
  (push-mark isearch-other-end)
  (activate-mark))

(define-key isearch-mode-map (kbd "<C-return>") #'asm/isearch-exit-mark-match)

;; tramp
(setq tramp-default-method "ssh")

(defun asm/cleanup-tramp-everything ()
  (interactive)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections)
  (message "Cleaned up remote buffers and connections"))

(global-set-key (kbd "C-c q") 'asm/cleanup-tramp-everything)

;; smartparens
(require 'smartparens)
(sp-pair "(" ")" :unless '(sp-point-before-word-p))
(sp-pair "'" "'" :unless '(sp-point-before-word-p))
(sp-pair "[" "]" :unless '(sp-point-before-word-p))
(sp-pair "{" "}" :unless '(sp-point-before-word-p))
