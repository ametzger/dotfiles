{ config, lib, pkgs, ... }: {
  programs.emacs = {
    enable = true;

    package = with pkgs; pkgs.emacsPgtk.overrideAttrs (old: {
      patches =
        (old.patches or [])
        ++ [
          # Fix OS window role (needed for window managers like yabai)
          (fetchpatch {
            url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/bf76434954c977f0414d09ea9f9d8d92dc7611b0/patches/emacs-28/fix-window-role.patch";
            sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
          })
          # Use poll instead of select to get file descriptors
          (fetchpatch {
            url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/bf76434954c977f0414d09ea9f9d8d92dc7611b0/patches/emacs-29/poll.patch";
            sha256 = "sha256-jN9MlD8/ZrnLuP2/HUXXEVVd6A+aRZNYFdZF8ReJGfY=";
          })
          # Make Emacs aware of OS-level light/dark mode
          (fetchpatch {
            url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/bf76434954c977f0414d09ea9f9d8d92dc7611b0/patches/emacs-28/system-appearance.patch";
            sha256 = "sha256-oM6fXdXCWVcBnNrzXmF0ZMdp8j0pzkLE66WteeCutv8=";
          })
        ];
    });

    init = {
      enable = true;
      recommendedGcSettings = true;
      startupTimer = true;
      packageQuickstart = true;

      prelude = ''
        (setq-default user-full-name    "Alex Metzger"
                      user-mail-address "asm@asm.io"
                      user-login-name   "asm"
                      inhibit-startup-message t
                      inhibit-startup-echo-area-message (user-login-name)
                      custom-file                         (make-temp-file "")
                      line-spacing                        0
                      ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
                      gc-cons-threshold                   100000000
                      large-file-warning-threshold        50000000
                      read-process-output-max             (* 1024 1024)
                      save-interprogram-paste-before-kill t
                      ring-bell-function 'ignore
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
                      initial-major-mode 'text-mode
                      help-window-select t
                      indent-tabs-mode nil
                      tab-width 4
                      sh-basic-offset 2
                      fill-column 100
                      require-final-newline t
                      backup-directory-alist `((".*" . ,temporary-file-directory))
                      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
              )

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

        ;; indent on RET
        (global-set-key (kbd "RET") #'newline-and-indent)

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

        (line-number-mode t)
        (column-number-mode t)
        (size-indication-mode t)
        (delete-selection-mode t)
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

         (global-auto-revert-mode t)

         (set-charset-priority        'unicode)
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
         (global-set-key (kbd "M-s o") #'asm/occur-dwim)

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

        ;; disable version control, magit forever
        (remove-hook 'find-file-hook 'vc-find-file-hook)
        (setq vc-handled-backends ())
      '';

      usePackage = {
        abbrev = {
          enable = true;
          diminish = [ "abbrev-mode" ];
          command = [ "abbrev-mode" ];
        };

        doom-themes = {
          enable = true;
          init = ''
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
          '';
          config = ''
            (doom-themes-neotree-config)
            (doom-themes-org-config)
          '';
        };

        doom-modeline = {
          enable = true;
          hook = [ "(after-init . doom-modeline-mode)" ];
          init = ''
            (setq doom-modeline-python-executable (expand-file-name "~/.pyenv/shims/python")
                  doom-modeline-lsp nil
                  doom-modeline-mu4e nil
                  doom-modeline-irc nil
                  doom-modeline-env-version nil)
          '';
        };
        
        winner = {
          enable = true;
          diminish = [ "winner-mode" ];
          init = "(winner-mode)";
        };

        windmove = {
          enable = true;
          config = "(windmove-default-keybindings)";
        };

        nix-mode = {
          enable = true;
          mode = [ ''"\\.nix\\'"'' ];
          hook = [ "(nix-mode . subword-mode)" ];
        };
      };

    };

    # extraPackages = (epkgs:
    #   (with epkgs; [
    #     ace-window
    #     all-the-icons
    #     all-the-icons-dired
    #     anzu
    #     avy
    #     blacken
    #     browse-kill-ring
    #     cider
    #     clojure-mode
    #     company
    #     company-go
    #     company-jedi
    #     company-terraform
    #     counsel
    #     crux
    #     deadgrep
    #     direnv
    #     discover-my-major
    #     dockerfile-mode
    #     doom-modeline
    #     doom-themes
    #     dumb-jump
    #     dwim-shell-command
    #     easy-kill
    #     ein
    #     elixir-mode
    #     exec-path-from-shell
    #     expand-region
    #     flycheck-mypy
    #     flycheck-rust
    #     git-link
    #     git-timemachine
    #     go-eldoc
    #     go-guru
    #     go-mode
    #     go-projectile
    #     gorepl-mode
    #     hl-todo
    #     iedit
    #     imenu-anywhere
    #     imenu-list
    #     ivy
    #     ivy-prescient
    #     jedi
    #     jinja2-mode
    #     js2-mode
    #     json-mode
    #     just-mode
    #     lsp-ivy
    #     lsp-mode
    #     lsp-pyright
    #     lsp-ui
    #     magit
    #     markdown-mode
    #     move-text
    #     multiple-cursors
    #     neotree
    #     nix-mode
    #     org
    #     org-bullets
    #     persp-projectile
    #     perspective
    #     pipenv
    #     prescient
    #     projectile
    #     pt
    #     py-isort
    #     pyenv-mode
    #     racer
    #     rainbow-delimiters
    #     reformatter
    #     restclient
    #     rust-mode
    #     s
    #     sdlang-mode
    #     shackle
    #     smartparens
    #     swiper
    #     terraform-mode
    #     toml-mode
    #     typescript-mode
    #     undo-tree
    #     volatile-highlights
    #     vterm
    #     web-mode
    #     wgrep
    #     which-key
    #     yaml-mode
    #     yasnippet
    #     yasnippet-snippets
    #     zop-to-char
    #   ]));
  };
}
