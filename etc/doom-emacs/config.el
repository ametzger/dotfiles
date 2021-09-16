;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(let ((font-size (if (eq system-type 'darwin)
                     18
                   13)))
  (setq doom-font (format "Operator Mono Medium %d" font-size)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "#6d7a96" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "#6d7a96" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "#81A1C1" :slant italic)))))

(setq doom-theme 'doom-nord)

(setq default-directory "~/"
      avy-all-windows t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((flycheck-flake8rc . "/Users/asm/proj/jellyfish/.flake8"))))
