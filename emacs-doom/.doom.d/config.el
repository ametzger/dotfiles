;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(let ((font-size (if (eq system-type 'darwin)
                     18
                   13)))
  (setq doom-font (format "Operator Mono Medium %d" font-size)))

(custom-set-faces
 '(font-lock-comment-face ((t (:foreground "#6d7a96" :slant italic))))
 '(font-lock-doc-face ((t (:foreground "#6d7a96" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "#81A1C1" :slant italic)))))

(setq doom-theme 'doom-nord)

(setq default-directory "~/"
      avy-all-windows t)
