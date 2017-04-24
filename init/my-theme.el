;;; my-theme.el --- Defaults for the emacs UI
;;; Commentary:
;;; Code:


;; The default font is set in the site-specific preload file
(unless (boundp 'my-default-font)
  (defvar my-default-font "Courier-10"))

;; Specify the default font
(add-to-list 'default-frame-alist `(font . ,my-default-font))
(set-face-font 'default my-default-font)

;; Display the column number in the mode line
(setq column-number-mode t)

;; Truncate long lines everywhere
(global-visual-line-mode 0)
(setq-default truncate-lines t)


(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-tomorrow-night t)
  (let ((base00 (plist-get base16-tomorrow-night-colors :base00))
        (base01 (plist-get base16-tomorrow-night-colors :base01))
        (base02 (plist-get base16-tomorrow-night-colors :base02))
        (base03 (plist-get base16-tomorrow-night-colors :base03))
        (base04 (plist-get base16-tomorrow-night-colors :base04))
        (base05 (plist-get base16-tomorrow-night-colors :base05))
        (base06 (plist-get base16-tomorrow-night-colors :base06))
        (base07 (plist-get base16-tomorrow-night-colors :base07))
        (base08 (plist-get base16-tomorrow-night-colors :base08))
        (base09 (plist-get base16-tomorrow-night-colors :base09))
        (base0A (plist-get base16-tomorrow-night-colors :base0A))
        (base0B (plist-get base16-tomorrow-night-colors :base0B))
        (base0C (plist-get base16-tomorrow-night-colors :base0C))
        (base0D (plist-get base16-tomorrow-night-colors :base0D))
        (base0E (plist-get base16-tomorrow-night-colors :base0E))
        (base0F (plist-get base16-tomorrow-night-colors :base0F)))
    (setq face-remapping-alist
          `((show-paren-match
             . (:foreground ,base0D
                :background ,base02))
            (show-paren-mismatch
             . (:strike-through t
                :foreground ,base09
                :background ,base02))
            (font-lock-keyword-face
             . (:foreground ,base0E
                :weight bold))
            (font-lock-function-name-face
             . (:foreground "cornflower blue"))
            (font-lock-doc-face
             . (:foreground ,base0C))
            (font-lock-string-face
             . (:foreground "slate gray"))
            (font-lock-comment-face
             . (:foreground "light slate gray"))
            (font-lock-comment-delimiter-face
             . (:foreground "light slate gray")))))
  :pin melpa-stable)


(provide 'my-theme)
