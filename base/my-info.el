;;; Set up local emacs info directory
;; Rod Price, August 2009


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Organize a local info directory with a hierarchy less haphazard
;; than the standard emacs info directory.

;; Customize the faces in info+.el to look decent with my color theme.

(add-to-list 'auto-mode-alist '("\\.info$" . texinfo-mode))
(add-to-list 'auto-mode-alist '("^dir$" . texinfo-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoloads and requires
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'info)
;; Fix faces before info+ becomes usable
;;(require 'info+)

(add-hook 'info-mode-hook 'turn-on-iimage-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (kbd `F3') Open the local user's info directory
(global-set-key [(f3)] 'open-user-info-directory)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set the local info paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The system-wide default infopath is set here.
(add-hook 'Info-mode-hook
          (lambda ()
            (setq Info-additional-directory-list
                  Info-default-directory-list)))

(defun open-user-info-directory ()
  "Open to the top level of the user's local info directory."
  (interactive)
  (info (expand-file-name "~/info/dir")))

;; Allow inline images in info mode.
;; (require ''iimage)
;; (add-hook 'Info-mode-hook 'turn-on-iimage-mode)

(provide 'my-info)
