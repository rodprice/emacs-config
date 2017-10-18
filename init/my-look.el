;;; my-look.el --- Defaults for the emacs UI
;;; Commentary:
;;; Code:


(defvar my-default-font "Courier-12"
  "The font family used by default throughout Emacs.")

;; These are the same values that a bare-naked emacs uses on initial startup
(defvar my-initial-geometry '(nil 80 38 100 100)
  "The initial frame geometry to use when no geometry file is present.")

(defvar graphene-geometry-file
  (expand-file-name ".graphene-geometry" user-emacs-directory)
  "The file where frame geometry settings are saved.")

(defun graphene-load-frame-geometry ()
  "Load saved frame geometry settings."
  (let ((geometry
	 (if (file-readable-p graphene-geometry-file)
	     (with-temp-buffer
	       (insert-file-contents graphene-geometry-file)
	       (read (buffer-string)))
	   graphene-initial-geometry)))
    (if	(equal (length geometry) 4)
	 (progn
	   (message
	    "Old-style .graphene-geometry file ignored; default geometry used")
	   graphene-initial-geometry)
	geometry)))

(defun graphene-get-geometry ()
  "Get the current geometry of the active frame."
  (mapcar
   (apply-partially 'frame-parameter nil)
   (list 'fullscreen 'width 'height 'top 'left)))

(defun graphene-save-frame-geometry ()
  "Save current frame geometry settings."
  (with-temp-file graphene-geometry-file
    (print (graphene-get-geometry) (current-buffer))))

(defun graphene-set-geometry ()
  "Set the default frame geometry using the values loaded from graphene-geometry-file."
  (let ((geom (graphene-load-frame-geometry)))
    (setq default-frame-alist
	  (append
	   default-frame-alist
	   `((fullscreen . ,(nth 0 geom))
	     (width . ,(nth 1 geom))
	     (height . ,(nth 2 geom))
	     (top . ,(nth 3 geom))
	     (left . ,(nth 4 geom)))))))

(defun graphene-look-startup-after-init ()
  "Load defaults for the overall Graphene look -- to be called
after loading the init file so as to pick up custom settings."
  (if window-system
      (progn
        (graphene-set-geometry)
        (add-hook 'kill-emacs-hook 'graphene-save-frame-geometry)
        (setq-default line-spacing 2)
        (graphene-set-fonts)
        (add-to-list 'default-frame-alist `(font . ,graphene-default-font))
        (set-face-font 'default graphene-default-font)
        (set-face-font 'variable-pitch graphene-variable-pitch-font)
        (set-face-font 'fixed-pitch graphene-fixed-pitch-font)
        (add-to-list 'default-frame-alist '(internal-border-width . 0))
        (set-fringe-mode '(8 . 0))
        (require 'graphene-theme)
        (load-theme 'graphene t)
        (defadvice load-theme
          (after load-graphene-theme (theme &optional no-confirm no-enable) activate)
          "Load the graphene theme extensions after loading a theme."
          (when (not (equal theme 'graphene))
            (load-theme 'graphene t))))
    (when (not (eq system-type 'darwin))
      (menu-bar-mode -1))
    ;; Menu bar always off in text mode
    (menu-bar-mode -1)))

(add-hook 'after-init-hook 'graphene-look-startup-after-init)


(provide 'my-look.el)
