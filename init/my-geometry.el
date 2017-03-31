;;; my-geometry.el --- Saves and restores Emacs frame size and position
;;; Commentary:
;;; Code:



;; These are the same values that a bare-naked emacs uses on initial startup
(defvar my-initial-geometry '(nil 80 38 100 100)
  "The initial frame geometry to use when no geometry file is present.")


(defvar my-geometry-file
  (expand-file-name ".geometry" user-emacs-directory)
  "The file where frame geometry settings are saved.")


(defun my-load-frame-geometry ()
  "Load saved frame geometry settings."
  (if (file-readable-p my-geometry-file)
      (with-temp-buffer
        (insert-file-contents my-geometry-file)
        (read (buffer-string)))
    my-initial-geometry))


(defun my-get-geometry ()
  "Get the current geometry of the active frame."
  (mapcar
   (apply-partially 'frame-parameter nil)
   (list 'fullscreen 'width 'height 'top 'left)))


(defun my-save-frame-geometry ()
  "Save current frame geometry settings."
  (with-temp-file my-geometry-file
    (print (my-get-geometry) (current-buffer))))


(defun my-set-geometry ()
  "Set the default frame geometry using the values loaded from my-geometry-file."
  (let ((geom (my-load-frame-geometry)))
    (setq default-frame-alist
	  (append
	   default-frame-alist
	   `((fullscreen . ,(nth 0 geom))
	     (width . ,(nth 1 geom))
	     (height . ,(nth 2 geom))
	     (top . ,(nth 3 geom))
	     (left . ,(nth 4 geom)))))))


;; Returns (top left bottom right) measured in pixels
(defun my-get-geometry-pixels ()
  "Get the current geometry, measured in pixels, of the active frame."
  (let ((width (frame-pixel-width))
	(height (frame-pixel-height))
	(top (eval (frame-parameter nil 'top)))
	(left (eval (frame-parameter nil 'left))))
    (list top left (+ top width) (+ left height))))

(defun my-get-margins-pixels ()
  "Return the margins remaining on each side of the frame, measured in pixels."
  (let ((frame-size (my-get-geometry-pixels))
	(display-size (cdr (assq 'workarea (frame-monitor-attributes)))))
    (list
     (- (nth 0 frame-size) (nth 0 display-size))      ; left side of the frame
     (- (nth 1 frame-size) (nth 1 display-size))      ; top side of the frame
     (- (nth 2 display-size) (nth 2 frame-size))      ; right side of the frame
     (- (nth 3 display-size) (nth 3 frame-size)))))   ; bottom side of the frame


(defun my-geometry-startup-after-init ()
  "Called after loading the init file."
  (if (display-graphic-p)
      (progn
        (my-set-geometry)
        (add-hook 'kill-emacs-hook 'my-save-frame-geometry))))
        ;; (add-to-list 'default-frame-alist '(internal-border-width . 0))
        ;; (set-fringe-mode '(8 . 0)))
    ;; Menu bar always off in text mode
    ;; (menu-bar-mode -1))))

(add-hook 'after-init-hook 'my-geometry-startup-after-init)


(provide 'my-geometry)
