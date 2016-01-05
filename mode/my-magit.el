;;; my-magit.el --- Python mode and programming tools setup code

;; Created: 4 Jan 2016
;; Version: 0.1
;; Package-Requires: ((magit 2.3.1))
;;; Commentary:
;;; Code:

;; From http://whattheemacsd.com
(defadvice magit-status (around magit-fullscreen activate)
  "Make magit-status run alone in the frame, then restore the
previous window configuration when you exit magit."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
