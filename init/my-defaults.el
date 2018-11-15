;;; my-defaults.el --- Stuff that wasn't in better-defaults.el
;;; Commentary:
;;; Code:


(setq inhibit-startup-message t) ;; hide the startup message
(global-linum-mode t) ;; enable line numbers globally

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows

;; Control the window in which Emacs visits a new file
(setq ido-default-file-method 'raise-frame)

;; Don't pop up window of completions
(setq ido-cannot-complete-command 'ido-next-match)

;; When opening a help window, always select the new help window
(setq help-window-select t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Files

;; Backup file configuration
;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files?rq=1
(setq delete-old-versions t
      kept-new-versions 2
      kept-old-versions 2
      version-control t)

;; File locations for custom settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (custom-save-all))                   ; Create new, empty custom file
(load custom-file)

;; See https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Texinfo-documentation.html
(require 'info)
(info-initialize)                      ; populate Info-directory-list


(provide 'my-defaults)
