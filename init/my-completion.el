;;; my-completion.el --- Set up code completion utilities.
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete configuration
;; TODO integrate with my-tab and my-minitab

(use-package yasnippet
  :disabled t
  :config
  (setq yas-verbosity 2)
  (yas-global-mode t)
  ;; Remove yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Distinguish C-i from TAB
  (define-key input-decode-map [?\C-i] [C-i])
  ;; Bind C-i to yasnippet
  (define-key yas-minor-mode-map (kbd "<C-i>") 'yas-expand)
  :pin melpa-stable)

;; Autocomplete configuration
(require 'auto-complete-config)
(ac-config-default)                     ; perhaps should be mode-specific
(setq ac-auto-start nil)                ; don't start automatically
(define-key ac-mode-map                 ; TAB starts autocompletion
  [tab] 'auto-complete)
;; (setq-default
;;  ac-sources
;;  '(ac-source-abbrev
;;    ac-source-dictionary
;;    ac-source-words-in-same-mode-buffers))

;; TODO move these hooks to mode-specific setup
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
(add-hook 'css-mode-hook 'ac-css-mode-setup)
(add-hook 'auto-complete-mode-hook 'ac-common-setup)
(global-auto-complete-mode t)


;; From https://github.com/purcell/emacs.d/blob/master/lisp/init-auto-complete.el
;; See http://stackoverflow.com/questions/7022898/emacs-autocompletion-in-emacs-lisp-mode
(require-package 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(setq-default ac-expand-on-auto-complete nil)
(setq-default ac-auto-start nil)
(setq-default ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed

;;----------------------------------------------------------------------------
;; Use Emacs' built-in TAB completion hooks to trigger AC (Emacs >= 23.2)
;;----------------------------------------------------------------------------
(setq tab-always-indent 'complete)  ;; use 't when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)
;; Stop completion-at-point from popping up completion buffers so eagerly
(setq completion-cycle-threshold 5)

;; TODO: find solution for php, haskell and other modes where TAB always does something

(setq c-tab-always-indent nil
      c-insert-tab-function 'indent-for-tab-command)

;; hook AC into completion-at-point
(defun sanityinc/auto-complete-at-point ()
  (when (and (not (minibufferp))
	     (fboundp 'auto-complete-mode)
	     auto-complete-mode)
    #'auto-complete))

(defun sanityinc/never-indent ()
  (set (make-local-variable 'indent-line-function) (lambda () 'noindent)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions
        (cons 'sanityinc/auto-complete-at-point
              (remove 'sanityinc/auto-complete-at-point completion-at-point-functions))))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)


(set-default 'ac-sources
             '(ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(log-edit-mode org-mode text-mode haml-mode
                git-commit-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode
                js3-mode css-mode less-css-mode sql-mode
                sql-interactive-mode
                inferior-emacs-lisp-mode))
  (add-to-list 'ac-modes mode))


;; Exclude very large buffers from dabbrev
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

(provide 'my-completion)
;;; my-completion.el ends here
