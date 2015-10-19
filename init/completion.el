
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete configuration
;; TODO integrate with my-tab and my-minitab

;; yasnippet configuration
(require 'yasnippet)
(setq yas-verbosity 2)
(yas-global-mode t)
;; Remove yasnippet's default tab key binding
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
;; Distinguish C-i from TAB
(define-key input-decode-map [?\C-i] [C-i])
;; Bind C-i to yasnippet
(define-key yas-minor-mode-map (kbd "<C-i>") 'yas-expand)

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

