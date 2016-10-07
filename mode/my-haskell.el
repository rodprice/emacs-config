;;; my-haskell.el -- Haskell and Elm modes
;;; Commentary:
;;; Code:

;; ________________________________________________________________________
;; Haskell mode

;; From https://github.com/hlian/emacs-d/blob/master/init-packages.el
(eval-after-load 'flycheck
  '(require 'flycheck-hdevtools))

(defun my-fix-imports ()
  "Clean up Haskell import lists"
  (interactive)
  (sort-lines nil (region-beginning) (region-end))
  (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)#-"))

(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :commands haskell-mode
  :bind ("C-c C-s" . my-fix-imports)
  :config
  (custom-set-variables
   '(haskell-ask-also-kill-buffers nil)
   '(haskell-process-type (quote stack-ghci))
   '(haskell-interactive-popup-errors nil))
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'before-save-hook 'haskell-mode-format-imports nil t)
  ; (add-hook 'haskell-mode-hook 'ghc-init)
  :pin melpa-stable)

(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-setup
  :pin melpa-stable)

(use-package ghc
  :ensure t
  :commands ghc-init ghc-debug
  :pin melpa-stable)

(use-package haskell-interactive-mode
  :ensure t
  :commands haskell-interactive-mode
  :pin melpa-stable)

;; ________________________________________________________________________
;; Elm mode

(defun my-elm-compile-buffer ()
    "Switch to the compile buffer after compiling."
  (interactive)
  (elm-compile-buffer)
  (other-window 1))

;; See https://github.com/jcollard/elm-mode#installation
(use-package elm-mode
  :ensure t
  :bind (:map elm-mode-map ("C-c C-c" . my-elm-compile-buffer))
  :init
  (setq elm-format-on-save nil            ; generate new TAGS file on save
        elm-tags-exclude-elm-stuff nil    ; include source files inside `elm-stuff' directories
        elm-sort-imports-on-save t        ; apply `elm-sort-imports' on save
        elm-format-elm-version "0.17"     ; version of Elm to format against
        elm-compile-arguments '("--yes" "--output=elm.js"))
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  :pin melpa-stable)


;; (add-hook 'elm-mode-hook
;;           (lambda ()
;;             (define-key elm-mode-map
;;               (kbd "C-c C-c") 'my-elm-compile-buffer)))


(provide 'my-haskell)
;;; my-haskell.el ends here
