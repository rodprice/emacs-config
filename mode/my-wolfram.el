;;; my-wolfram.el --- Customizations for Mathematica code
;;; Commentary:
;;; Code:

(require 'my-functions)

(require 'use-package)
(use-package wolfram-mode
  :ensure t
  :config
  (setq wolfram-program
        (shell-quote-argument  ; make path OS-independent 
         (expand-file-name "math.exe" my-mathematica-kernel-dir)))
  (setq wolfram-program-arguments
        (list
         "-pwfile "
         (shell-quote-argument  ; make path OS-independent 
          (expand-file-name "mathpass" my-mathematica-license-dir))))
  ;; Remove binding for .m files to objc-mode; bind to wolfram-mode
  (setq auto-mode-alist
        (cons '("\\.m\\'" . wolfram-mode)
              (remove-all-matches-from-alist ".m" auto-mode-alist)))
  (setq wolfram-indent 4)
  ;; TODO fix wolfram-mode-map
  :pin local)

;; To set up flycheck mode I need a syntax checker. Perhaps
;; `http://mathematica.stackexchange.com/questions/24176/mathematica-command-for-type-checking'
;; is a start?

;; Auto-pair comment brackets --- solution doesn't work
;; (sp-local-pair '(wolfram-mode) "(*" "*)"
;;                :unless '(sp-in-string-p)
;;                :actions '(insert wrap))

;; A configuration for using git version control with Mathematica
;; development is given here:
;; http://mathematica.stackexchange.com/questions/5789/how-to-setup-team-development-for-a-mathematica-project/39691#39691

(add-hook 'wolfram-mode-hook
          (lambda ()
            (turn-off-smartparens-mode)
            (turn-off-show-smartparens-mode)))

(provide 'my-wolfram)
;;; my-wolfram.el ends here
