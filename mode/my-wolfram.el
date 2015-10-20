;;; my-wolfram.el --- Customizations for Mathematica code
;;; Commentary:
;;; Code:

;; Auto-pair comment brackets
(sp-local-pair '(wolfram-mode) "(*" "*)"
               :unless '(sp-in-string-p)
               :actions '(insert wrap))

(provide 'my-wolfram)
;;; my-wolfram.el ends here
