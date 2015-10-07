;;; Wolfram --- Customizations for Mathematica code
;;; Commentary:
;; Customization and setup for wolfram-mode
;;; Code:

;; Auto-pair comment brackets
(sp-local-pair '(wolfram-mode) "(*" "*)"
               :unless '(sp-in-string-p)
               :actions '(insert wrap))

(provide 'wolfram)
;;; wolfram.el ends here
