;;; my-nxml.el --- nxml mode tweaks for working with XML files

;; Created: 10 Feb 2016
;; Version: 0.1
;; Package-Requires: ()

;;; Commentary:
;;; Code:

;; From http://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs
(defun nxml-pretty-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region
     (point-min) (point-max)
     "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region 0 (count-lines (point-min) (point-max)))))

(provide 'my-nxml)
;;; my-nxml.el ends here
