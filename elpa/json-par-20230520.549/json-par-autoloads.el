;;; json-par-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "json-par" "json-par.el" (0 0 0 0))
;;; Generated autoloads from json-par.el

(let ((loads (get 'json-par 'custom-loads))) (if (member '"json-par" loads) nil (put 'json-par 'custom-loads (cons '"json-par" loads))))

(autoload 'json-par-mode "json-par" "\
Toggle minor mode for structural editing of JSON.

This is a minor mode.  If called interactively, toggle the
`Json-Par mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `json-par-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{json-par-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "json-par" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-ancestor-overlay" "json-par-ancestor-overlay.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from json-par-ancestor-overlay.el

(register-definition-prefixes "json-par-ancestor-overlay" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-clone" "json-par-clone.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from json-par-clone.el

(register-definition-prefixes "json-par-clone" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-delete" "json-par-delete.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from json-par-delete.el

(register-definition-prefixes "json-par-delete" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-fixup" "json-par-fixup.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from json-par-fixup.el

(register-definition-prefixes "json-par-fixup" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-guess" "json-par-guess.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from json-par-guess.el

(register-definition-prefixes "json-par-guess" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-indent" "json-par-indent.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from json-par-indent.el

(register-definition-prefixes "json-par-indent" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-insert" "json-par-insert.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from json-par-insert.el

(register-definition-prefixes "json-par-insert" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-keymap" "json-par-keymap.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from json-par-keymap.el

(register-definition-prefixes "json-par-keymap" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-lexer" "json-par-lexer.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from json-par-lexer.el

(register-definition-prefixes "json-par-lexer" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-mark-narrow" "json-par-mark-narrow.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from json-par-mark-narrow.el

(register-definition-prefixes "json-par-mark-narrow" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-motion" "json-par-motion.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from json-par-motion.el

(register-definition-prefixes "json-par-motion" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-oneline-multiline" "json-par-oneline-multiline.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from json-par-oneline-multiline.el

(register-definition-prefixes "json-par-oneline-multiline" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-raise" "json-par-raise.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from json-par-raise.el

(register-definition-prefixes "json-par-raise" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-split-join" "json-par-split-join.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from json-par-split-join.el

(register-definition-prefixes "json-par-split-join" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-transpose" "json-par-transpose.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from json-par-transpose.el

(register-definition-prefixes "json-par-transpose" '("json-par-"))

;;;***

;;;### (autoloads nil "json-par-utils" "json-par-utils.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from json-par-utils.el

(register-definition-prefixes "json-par-utils" '("json-par-"))

;;;***

;;;### (autoloads nil nil ("json-par-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; json-par-autoloads.el ends here
