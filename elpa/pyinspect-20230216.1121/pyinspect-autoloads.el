;;; pyinspect-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "pyinspect" "pyinspect.el" (0 0 0 0))
;;; Generated autoloads from pyinspect.el

(autoload 'pyinspect-goto-parent-object "pyinspect" "\
Inspect parent object of currently inspected object.
E.g. if we're inspecting `x.y.z', this function switches to buffer `x.y'.
If this objecet has no parent, quit all pyinspect buffers." t nil)

(autoload 'pyinspect-inspect-at-point "pyinspect" "\
Inspect symbol at point in `pyinspect-mode'." t nil)

(autoload 'pyinspect-kill-all-buffers "pyinspect" "\
Kill all pyinspect inspection buffers." t nil)

(autoload 'pyinspect-mode "pyinspect" "\


\(fn)" t nil)

(register-definition-prefixes "pyinspect" '("pyinspect-"))

;;;***

;;;### (autoloads nil nil ("pyinspect-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; pyinspect-autoloads.el ends here
