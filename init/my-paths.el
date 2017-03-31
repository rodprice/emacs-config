;;; my-paths.el
;;; Commentary:

;; Declare a few paths to external applications used by Emacs. These
;; paths are specific to each site (meaning machine and operating
;; system), and so should be set in the site/*-preload.el file. The
;; paths stored in the path variables will be prepended to `exec-path'
;; in the order that they are defined below. See the site/README.md
;; file for details.

;;; Code:


(defvar my-path-variables nil
  "A list of path variables to be prepended to $PATH and `exec-path'.")

;; Python
(defvar my-anaconda-dir nil
  "The path to the top directory of my Python Anaconda distribution.")
(add-to-list 'my-path-variables 'my-anaconda-dir)
(defvar my-anaconda-scripts-dir nil
  "The path to the scripts directory of my Python Anaconda distribution.")
(add-to-list 'my-path-variables 'my-anaconda-scripts-dir)

;; Mathematica
(defvar my-mathematica-kernel-dir nil
  "The path to the Kernel/Binaries directory of my Mathematica installation.")
(add-to-list 'my-path-variables 'my-mathematica-kernel-dir)
(defvar my-mathematica-license-dir nil
  "The path to the $BaseDirectory/Licensing directory of my Mathematica installation.")

;; Windows-only
(if (eq system-type "windows-nt")
    (progn
      (defvar my-git-for-windows-dir nil
        "The path to the top directory of my git installation.")
      (add-to-list 'my-path-variables 'my-git-for-windows-dir)
      (defvar my-msys-binaries-dir nil
        "The path to the bin directory of my MSYS installation.")
      (add-to-list 'my-path-variables 'my-msys-binaries-dir)))


(defun my-normalize-path (path)
  "Return a string representing a normalized version of PATH.
This function is intended for use with Windows paths, which are
case-insensitive.  As such, it will downcase all names if emacs
is running on Windows; otherwise it will leave the case as is."
  (let ((normed (expand-file-name "" path)))
    (if (eq system-type "windows-nt")
        (downcase normed)
      normed)))


(defun my-remove-and-prepend-paths (ps1 ps2)
  "Prepend list of paths PS1 in reverse order to list of paths PS2.
Any elements of PS1 initially present in PS2 are removed."
  (if ps1
      (my-remove-and-prepend-paths
       (cdr ps1)
       (cons (car ps1) (remove (car ps1) ps2)))
    ps2))


(defun my-concat-paths (ps1 ps2)
  "Prepend list of paths PS1 to list of paths PS2.
Each path in the resulting list will be normalized and duplicates
removed from the final list."
  (let ((ps1_ (mapcar 'my-normalize-path ps1))
        (ps2_ (mapcar 'my-normalize-path ps2)))
    (my-remove-and-prepend-paths ps1_ ps2_)))


;; Prepend the contents of `my-path-variables' to `exec-path'.
(setq exec-path
      (let ((my-paths (mapcar 'symbol-value my-path-variables)))
        (my-concat-paths my-paths exec-path)))

;; Make the environment variable $PATH match `exec-path'
(let ((sep (if (eq system-type "windows-nt") ";" ":")))
  (setenv "PATH" (mapconcat 'identity exec-path sep)))


(message "%s" exec-path)


(require 'exec-path-from-shell)

;; (exec-path-from-shell-copy-env "PATH")


(provide 'my-paths)
;;; my-paths ends here
