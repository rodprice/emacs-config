;;; my-paths.el
;;; Commentary:

;; Declare a few paths to external applications used by Emacs. These
;; paths are specific to each site (meaning machine and operating
;; system), and so should be set in the site/*-preload.el file. The
;; paths stored in the path variables will be prepended to `exec-path'
;; in the order that they are defined below. See the site/README.md
;; file for details.

;;; Code:


(require 'cl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup paths for executables emacs will use
;; These paths should be set in site/<system-name>-preload.el, not here.

(defvar my-path-variables nil
  "A list of path variables to be prepended to $PATH and `exec-path'.")

;; Python 2
(defvar my-anaconda2-dir nil
  "The path to the top directory of my Python Anaconda2 distribution.")
(defvar my-anaconda2-lib-dir nil
  "The path to the Lib directory of my Python Anaconda2 distribution.")
(defvar my-anaconda2-dlls-dir nil
  "The path to the DLLs directory of my Python Anaconda2 distribution.")
(defvar my-anaconda2-scripts-dir nil
  "The path to the scripts directory of my Python Anaconda2 distribution.")

;; Python 3
(defvar my-anaconda3-dir nil
  "The path to the top directory of my Python Anaconda3 distribution.")
(defvar my-anaconda3-lib-dir nil
  "The path to the Lib directory of my Python Anaconda2 distribution.")
(defvar my-anaconda3-dlls-dir nil
  "The path to the DLLs directory of my Python Anaconda2 distribution.")
(defvar my-anaconda3-scripts-dir nil
  "The path to the scripts directory of my Python Anaconda3 distribution.")

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
(if (eq (intern "windows-nt") system-type)
    (progn
      (defvar my-git-for-windows-dir nil
        "The path to the top directory of my git installation.")
      (add-to-list 'my-path-variables 'my-git-for-windows-dir)
      (defvar my-msys-binaries-dir nil
        "The path to the bin directory of my MSYS installation.")
      (add-to-list 'my-path-variables 'my-msys-binaries-dir)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function definitions, mostly to handle Windows quirks

(defun my-normalize-path (path)
  "Return a string representing a normalized version of PATH.
This function is intended for use with Windows paths, which are
case-insensitive.  As such, it will downcase all names if emacs
is running on Windows; otherwise it will leave the case as is."
  (let ((normed (expand-file-name "" path)))
    (if (eq (intern "windows-nt") system-type)
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
removed from the final list.  If running on Windows, all system
directories will be pushed to the end of the list."
  (let ((ps1_ (mapcar 'my-normalize-path ps1))
        (ps2_ (mapcar 'my-normalize-path ps2)))
    (delete-dups (sort
     (my-remove-and-prepend-paths ps1_ ps2_)
     (lambda (path1 path2) (not (my-windows-system-path-p path1)))))))


(defun my-remove-path (path paths)
  "Remove PATH from a list of paths PATHS."
  (let ((path_ (my-normalize-path path))
        (paths_ (delete-dups (mapcar 'my-normalize-path paths))))
    (remove path_ paths_)))


(defun my-remove-paths (ps1 ps2)
  "Remove all paths in PS1 from the list of paths PS2"
  (if ps1
      (my-remove-paths
       (cdr ps1)
       (my-remove-path (car ps1) ps2))
    ps2))


(defun my-replace-exec-path (path1 path2)
  "Replace PATH1 with PATH2 in exec-path."
  (setq exec-path
        (cons (my-normalize-path path2)
              (my-remove-path path1 exec-path))))


(defun my-harmonize-paths (newvars &optional oldvars)
  "Modifies exec-path and the $PATH environment variable, by
removing the contents of the variables in OLDVARS and adding the
contents of the variables in NEWVARS.  Paths are normalized and
sorted such that Windows system paths are last in the paths."
  (let* ((no-old-paths (if oldvars
                           (my-remove-paths
                            (mapcar 'symbol-value oldvars)
                            exec-path)
                         (mapcar 'my-normalize-path exec-path)))
         (paths (my-concat-paths
                 (mapcar 'symbol-value newvars)
                 no-old-paths))
         (sorted (sort paths
                       (lambda (p1 p2)
                         (my-windows-system-path-p p2)))))
    (setq exec-path sorted)
    (setenv "PATH" (mapconcat 'identity sorted path-separator))))


(defun my-windows-system-path-p (path)
  "Determine whether a string matches a Windows system path.
Returns false unless running on MS Windows.  When on Windows, it
compares the top-level directory name to each element of a list
containing a few common Windows directories that contain system
files, program files, or global data."
  (if (eq (intern "windows-nt") system-type)
      (let* ((elems (split-string (my-normalize-path path) "/"))
             (topdir (if (eq (length elems) 1) "" (cadr elems))))
        (some (lambda (arg) (string-equal topdir arg))
                    '("windows" "programdata" "program files" "program files (x86)")))
    nil))


(provide 'my-paths)
;;; my-paths ends here
