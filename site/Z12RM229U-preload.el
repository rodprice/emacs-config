;;; Z12RM229U-preload.el --- Site-specific initialization
;;; Commentary:
;;; Code:

(require 'cl)
(require 'vc-git)
(let ((load-path (list (expand-file-name "lisp/" user-emacs-directory))))
 (require 'my-functions))

(defvar my-default-font "Consolas-12")

(defvar my-apps-dir "c:/Users/rdprice/Apps")
(defvar my-git-dir (concat my-apps-dir "/Git"))
(defvar my-msys2-dir (concat my-apps-dir "/msys64"))
(defvar my-mingw-dir (concat my-apps-dir "/msys64/mingw64"))
(defvar my-ucrt-dir (concat my-apps-dir "/msys64/ucrt64"))
(defvar my-anaconda-dir (concat my-apps-dir "/Anaconda3"))
(defvar my-anaconda-scripts-dir (concat my-apps-dir "/Anaconda3/Scripts"))
(defvar my-putty-directory "c:/Program Files/PuTTY")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MSYS2

;; Use the MSYSTEM env variable to point to MSYS2 environment
;; MSYSTEM_PREFIX shows up in emacs as an absolute path, but
;; elsewhere as a relative path, so we don't rely on it.
(defvar my-msystem-dir
  (let ((msystem (getenv "MSYSTEM")))
    (cond
     ((string= msystem "MSYS")
      my-msys2-dir)
     ((string= msystem "CLANG32")    (concat my-msys2-dir "/clang32"))
     ((string= msystem "CLANG64")    (concat my-msys2-dir "/clang64"))
     ((string= msystem "CLANGARM64") (concat my-msys2-dir "/clangarm64"))
     ((string= msystem "MINGW32")    (concat my-msys2-dir "/mingw32"))
     ((string= msystem "MINGW64")    (concat my-msys2-dir "/mingw64"))
     ((string= msystem "UCRT64")     (concat my-msys2-dir "/ucrt64"))
     (t "")))
  "The directory corresponding to the MSYSTEM bash environment variable.")

(defvar my-msystem-relative-paths
  (let ((msystem (getenv "MSYSTEM"))
        (msys-paths
         '("/usr/local/bin"
           "/usr/bin"
           "/usr/bin/site_perl"
           "/usr/bin/vendor_perl"
           "/usr/bin/core_perl")))
    (cond
     ((string= msystem "MSYS")
      ;; Special case because /opt/bin shows up
      '("/usr/local/bin"
        "/usr/bin"
        "/opt/bin"
        "/usr/bin/site_perl"
        "/usr/bin/vendor_perl"
        "/usr/bin/core_perl"))
     ((string= msystem "CLANG32")    (cons "/clang32/bin"    msys-paths))
     ((string= msystem "CLANG64")    (cons "/clang64/bin"    msys-paths))
     ((string= msystem "CLANGARM64") (cons "/clangarm64/bin" msys-paths))
     ((string= msystem "MINGW32")    (cons "/mingw32/bin"    msys-paths))
     ((string= msystem "MINGW64")    (cons "/mingw64/bin"    msys-paths))
     ((string= msystem "UCRT64")     (cons "/ucrt64/bin"     msys-paths))
     (t msys-paths)))
  "Relative path corresponding to the MSYSTEM bash environment variable.")

(defvar my-msystem-extra-paths
  (mapcar
   (lambda (dir) (expand-file-name (concat my-msys2-dir dir)))
   my-msystem-relative-paths)
  "Absolute path corresponding to the MSYSTEM bash environment variable.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git for Windows

(defvar my-git-executable (expand-file-name "bin/git.exe" my-git-dir))
(setq vc-git-program my-git-executable)

;; Use these only when calling the git bash shell
(defvar my-git-relative-paths
  '("/mingw64/bin"
    "/usr/local/bin"
    "/usr/bin"
    "/usr/bin/vendor_perl"
    "/usr/bin/core_perl")
  "Paths pointing to executables in the Git for Windows shell.")

(defvar my-git-extra-paths
  (mapcar
   (lambda (dir) (expand-file-name (concat my-git-dir dir)))
   my-git-relative-paths)
  "Absolute path to Git for Windows executables.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assemble PATH and exec-path

(defvar my-user-paths
  (list
   (expand-file-name "bin" (getenv "HOME")))
  "Path for user executables.")

(defvar my-apps-paths
  '("c:/Users/rdprice/Apps/GnuPG/bin"
    "c:/Users/rdprice/Apps/CMake/bin"
    "c:/Users/rdprice/Apps/Emacs/bin"
    "c:/Users/rdprice/Apps/Julia/bin"
    "c:/Users/rdprice/Apps/Pandoc"
    "c:/Users/rdprice/Apps/msys64/mingw64/bin"
    "c:/Users/rdprice/Apps/shellcheck"
    "c:/Users/rdprice/AppData/Local/Microsoft/WindowsApps"
    ;; "c:/Users/rdprice/Apps/Microsoft VS Code/bin"
    )
  "Path for user-installed executables (except MSYS2 and Git for Windows).")

(defvar my-windows-paths
  '(
    ;; "c:/Program Files (x86)/Common Files/Oracle/Java/javapath"
    "c:/Windows/system32"
    "c:/Windows"
    "c:/Windows/System32/Wbem"
    "c:/Windows/System32/WindowsPowerShell/v1.0"
    "c:/Windows/System32/OpenSSH"
    ;; "c:/Program Files/PuTTY"
    "c:/Users/rdprice/AppData/Local/Microsoft/WindowsApps")
  "Path for Windows executables.")

(defvar my-exec-path-base
  (append
   my-user-paths
   my-apps-paths
   my-windows-paths)
  "Complete path omitting MSYS2 and Git for Windows.")

(defvar my-exec-path-git
  (append
   my-user-paths
   my-git-relative-paths
   my-apps-paths
   my-windows-paths)
  "Complete path for Git for Windows.")

(defvar my-exec-path-msystem
  (append
   my-user-paths
   my-msystem-relative-paths
   my-apps-paths
   my-windows-paths)
  "Complete path for MSYS2.")

(setq exec-path my-exec-path-base)
(setenv "PATH" (string-join exec-path path-separator))

(setenv "BASH_ENV" (expand-file-name "site/bash-env.sh" user-emacs-directory))
(my-write-bash-env-file my-exec-path-msystem 'nospaces)  ;; just in case

;; ;; Use these only when calling the MSYS2 bash shell
;; (defvar my-msys2-extra-paths
;;   (list
;;    (expand-file-name "bin" my-msys2-dir)
;;    (expand-file-name "usr\\bin" my-msys2-dir))
;;   "Paths pointing to MSYS2 executables in the MSYS2 shell.")

;; ;; Use these only when calling the MinGW bash (or zsh) shell
;; (defvar my-mingw-extra-paths
;;   (list
;;    (expand-file-name "bin" my-mingw-dir))
;;   "Paths pointing to MSYS2 executables in the MINGW shell.")

;; See this post from help-gnu-emacs
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2022-02/msg00228.html

;; ;; Remove all paths to MSYS2 or Git for Windows executables
;; (defvar my-exec-path-sans-msystem
;;   (my-remove-all-subdir-paths
;;    (cons my-git-dir my-msystem-extra-paths)
;;    (my-sort-subdir-paths my-apps-dir exec-path)))

;; (setenv "PATH" (string-join my-exec-path-sans-msystem path-separator))
;; (setq exec-path my-exec-path-sans-msystem)

;; Set up the BASH_ENV environment variable to initialize bash shells
;; created by e.g. shell-command.
;; (defvar my-bash-env-paths
;;   (my-dedup-paths
;;    (my-sort-subdir-paths
;;     (concat my-msystem-dir "\\home\\rdprice")
;;     (append my-msystem-extra-paths
;;             (my-remove-subdir-paths
;;              (concat my-apps-dir "\\Microsoft VS Code")
;;              (my-filter-subdir-paths
;;               my-apps-dir
;;               my-exec-path-sans-msystem)))))
;;   "Value of PATH in a non-interactive bash shell (i.e. in `shell-command')")

;; Junkyard
;; (let ((my-paths (my-remove-regex-paths "/Apps/Git" (my-sort-regex-paths "/Apps" exec-path))))
;;   (setenv "PATH" (string-join my-paths path-separator)))
