;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up use-package for use with MELPA Stable and local archives

;; MELPA Stable archive
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;; 	     '("org" . "http://org-mode.org/elpa/") t)

;; Local archive. This is for packages that are not available on the
;; MELPA Stable or GNU package archives, but may be available on
;; MELPA.  Simply adding MELPA to package-archives results in all
;; packages being downloaded from these archives. I prefer versioned
;; packages, so I set up this local archive to store local versions of
;; packages not available on GNU or MELPA Stable. It also gives me the
;; opportunity to clean up the mess all too often found in these
;; unversioned packages.
(require 'package-x)
(defvar local-archive
  (expand-file-name "local/" user-emacs-directory)
  "Location of the package archive for packages stored locally.")
(unless (file-exists-p local-archive)
  (mkdir local-archive))
(setq package-archive-upload-base local-archive)
;; (add-to-list 'package-archives `("local" . ,local-archive) t)

;; Upload packages from current contents of pkgs directory
;; http://emacs.stackexchange.com/questions/19068/correct-usage-of-package-upload-file-for-multi-file-package
;; copy of tar files is necessary due to bug in package-x.el lines 246-252,
;; function package-upload-buffer-internal. It copies the directory
;; listing rather than the tar file itself.  In fact, the author just
;; ignored the tar file case altogether!
;; (dolist (file (directory-files myelpa-msde 'fqn "\\.*[.]\\(el\\|tar\\)"))
;;   (message "Preparing %s" file)
;;   (package-upload-file file)
;;   (when (string= (file-name-extension file) "tar")
;;     (copy-file file myelpa 'force)))

(package-initialize)

;; See https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(require 'diminish)
(setq use-package-verbose t)
;;(setq use-package-debug t)


(provide 'my-packaging)
