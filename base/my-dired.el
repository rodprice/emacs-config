;;; Various customizations to emacs' dired mode.
;; Rod Price, Oct 2009

;; Bookmark-related stuff is from
;; http://scottfrazersblog.blogspot.com/2009/12/emacs-using-bookmarked-directories.html


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; try out ido-dired, ido-list-directory


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoloads and requires
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dired extensions
(require 'dired-x)
;; Writable wired mode
(require 'wdired)
;; Show or hide detailed directory listing
(require 'dired-details+)
;; Use emacs bookmarks to store directories
(require 'bookmark)

;; Make dired use a single buffer.
(autoload 'dired-single-buffer             "dired-single" "" t)
(autoload 'dired-single-buffer-mouse       "dired-single" "" t)
(autoload 'dired-single-magic-buffer       "dired-single" "" t)
(autoload 'dired-single-toggle-buffer-name "dired-single" "" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (kbd `F5') Start the single dired "magic" buffer.
(global-set-key [(f5)] 'dired-single-magic-buffer)
;; (kbd `C-F5') Set the dired buffer to the current directory.
(global-set-key [(control f5)]
                (function
                 (lambda nil (interactive)
                   (dired-single-magic-buffer default-directory))))
;; (kbd `S-F5') Display the current directory in the minibuffer.
(global-set-key [(shift f5)]
                (function
                 (lambda nil (interactive)
                   (message "Current directory is: %s"
                            default-directory))))
;; (kbd `M-F5') Toggle between magic and ordinary single dired buffer.
(global-set-key [(meta f5)] 'dired-single-toggle-buffer-name)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-specific key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when
        it's loaded."
  ;; <add other stuff here>
  (setq wdired-allow-to-change-permissions 'advanced)
  ;; (kbd `C-x C-q') Change to writable dired mode.
  (define-key dired-mode-map (kbd "C-x C-q") 'wdired-change-to-wdired-mode)
  ;; (kbd `return') Visit the directory selected in the current buffer.
  (define-key dired-mode-map [return] 'dired-single-buffer)
  ;; (kbd `mouse-1') Visit the directory that you click on.
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  ;; (kbd `^') Visit the parent of the current directory.
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer ".."))))
  ;; (kbd `$') Jump to a directory stored in a bookmark.
  (define-key dired-mode-map "$" 'my-ido-bookmark-jump))

;; (kbd `$') Jump to a bookmarked directory when opening a file.
(define-key
  ido-file-dir-completion-map (kbd "$")
  'my-ido-use-bookmark-dir)

;; If dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; Make dired close the buffer when you open a file
;; (put 'dired-find-alternate-file 'disabled nil)
;; (add-hook 'dired-mode-hook
;;  (lambda ()
;;   (define-key dired-mode-map (kbd "<return>")
;;     'dired-find-alternate-file) ; was dired-advertised-find-file
;;   (define-key dired-mode-map (kbd "^")
;;     (lambda () (interactive) (find-alternate-file "..")))
;;   ; was dired-up-directory
;;  ))

;; Edit filenames from the dired buffer
;; (add-hook 'dired-mode-hook
;;           '(lambda ()
;;              (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)))

;; Open only one dired buffer at most
;;(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View doc, ps, pdf, and ppt files from a dired buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package xlhtml is obsolete.  Txutils should be edited to remove the
;; ppthtml and xlhtml parts, which come from the xlhtml package.

;; (require 'txutils)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize variables for dired mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make sure the dired buffer is always in sync with the file system.
(defadvice switch-to-buffer-other-window
  (after auto-refresh-dired (buffer &optional norecord) activate)
  (if (equal major-mode 'dired-mode) (revert-buffer)))
(defadvice switch-to-buffer
  (after auto-refresh-dired (buffer &optional norecord) activate)
  (if (equal major-mode 'dired-mode) (revert-buffer)))
(defadvice display-buffer
  (after auto-refresh-dired (buffer &optional not-this-window frame) activate)
  (if (equal major-mode 'dired-mode) (revert-buffer)))
(defadvice other-window
  (after auto-refresh-dired (arg &optional all-frame) activate)
  (if (equal major-mode 'dired-mode) (revert-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make bookmarked directories easier to use in dired mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Stop cluttering up my home directory with bookmark files
(setq bookmark-default-file (concat dotfiles-dir "/bookmarks"))

(defun my-ido-bookmark-jump ()
  "Jump to bookmark using ido"
  (interactive)
  (let ((dir (my-ido-get-bookmark-dir)))
    (when dir
      (find-alternate-file dir))))

(defun my-ido-get-bookmark-dir ()
  "Get the directory of a bookmark."
  (let* ((name (ido-completing-read "Use dir of bookmark: " (bookmark-all-names) nil t))
         (bmk (bookmark-get-bookmark name)))
    (when bmk
      (setq bookmark-alist (delete bmk bookmark-alist))
      (push bmk bookmark-alist)
      (let ((filename (bookmark-get-filename bmk)))
        (if (file-directory-p filename)
            filename
          (file-name-directory filename))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use bookmarks to switch directories when opening a file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-ido-use-bookmark-dir ()
  "Get directory of bookmark"
  (interactive)
  (let* ((enable-recursive-minibuffers t)
         (dir (my-ido-get-bookmark-dir)))
    (when dir
      (ido-set-current-directory dir)
      (setq ido-exit 'refresh)
      (exit-minibuffer))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Put bookmarks in the shell as environment variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq bookmark-save-flag 1)
(setq bookmark-sort-flag nil)

(defadvice bookmark-write-file (after my-bookmark-to-shell activate)
  "Convert bookmarks to format bash and tcsh (yuck!) can use."
  (let (filename)
    (with-temp-buffer
      (dolist (bmk bookmark-alist)
        (if (listp (caadr bmk))
            (setq filename (cdr (assoc 'filename (cadr bmk))))
          (setq filename (cdr (assoc 'filename (cdr bmk)))))
        (unless (file-directory-p filename)
          (setq filename (file-name-directory filename)))
        (insert (car bmk) "=" filename)
        (delete-char -1)
        (newline))
      (write-file "~/.bashrc_bmk")
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (insert "set ")
        (forward-line))
      (write-file "~/.cshrc_bmk"))))



(provide 'my-dired)

