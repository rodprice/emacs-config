;;; my-local-vars.el --- Show interesting buffer-local variables
;;; Author: Rodney Price <price-rodney@zai.com>
;;; Commentary:
;;; Code:

(eval-when-compile (require 'cl-lib))

(defface my-local-vars-header '((t :inherit font-lock-type-face))
  "Face for headers in my-local-vars buffers."
  :group 'mlv
  :version "0.1")

(defface my-local-vars-header-value '((t :inherit font-lock-variable-name-face))
  "Face for header values in my-local-vars buffers."
  :group 'mlv
  :version "0.1")

(defface my-local-vars-variable '((t :inherit font-lock-function-name-face))
  "Face for variables in my-local-vars buffers."
  :group 'mlv
  :version "0.1")

(defface my-local-vars-variable-value '((t :inherit font-lock-builtin-face))
  "Face for variable values in my-local-vars buffers."
  :group 'mlv
  :version "0.1")

(defvar my-local-vars-target nil
  "The buffer whose local variables are displayed.")

(defvar my-local-vars-process nil
  "The process (if any) running in the target buffer.")

(defvar my-local-vars-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for local variables buffer.")

(define-derived-mode my-local-vars-mode special-mode "Local vars"
  "Major mode for my-local-vars buffers."
  (setq buffer-read-only t))

(cl-defstruct (variables (:type list))
  >name        ;; buffer name
  >process     ;; name of process running in buffer
  >major-mode  ;; current major mode for the buffer
  >minor-modes ;; list of minor modes for the buffer
  )

(defun my-local-vars-find-variables (target)
  ""
  (with-current-buffer target
    (let ((vars (buffer-local-variables)))
      (make-variables
       :>name (buffer-name target)
       :>process (get-buffer-process target)
       :>major-mode (alist-get 'major-mode vars)
       :>minor-modes (alist-get 'local-minor-modes vars)
       ))))

(defun my-local-vars-filter (regex &optional buffer)
  (let* ((buffer (or buffer (current-buffer)))
         (vars (with-current-buffer buffer
                 (buffer-local-variables)))
         (matches))
    (dolist (var vars matches)
      (when (string-match regex (symbol-name (car var)))
        (push var matches)))))

(defun my-local-vars-hooks (&optional buffer)
  (let ((buffer (or buffer (current-buffer))))
    (mapcar #'car (my-local-vars-filter "-hook\\'"))))

(defun my-local-vars-refresh-header (&optional target)
  ""
  (let* ((target      ;; target buffer
          (if (null target) (current-buffer) (get-buffer target)))
         (vars        ;; buffer-local variables
          (my-local-vars-find-variables target))
         (-name        ;; buffer name
          (variables->name vars))
         (-process     ;; process running in buffer, if any
          (let ((proc (variables->process vars)))
            (if (null proc)
                "<none>"
              (process-name proc))))
         (-major-mode  ;; major mode of the buffer
          (symbol-name (variables->major-mode vars)))
         (-minor-modes ;; list of minor modes for the buffer
          (mapcar #'symbol-name (variables->minor-modes vars))))
    (concat
     (propertize "Name: " 'face 'my-local-vars-header)
     (propertize (format "%s\n" -name) 'face 'my-local-vars-header-value)
     (propertize "Process: " 'face 'my-local-vars-header)
     (propertize (format "%s\n" -process) 'face 'my-local-vars-header-value)
     (propertize "Major mode: " 'face 'my-local-vars-header)
     (propertize (format "%s\n" -major-mode) 'face 'my-local-vars-header-value)
     (propertize "Minor modes: " 'face 'my-local-vars-header)
     (propertize (format "%s\n" -minor-modes) 'face 'my-local-vars-header-value))))

(defun my-local-vars-refresh ()
  ""
  (let* ((buffer-read-only nil)
         (buffer (current-buffer))
         (target (buffer-local-value 'my-local-vars-target buffer)))
    (when (null target)
      (error "Target buffer is not defined"))
    (erase-buffer)
    (insert (my-local-vars-refresh-header target))
    (align-regexp (point-min) (point-max) "\\(:\\)")
    ))

(defun my-local-vars-show (&optional target)
  ""
  (interactive)
  (let* ((target (if (null target) (current-buffer) (get-buffer target)))
         (process (get-buffer-process target))
         (buffer (get-buffer-create "*local variables*")))
    (with-current-buffer buffer
      (my-local-vars-mode)
      (setq-local my-local-vars-target target)
      (setq-local my-local-vars-process process)
      (my-local-vars-refresh)
      (pop-to-buffer buffer))))

(provide 'my-local-vars)

;;; my-local-vars.el ends here

  ;; ( major-mode . emacs-lisp-mode )
  ;; ( buffer-read-only )
  ;; (default-directory . "c:/Users/rdprice/Apps/msys64/home/rdprice/.emacs.d/")
  ;; (buffer-file-name . "c:/Users/rdprice/Apps/msys64/home/rdprice/.emacs.d/init.el")
  ;; (local-minor-modes
  ;;   whole-line-or-region-local-mode
  ;;   font-lock-mode
  ;;   eldoc-mode
  ;;   display-line-numbers-mode
  ;;   flycheck-mode
  ;;   origami-mode
  ;;   corfu-mode
  ;;   auto-save-mode)
  ;; (electric-pair-text-pairs
  ;;   ( 96 . 39 )
  ;;   ( 8216 . 8217 )
  ;;   ( 34 . 34 )
  ;;   ( 8216 . 8217 )
  ;;   ( 8220 . 8221 ))
  ;; ( file-local-variables-alist )
  ;; (post-command-hook
  ;;   jit-lock--antiblink-post-command
  ;;   eldoc-schedule-timer
  ;;   flycheck-perform-deferred-syntax-check
  ;;   flycheck-error-list-update-source
  ;;   flycheck-error-list-highlight-errors
  ;;   flycheck-maybe-display-error-at-point-soon
  ;;   flycheck-hide-error-buffer
  ;;   t)
  ;; (focus-out-hook
  ;;   flycheck-cancel-error-display-error-at-point-timer
  ;;   t)
  ;; ( focus-in-hook flycheck-display-error-at-point-soon t )
  ;; ( before-revert-hook flycheck-teardown t )
  ;; (change-major-mode-hook
  ;;   font-lock-change-mode
  ;;   flycheck-teardown
  ;;   t)
  ;; ( kill-buffer-hook flycheck-teardown t )
  ;; (window-configuration-change-hook
  ;;   flycheck-perform-deferred-syntax-check
  ;;   t)
  ;; (after-change-functions
  ;;   jit-lock-after-change
  ;;   flycheck-handle-change
  ;;   t)
  ;; ( after-save-hook flycheck-handle-save t )
  ;; ( delayed-after-hook-functions )

  ;; (buffer-file-truename
  ;;   .
  ;;   "~/working/kalman/src/kepler/rotators.py")
  ;; ( buffer-file-coding-system . undecided-unix )
  ;; (post-self-insert-hook
  ;;   t
  ;;   python-electric-pair-string-delimiter
  ;;   python-indent-post-self-insert-function)
  ;; ( completion-at-point-functions python-completion-at-point t )
  ;; ( before-change-functions t syntax-ppss-flush-cache )
  ;; (eshell-path-env
  ;;   .
  ;;   "c:\\Users\\rdprice\\Apps\\Anaconda3\\envs\\kalman;c:\\Users\\rdprice\\Apps\\Anaconda3\\envs\\kalman\\Library\\mingw-w64\\bin;c:\\Users\\rdprice\\Apps\\Anaconda3\\envs\\kalman\\Library\\usr\\bin;c:\\Users\\rdprice\\Apps\\Anaconda3\\envs\\kalman\\Library\\bin;c:\\Users\\rdprice\\Apps\\Anaconda3\\envs\\kalman\\Scripts;c:\\Users\\rdprice\\Apps\\Anaconda3\\envs\\kalman\\bin;C:\\Users\\rdprice\\Apps\\Anaconda3\\condabin;C:\\Users\\rdprice\\Apps\\msys64\\home\\rdprice\\bin;C:\\Users\\rdprice\\Apps\\msys64\\home\\rdprice\\bin;C:\\Users\\rdprice\\Apps\\GnuPG\\bin;C:\\Users\\rdprice\\Apps\\CMake\\bin;C:\\Users\\rdprice\\Apps\\Emacs\\bin;C:\\Users\\rdprice\\Apps\\Julia\\bin;C:\\Users\\rdprice\\Apps\\Pandoc;C:\\Users\\rdprice\\Apps\\msys64\\mingw64\\bin;C:\\Users\\rdprice\\Apps\\shellcheck;C:\\Users\\rdprice\\Apps\\Microsoft VS Code\\bin;C:\\Users\\rdprice\\Apps\\msys64\\ucrt64\\bin;C:\\Users\\rdprice\\Apps\\Microsoft VS Code\\bin;c:\\Users\\rdprice\\Apps\\Emacs\\libexec\\emacs\\29.0.60\\x86_64-w64-mingw32;C:\\Program Files (x86)\\Common Files\\Oracle\\Java\\javapath;C:\\Windows\\system32;C:\\Windows;C:\\Windows\\System32\\Wbem;C:\\Windows\\System32\\WindowsPowerShell\\v1.0;C:\\Windows\\System32\\OpenSSH;C:\\Program Files\\PuTTY;C:\\Users\\rdprice\\AppData\\Local\\Microsoft\\WindowsApps")
