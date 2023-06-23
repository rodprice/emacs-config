;;; my-local-vars.el --- Show interesting buffer-local variables
;;; Author: Rodney Price <price-rodney@zai.com>
;;; Commentary:
;;; Code:

(require 'seq)
(require 'project)
(require 'origami)

(eval-when-compile (require 'cl-lib))

(defface my-local-vars-label-face '((t :inherit font-lock-type-face))
  "Face for headers in my-local-vars buffers."
  :group 'mlv
  :version "0.1")

(defface my-local-vars-value-face '((t :inherit font-lock-variable-name-face))
  "Face for header values in my-local-vars buffers."
  :group 'mlv
  :version "0.1")

(defface my-local-vars-doc-face '((t :inherit font-lock-doc-face))
  "Face for documentation in my-local-vars buffers."
  :group 'mlv
  :version "0.1")

(defvar my-local-vars-target nil
  "The buffer whose local variables are displayed.")

(defvar my-local-vars-process nil
  "The process (if any) running in the target buffer.")

(defvar my-local-vars-header-text
  (propertize
   "'o' toggles all folded values, 'l' toggles values under point.\n"
   'face 'my-local-vars-doc-face)
  "Text to insert at the top of the local variables buffer.")

(defvar my-local-vars-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o")         #'origami-toggle-all-nodes)
    (define-key map (kbd "<backtab>") #'origami-toggle-all-nodes)
    (define-key map (kbd "l")         #'origami-recursively-toggle-node)
    (define-key map (kbd "<tab>")     #'origami-recursively-toggle-node)
    (define-key map (kbd "n")         #'next-line)
    (define-key map (kbd "p")         #'previous-line)
    (define-key map (kbd "q")         #'quit-window)
    map)
  "Keymap for local variables buffer.")

(define-derived-mode my-local-vars-mode special-mode "Local vars"
  "Major mode for my-local-vars buffers."
  (add-to-list
   'origami-parser-alist
   `(my-local-vars-mode . ,(origami-markers-parser "(" ")")))
  (origami-mode +1)
  ;; (setq buffer-read-only t)
  )

(cl-defstruct (variables (:type list))
  >name        ;; buffer name
  >process     ;; name of process running in buffer
  >major-mode  ;; current major mode for the buffer
  >minor-modes ;; list of minor modes for the buffer
  >project     ;; list of variables related to projects
  >conda       ;; list of variables related to conda
  >hooks       ;; list of buffer-local hooks
  >functions)  ;; list of buffer-local abnormal hooks

(defun my-local-vars-find-variables (target)
  "Return a variables struct populated from buffer-local variables
in buffer TARGET."
  (with-current-buffer target
    (let ((vars (buffer-local-variables)))
      (make-variables
       :>name        (buffer-name target)
       :>process     (get-buffer-process target)
       :>major-mode  (alist-get 'major-mode vars)
       :>minor-modes (alist-get 'local-minor-modes vars)
       :>project     (my-local-vars-filter "project" vars)
       :>conda       (my-local-vars-filter "conda" vars)
       :>hooks       (my-local-vars-filter "\\-hook\\'" vars)
       :>functions   (my-local-vars-filter "\\-functions\\'" vars)))))

(defun my-local-vars-filter (regex vars)
  "Return elements of VARS with leading symbols matching REGEX."
  (let (matches)
    (dolist (var vars matches)
      (when (string-match regex (symbol-name (car var)))
        (push var matches)))))

(defun my-local-vars--format-value (indent value)
  "Return a string with formatted values in VALUE."
  (cond
   ((null value)
    "nil")
   ((proper-list-p value)
    (concat
     "( "
     (string-chop-newline
      (string-join
       ;; Recurse on each element of the list
       (mapcar
        (apply-partially
         #'my-local-vars--format-value
         (+ indent 2))
        value)
       ;; Join with padding
       (concat
        "\n"
        (make-string indent ?\s))))
     " )"))
   ((listp value)  ;; cons cell
    (format
     "( %s . %s )"
     (my-local-vars--format-value 0 (car value))
     (my-local-vars--format-value 0 (cdr value))))
   ((stringp value)
    (format "\"%s\"" value))
   (t
    (format "%s" value))))

(defun my-local-vars--refresh-line (label tabstop value)
  "Format a LABEL / VALUE pair, padding LABEL with TABSTOP spaces."
  (defun propertize-item (item)
    (propertize item 'face 'my-local-vars-value-face))
  (concat
   ;; Print the label
   (propertize
    (string-pad label tabstop)
    'face 'my-local-vars-label-face)
   ;; Print the value(s)
   (propertize
    (cond
     ((listp value)
      (concat
       " : "
       (my-local-vars--format-value (+ tabstop 5) value)
       "\n"))
    (t
     (format " : %s\n" value)))
    'face 'my-local-vars-value-face)))

(defun my-local-vars--shell-env (regex)
  "get list of shell environment variables matching REGEX."
  (let* ((env (shell-command-to-string (concat "env | grep " regex)))
         (vars (seq-take-while
                (lambda (str) (> (length str) 0))
                (split-string env "\n"))))
    (mapcar
     (lambda (str)
       (let ((parts (split-string str "=")))
         (cons (car parts) (nth 1 parts))))
     vars)))

(defun my-local-vars--refresh (&optional target)
  "Return a string representing labels and values in the `variables'
struct."
  (let* ((target       ;; target buffer
          (if (null target) (current-buffer) (get-buffer target)))
         (vars         ;; buffer-local variables
          (my-local-vars-find-variables target))
         (-name        ;; buffer name
          (variables->name vars))
         (-project-name
          (with-current-buffer target
            (let ((project (project-current)))
              (if (null project)
                  "<none>"
                (file-name-nondirectory
                 (directory-file-name
                  (project-root project)))))))
         (-conda-name
          (let ((conda-env (getenv "CONDA_DEFAULT_ENV")))
            (if (null conda-env)
                "<none>"
              conda-env)))
         (-process     ;; process running in buffer, if any
          (let ((proc (variables->process vars)))
            (if (null proc)
                "<none>"
              (process-name proc))))
         (-major-mode  ;; major mode of the buffer
          (variables->major-mode vars))
         (-minor-modes ;; list of minor modes for the buffer
          (variables->minor-modes vars))
         (-project
          (variables->project vars))
         (-conda
          (variables->conda vars))
         (-hooks       ;; list of buffer-local hooks
          (variables->hooks vars))
         (-functions   ;; list of buffer-local hooks
          (variables->functions vars))
         (-conda-env
          (my-local-vars--shell-env "CONDA"))
         (tabstop 12))
    (concat
     my-local-vars-header-text
     (propertize "\nBuffer\n" 'face 'my-local-vars-doc-face)
     (my-local-vars--refresh-line "Buffer name" tabstop -name)
     (my-local-vars--refresh-line "Project name" tabstop -project-name)
     ;; (my-local-vars--refresh-line "Conda name" tabstop -conda-name)
     (my-local-vars--refresh-line "Process" tabstop -process)
     (propertize "\nBuffer-local variables\n" 'face 'my-local-vars-doc-face)
     (my-local-vars--refresh-line "Major mode" tabstop -major-mode)
     (my-local-vars--refresh-line "Minor modes" tabstop -minor-modes)
     (my-local-vars--refresh-line "Project" tabstop -project)
     (my-local-vars--refresh-line "Conda" tabstop -conda)
     (my-local-vars--refresh-line "Hooks" tabstop -hooks)
     (my-local-vars--refresh-line "Functions" tabstop -functions)
     (propertize "\nShell environment\n" 'face 'my-local-vars-doc-face)
     (my-local-vars--refresh-line "Conda env" tabstop -conda-env)
     )))

(defun my-local-vars-refresh ()
  "Refresh the contents of the local variables buffer using
buffer-local variables found in the current buffer."
  (let* ((buffer-read-only nil)
         (buffer (current-buffer))
         (target (buffer-local-value 'my-local-vars-target buffer)))
    (when (null target)
      (error "Target buffer is not defined"))
    (erase-buffer)
    (insert (my-local-vars--refresh target))
    (goto-char (point-min))))

;;;###autoload
(defun my-local-vars-show (&optional target)
  "Show interesting buffer-local variables in a new window."
  (interactive)
  (let* ((target (if (null target) (current-buffer) (get-buffer target)))
         (process (get-buffer-process target))
         (buffer (get-buffer-create "*local variables*")))
    (with-current-buffer buffer
      (my-local-vars-mode)
      (setq-local my-local-vars-target target)
      (setq-local my-local-vars-process process)
      (my-local-vars-refresh)
      (origami-close-all-nodes buffer)
      (pop-to-buffer buffer))))

(provide 'my-local-vars)
