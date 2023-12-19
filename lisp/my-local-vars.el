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

(defface my-local-vars-header-face
  '((t :inherit font-lock-constant-face
       :foreground "SlateGray"))
  "Face for documentation in my-local-vars buffers."
  :group 'mlv
  :version "0.1")

(defface my-local-vars-help-face
  '((t :inherit font-lock-constant-face
       :foreground "SlateGray"))
  "Face for help text in my-local-vars buffers."
  :group 'mlv
  :version "0.1")

(defvar my-local-vars--target nil
  "The buffer whose local variables are displayed.")

(defvar my-local-vars--process nil
    "The process (if any) running in the target buffer.")

(defvar my-local-vars-display-name
  "*local variables*"
  "Name of the buffer showing buffer-local variables.")

(defvar my-local-vars-help-name
  "*local variables help*"
  "Name of the help buffer for the local variables buffer.")

(defvar my-local-vars-header-text
  (propertize
   (concat
    "'F7' pops to new frame, "
    "'h' toggles help window, "
    "'q' kills this buffer.\n")
   'face 'my-local-vars-header-face)
  "Text to insert at the top of the local variables buffer.")

(defun my-local-vars--help-propertize (tabstop item)
  ""
  (let ((key-str (propertize
                  (string-pad (car item) tabstop)
                  'face 'my-local-vars-doc-face))
        (help-str (propertize
                   (cdr item)
                   'face 'my-local-vars-help-face))
        (sep-str (propertize
                   ": "
                   'face 'my-local-vars-doc-face)))
    (concat key-str sep-str help-str "\n")))

(defvar my-local-vars--help-text
  (apply #'concat
         (mapcar
          (apply-partially #'my-local-vars--help-propertize 6)
          '(
            ("F7"    . "pop to new frame")
            ("S-tab" . "fold/unfold all values")
            ("tab"   . "fold/unfold value under point")
            ("n"     . "move point to next line")
            ("p"     . "move point to previous line")
            ("h"     . "toggle help window")
            ("q"     . "close current window"))))
  "Help text for local variables window.")

(defvar my-local-vars-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o")         #'origami-toggle-all-nodes)
    (define-key map (kbd "<backtab>") #'origami-toggle-all-nodes)
    (define-key map (kbd "l")         #'origami-recursively-toggle-node)
    (define-key map (kbd "<tab>")     #'origami-recursively-toggle-node)
    (define-key map (kbd "n")         #'next-line)
    (define-key map (kbd "p")         #'previous-line)
    (define-key map (kbd "<f7>")      #'my-local-vars-pop-to-frame)
    (define-key map (kbd "h")         #'my-local-vars-show-help)
    (define-key map (kbd "q")         #'my-local-vars-close-window)
    map)
  "Keymap for local variables buffer.")

(define-derived-mode my-local-vars-mode special-mode "Local vars"
  "Major mode for my-local-vars buffers."
  (add-to-list
   'origami-parser-alist
   `(my-local-vars-mode . ,(origami-markers-parser "(" ")")))
  (origami-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers

(defun my-local-vars--buffer-visible-p (buffer-or-name &optional all-frames)
  "Return nil if BUFFER-OR-NAME has no window. See help for
`get-buffer-window' for information on ALL-FRAMES."
  (let* ((buffer (unless (null buffer-or-name)
                   (get-buffer buffer-or-name)))
         (window (unless (null buffer)
                   (get-buffer-window buffer all-frames))))
    (window-live-p window)))

(defun my-local-vars--buffer-relevant-p (buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME was created by my-local-vars."
  (let ((buffer (unless (null buffer-or-name)
                  (get-buffer buffer-or-name))))
    (and
     buffer
     (with-current-buffer buffer
       (eq major-mode 'my-local-vars-mode))
     (or
      (string=
       (buffer-name buffer)
       my-local-vars-display-name)
      (string=
       (buffer-name buffer)
       my-local-vars-help-name)))))

(defun my-local-vars--buffer-selected-p (buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME is in the selected window."
  (let ((buffer (get-buffer buffer-or-name)))
    (and buffer
         (eq (get-buffer-window buffer)
             (selected-window)))))

(defun my-local-vars--buffer-in-frame-p (buffer-or-name &optional frame)
  "Return the first buffer matching BUFFER-OR-NAME in FRAME."
  (let* ((frame (or frame (selected-frame)))
         (buffer-list (my-local-vars--buffer-list frame)))
    (my-local-vars--find-buffer buffer-or-name buffer-list)))

(defun my-local-vars--separate-frame-p (frame)
  "True iff the buffer-local variables buffer is in its own frame."
  (let* ((buffer-list (frame-parameter frame 'buffer-list))
         (visible-list (seq-filter
                        (lambda (buffer)
                          (my-local-vars--buffer-visible-p buffer frame))
                        buffer-list)))
    (seq-every-p
     #'my-local-vars--buffer-relevant-p
     visible-list)))

(defun my-local-vars--find-buffer (buffer-or-name buffer-list)
  "Return the first buffer matching BUFFER-OR-NAME in BUFFER-LIST."
  (let ((buffer (get-buffer buffer-or-name)))
    (seq-find
     (lambda (buf) (eq buffer buf))
     buffer-list)))

(defun my-local-vars--buffer-list (frame)
  "Returns a list of buffers relevant to my-local-vars in FRAME.

Results are given as follows:
  - The display buffer and help buffer both exist:
    ( display-buffer . help-buffer)
  - The display buffer exists but the help buffer does not:
    ( display-buffer )
  - Neither buffer exists:
    nil"
  (let* ((buffer-list (frame-parameter frame 'buffer-list))
         (buffers (seq-filter
                   (lambda (buffer)
                     (with-current-buffer buffer
                       (eq major-mode 'my-local-vars-mode)))
                   buffer-list))
         (buffer-names (mapcar #'buffer-name buffers)))
    (cond
     ((and
       (eq (length buffers) 1)
       (member my-local-vars-display-name buffer-names))
      buffers)
     ((and
       (eq (length buffers) 2)
       (member my-local-vars-display-name buffer-names)
       (member my-local-vars-help-name buffer-names))
      (let ((display-buf (my-local-vars--find-buffer
                          my-local-vars-display-name
                          buffers))
            (help-buf (my-local-vars--find-buffer
                       my-local-vars-help-name
                       buffers)))
        (list display-buf help-buf)))
     (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows

(defun my-local-vars--get-target-window (&optional all-frames)
  "Return the window showing the target buffer."
  (let ((display-buf (get-buffer my-local-vars-display-name)))
    (if (null display-buf)
        nil
      (let ((target-buf (buffer-local-value
                         'my-local-vars--target
                         display-buf)))
        (when target-buf
          (get-buffer-window target-buf all-frames))))))

(defun my-local-vars--get-display-window (&optional all-frames)
  "Return the window showing the display buffer."
  (let ((display-buf (get-buffer my-local-vars-display-name)))
    (if (null display-buf)
        nil
      (get-buffer-window display-buf all-frames))))

(defun my-local-vars--get-help-window (&optional all-frames)
  "Return the window showing the help buffer."
  (let ((help-buf (get-buffer my-local-vars-help-name)))
    (if (null help-buf)
        nil
      (get-buffer-window help-buf all-frames))))

(defun my-local-vars--quit-buffer (buffer-or-name &optional nokill frame)
  "Close the window showing BUFFER-OR-NAME in FRAME. If NOKILL is
nil, kill BUFFER-OR-NAME."
  (unless buffer-or-name
    (error "'buffer-or-name' argument is nil"))
  (let ((buffer (get-buffer buffer-or-name))
        (frame (or frame (selected-frame)))
        (kill (not nokill)))
    (unless buffer
      (error "Buffer %s cannot be found" buffer-or-name))
    (let* ((all-frames (if (frame-live-p frame) frame 'visible))
           (window (get-buffer-window buffer all-frames)))
      (if (window-live-p window)
          (quit-window kill window)
        (when kill
          (kill-buffer buffer))))))

;;;###autoload
(defun my-local-vars-close-window (&optional window nokill)
  "Close WINDOW and, if WINDOW's buffer is the only one ever shown
in the frame, close the frame as well. If NOKILL is nil, kill
WINDOW's buffer."
  (interactive)
  (let ((window (or window (selected-window))))
    (when (window-live-p window)
      (let* ((frame (window-frame window))
             (buffer (window-buffer window))
             (display-buf
              (my-local-vars--buffer-in-frame-p
               my-local-vars-display-name
               frame))
             (help-buf
              (my-local-vars--buffer-in-frame-p
               my-local-vars-help-name
               frame)))
        (cond
         ;; Window shows irrelevant buffer
         ((not (my-local-vars--buffer-relevant-p buffer)) nil)
         ;; Help buffer selected in window
         ((eq buffer help-buf)
          (my-local-vars--quit-buffer help-buf nokill frame))
         ;; Display buffer selected in window, but no help buffer
         ((and (eq buffer display-buf) (null help-buf))
          (when (my-local-vars--separate-frame-p frame)
            (delete-frame frame))
          (my-local-vars--quit-buffer display-buf nokill frame))
         ;; Display buffer selected in window, help buffer exists
         ((and (eq buffer display-buf) help-buf)
          (when (my-local-vars--separate-frame-p frame)
            (delete-frame frame))
          (my-local-vars--quit-buffer help-buf nokill frame)
          (my-local-vars--quit-buffer display-buf nokill frame)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames

;;;###autoload
(defun my-local-vars-pop-to-frame ()
  "Close the window showing buffer-local variables, open a new
frame, and show buffer-local variables in that frame."
  (interactive)
  (let* ((frame (selected-frame))
         (display-buf
          (my-local-vars--buffer-in-frame-p
           my-local-vars-display-name
           frame))
         (display-win (get-buffer-window display-buf))
         (help-buf
          (my-local-vars--buffer-in-frame-p
           my-local-vars-help-name
           frame))
         (help-win (get-buffer-window help-buf))
         (target
          (buffer-local-value
           'my-local-vars--target
           display-buf)))
    (cond
     ;; Already in separate frame
     ((my-local-vars--separate-frame-p frame)
      (message "Buffer '%s' already has its own frame" display-buf))
     ;; Display buffer exists, but no help buffer
     ((and display-buf (null help-buf))
      (make-frame `((name . ,my-local-vars-display-name)
                    (minibuffer . nil)))
      (my-local-vars-close-window display-win t))
     ;; Display buffer and help buffer both exist
     ((and display-buf help-buf t)
      (let ((display-selected-p
             (my-local-vars--buffer-selected-p display-buf))
            (help-selected-p
             (my-local-vars--buffer-selected-p help-buf)))
        (my-local-vars-close-window help-win)
        (let ((new-frame
               (with-current-buffer display-buf
                 (make-frame `((name . ,my-local-vars-display-name)
                               (minibuffer . nil))))))
          (with-selected-frame new-frame
            (my-local-vars-show-help)
            (select-window
             (get-buffer-window
              (cond
               (help-selected-p my-local-vars-help-name)
               (display-selected-p my-local-vars-display-name))
              new-frame)))))
      (my-local-vars-close-window display-win t)))))

(defun my-local-vars--buffer-frames (buffer)
  "Return a list of frames in which BUFFER has been displayed."
  (let ((buffer (get-buffer buffer)))
    (if (null buffer)
        nil
      (seq-filter
       (lambda (frame)
         (and
          (frame-visible-p frame)
          (memq buffer (frame-parameter frame 'buffer-list))))
       (frame-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data

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

(defun my-local-vars--shell-env (regex)
  "Get list of shell environment variables matching REGEX."
  (let* ((env (shell-command-to-string (concat "env | grep " regex)))
         (vars (seq-take-while
                (lambda (str) (> (length str) 0))
                (split-string env "\n"))))
    (mapcar
     (lambda (str)
       (let ((parts (split-string str "=")))
         (cons (car parts) (nth 1 parts))))
     vars)))

(defun my-local-vars-project-name (target)
  "Get the name of the project for buffer TARGET."
  (with-current-buffer target
    (let ((project (project-current)))
      (if (null project)
          "<none>"
        (file-name-nondirectory
         (directory-file-name
          (project-root project)))))))

(defun my-local-vars-git-branch-name (target)
  "Get the name of the current git branch for buffer TARGET."
  (with-current-buffer target
    (if vc-mode
        (cadr (split-string
               (substring-no-properties vc-mode) "[:-]+"))
      "<none>")))

(defun my-local-vars-conda-name (target)
  "Get the name of the Conda environment for buffer TARGET."
  (with-current-buffer target
    (let ((conda-env (getenv "CONDA_DEFAULT_ENV")))
      (if (null conda-env)
          "<none>"
        conda-env))))

(defun my-local-vars-popper-status (target)
  "If popper-mode is non-nil, get popper status for buffer TARGET."
  (cond
   ((null (memq 'popper package-activated-list))
    "<popper not available>")
   ((null (bound-and-true-p popper-mode))
    "<popper not in use>")
   (t
    (with-current-buffer target
      (if (boundp 'popper-popup-status)
          popper-popup-status
        "<none>")))))

(defun my-local-vars--symbol< (a b)
  (string< (symbol-name a) (symbol-name b)))

(defun my-local-vars-active-local-modes (target)
  ""
  (let ((modes (buffer-local-value 'local-minor-modes target)))
    (sort modes 'my-local-vars--symbol<)))

;; https://stackoverflow.com/questions/1511737/how-do-you-list-the-active-minor-modes-in-emacs
(defun my-local-vars-active-global-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and
                                  (symbolp mode)
                                  (symbol-value mode)
                                  (not (with-temp-buffer
                                         (local-variable-if-set-p mode))))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (sort active-modes 'my-local-vars--symbol<)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting

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
          (my-local-vars-project-name target))
         (-conda-name
          (my-local-vars-conda-name target))
         (-git-branch-name
          (my-local-vars-git-branch-name target))
         (-process     ;; process running in buffer, if any
          (let ((proc (variables->process vars)))
            (if (null proc) "<none>" (process-name proc))))
         (-popper-status
          (my-local-vars-popper-status target))
         (-major-mode  ;; major mode of the buffer
          (variables->major-mode vars))
         (-project
          (variables->project vars))
         (-conda
          (variables->conda vars))
         (-hooks       ;; list of buffer-local hooks
          (variables->hooks vars))
         (-functions   ;; list of buffer-local hooks
          (variables->functions vars))
         (-conda-vars
          (my-local-vars--shell-env "CONDA"))
         (-local-minor-modes ;; list of buffer-local minor modes
          (my-local-vars-active-local-modes target))
          ;; (variables->minor-modes vars))
         (-global-minor-modes ;; list of active global minor modes
          (my-local-vars-active-global-modes))
         (tabstop 13))
    (concat
     my-local-vars-header-text
     (propertize "\nBuffer\n" 'face 'my-local-vars-doc-face)
     (my-local-vars--refresh-line "Buffer name"   tabstop -name)
     (my-local-vars--refresh-line "Project name"  tabstop -project-name)
     (my-local-vars--refresh-line "Conda env"     tabstop -conda-name)
     (my-local-vars--refresh-line "Git branch"    tabstop -git-branch-name)
     (my-local-vars--refresh-line "Process"       tabstop -process)
     (propertize "\nActive modes\n" 'face 'my-local-vars-doc-face)
     (my-local-vars--refresh-line "Major mode"    tabstop -major-mode)
     (my-local-vars--refresh-line "Local minors"  tabstop -local-minor-modes)
     (my-local-vars--refresh-line "Global minors" tabstop -global-minor-modes)
     (propertize "\nBuffer-local variables\n" 'face 'my-local-vars-doc-face)
     (my-local-vars--refresh-line "Popper status" tabstop -popper-status)
     (my-local-vars--refresh-line "Project"       tabstop -project)
     (my-local-vars--refresh-line "Conda"         tabstop -conda)
     (my-local-vars--refresh-line "Hooks"         tabstop -hooks)
     (my-local-vars--refresh-line "Functions"     tabstop -functions)
     (propertize "\nShell environment\n" 'face 'my-local-vars-doc-face)
     (my-local-vars--refresh-line "Conda vars"    tabstop -conda-vars)
     )))

(defun my-local-vars-refresh ()
  "Refresh the contents of the local variables buffer using
buffer-local variables found in the current buffer."
  (let* ((buffer-read-only nil)
         (buffer (current-buffer))
         (target (buffer-local-value 'my-local-vars--target buffer)))
    (when (null target)
      (error "Target buffer is not defined"))
    (erase-buffer)
    (insert (my-local-vars--refresh target))
    (goto-char (point-min))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display

;;;###autoload
(defun my-local-vars-show-help ()
  "Show help text in a new window."
  (interactive)
  (cond
   ((my-local-vars--buffer-selected-p my-local-vars-help-name)
    (my-local-vars--quit-buffer (current-buffer)))
   (t
    (let ((buffer (get-buffer-create my-local-vars-help-name)))
      (with-current-buffer buffer
        (my-local-vars-mode)
        (let ((buffer-read-only nil))
          (erase-buffer)
          (insert my-local-vars--help-text)
          (goto-char (point-min)))
        (display-buffer-below-selected
         buffer
         '((dedicated . t)
           (inhibit-same-window . t)
           (window-height . 9)))
        (select-window (get-buffer-window buffer)))))))

;;;###autoload
(defun my-local-vars-show (&optional target)
  "Show interesting buffer-local variables in a new window."
  (interactive)
  (let* ((target (if (null target) (current-buffer) (get-buffer target)))
         (process (get-buffer-process target))
         (buffer (get-buffer-create my-local-vars-display-name)))
    (with-current-buffer buffer
      (my-local-vars-mode)
      (setq-local my-local-vars--target target)
      (setq-local my-local-vars--process process)
      (my-local-vars-refresh)
      (origami-close-all-nodes buffer)
      (pop-to-buffer buffer)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug

(defun my-local-vars--show (buffer target)
  ""
  (with-current-buffer buffer
    (my-local-vars-mode)
    (setq-local my-local-vars--target target)
    (setq-local my-local-vars--process (get-buffer-process target))
    (my-local-vars-refresh)
    (origami-close-all-nodes buffer)
    (pop-to-buffer buffer)))

;;;###autoload
(defun my-local-vars-debug (&optional target)
  "Show interesting buffer-local variables in a new window."
  (interactive)
  (let ((target (if (null target) (current-buffer) (get-buffer target))))
    (message "--- Target buffer is %s" target)
    (unless (buffer-live-p target)
      (user-error "Target buffer %s appears to be dead" target))
    (let ((target-frames (my-local-vars--buffer-frames target)))
      (unless target-frames
        (user-error "Can't find a frame for target buffer %s" target))
      (let ((buffer (get-buffer my-local-vars-display-name)))
        (cond ((null buffer)
               (message
                "--- Creating buffer %s"
                (my-local-vars--show
                 (get-buffer-create my-local-vars-display-name)
                 target)))
              ((and
                (buffer-live-p buffer)
                (my-local-vars--buffer-frames buffer))
                (let ((buffer-frames (my-local-vars--buffer-frames buffer))
                      (buffer-win (get-buffer-window buffer)))
                  (message "--- Buffer %s found in frames %s" buffer buffer-frames)
                  (message "--- Buffer window is %s" buffer-win)
                  (if (window-valid-p buffer-win)
                      (let ((buffer-frame (window-frame buffer-win)))
                        (message "--- Buffer window %s is valid" buffer-win)
                        (message "--- Buffer window frame is %s" buffer-frame))
                    (pop-to-buffer buffer)
                    (message "--- Buffer window %s is not valid" buffer-win))
                  (if (string= (buffer-name (current-buffer))
                               my-local-vars-display-name)
                      (message "--- Buffer %s is the current buffer" buffer)
                    (message "--- Buffer %s is the current buffer" (current-buffer)))
                  (if (eq (car target-frames) (car buffer-frames))
                      (message "--- Target frame and buffer frame are the same")
                    (message "--- Target frame and buffer frame are not the same"))
                  (message "--- Target frame is %s" (car target-frames))
                  (message "--- Buffer frame is %s" (car buffer-frames))
                  ))
              (t
               (message "--- Oops, buffer %s has a problem" buffer)
               (unless (buffer-live-p buffer)
                 (message "--- Buffer %s appears to be dead" buffer))
               (when (my-local-vars--buffer-frames buffer)
                 (message "--- Buffer %s not found in frames list" buffer)))
              )))))

(provide 'my-local-vars)
