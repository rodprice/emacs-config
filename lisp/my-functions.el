;;; my-functions.el --- Functions for use in emacs initialization and customization.
;;; Commentary:
;;; Code:

(require 'cl-lib)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package public keys

(defun my-keyring-modification-time ()
  "Return the time the keyring file was last modified."
  (file-attribute-modification-time
   (file-attributes
    (concat package-gnupghome-dir "/pubring.kbx")
    'string)))

;; https://stackoverflow.com/questions/5701388/where-can-i-find-the-public-key-for-gnu-emacs
(defun my-keyring-update-age ()
  "Return the age in months of the public key for package.el."
  (require 'calendar)
  (let* ((desc (alist-get 'gnu-elpa-keyring-update package-alist))
         (date (calendar-current-date))
         (year (nth 2 date))
         (month (car date)))
    (if (null desc)
        nil
      (let* ((pkg-date (package-desc-version (car desc)))
             (pkg-year (car pkg-date))
             (pkg-month (cadr pkg-date)))
        (if (> pkg-month month)
            (- (* 12 (- year pkg-year))
               (- 12 (- pkg-month month)))
          (+ (* 12 (- year pkg-year))
             (- month pkg-month)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions for running shells

(defun my-add-kill-buffer-sentinel ()
  "Set a process sentinel that kills the buffer when the process exits."
  (let* ((process (get-buffer-process (current-buffer)))
         (sentinel (process-sentinel process)))
    (set-process-sentinel
     process
     `(lambda (process signal)
        ;; Call the original process sentinel first.
        (funcall #',sentinel process signal)
        ;; Kill the buffer on an exit signal.
        (when (memq (process-status process) '(exit signal))
          (let ((buffer (process-buffer process)))
            (when (buffer-live-p buffer)
              (quit-window t (get-buffer-window buffer)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to manipulate alists

(defun alist-modify (alist key value)
  "If KEY is found in ALIST, replace the old value for KEY with
VALUE. If KEY is not found in ALIST, add KEY,VALUE to the result."
  (if (assq key alist)
      (mapcar
       (lambda (pair)
         (if (not (eq key (car pair))) pair (cons key value)))
       alist)
    (cons (cons key value) alist)))

(defun alist-remove (alist key)
  "Return ALIST with KEY removed."
  (seq-filter
   (lambda (pair) (not (eq key (car pair))))
   alist))

(defmacro alist-pop (alist key)
  "Pop the value matching KEY, then remove KEY from ALIST."
  `(prog1
       (cdr (assq ,key ,alist))
     (setq ,alist (alist-remove ,alist ,key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulate project data and files

(defun my-safe-append (var objs)
  "Append the sequence OBJS to the list in symbol VAR, if VAR
contains a list. Otherwise do nothing."
  (if (boundp var)
      (if (listp (symbol-value var))
          (let ((lst (if (listp objs) objs (list objs))))
            (dolist (obj lst)
              (cl-pushnew obj (symbol-value var))))
        (message "Symbol '%s' does not contain a list." var))
    (message "Symbol '%s' is not bound." var)))

(defun my-list-of-strings-p (value)
  "Test whether VALUE is a list of strings."
  (and
   (listp value)
   (cl-every #'stringp value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monkey around with exec-path and env variable PATH

(defun my-seq-most (seq)
  "Drop the last element of SEQ."
  (seq-take seq (- (length seq) 1)))

(defun my-seq-last (seq)
  "Take the last element of SEQ."
  (seq-drop seq (- (length seq) 1)))

(defun my-file-name-split (dir)
  "Return a list of the parts of DIR."
  (let ((parts
         (my-seq-most
          (file-name-split
           (file-name-as-directory dir)))))
    (if (eq system-type 'windows-nt)
        (mapcar #'downcase parts)
      parts)))

(defun my-subdirectory-p (dir subdir)
  "Check whether SUBDIR is a subdirectory of DIR."
  ;; (unless (file-directory-p dir)
  ;;   (error "Not a directory `%s'" dir))
  ;; (unless (file-directory-p subdir)
  ;;   (error "Not a directory `%s'" subdir))
  (let ((dir-parts (my-file-name-split dir))
        (subdir-parts (my-file-name-split subdir)))
    (and (<= (length dir-parts) (length subdir-parts))
         (cl-every 'identity
                   (cl-mapcar #'string=
                              dir-parts
                              subdir-parts)))))

(defun my-dedup-paths (paths)
  "Remove any duplicate paths in PATHS."
  (cond
   ((null paths)
    paths)
   ((member (car paths) (cdr paths))
    (my-dedup-paths (cdr paths)))
   (t
    (cons (car paths) (my-dedup-paths (cdr paths))))))

(defun my-filter-subdir-paths (dir paths)
  "Given a list of strings PATHS, return a list of all those
that are subdirectories of DIR, in the order they were found."
  (cond
   ((null paths)
    paths)
   ((my-subdirectory-p dir (car paths))
    (cons (car paths) (my-filter-subdir-paths dir (cdr paths))))
   (t
    (my-filter-subdir-paths dir (cdr paths)))))

(defun my-subdirectory-any-p (dirs subdir)
  "Check whether SUBDIR is a subdirectory of any directory in DIRS."
  (cl-some 'identity
           (mapcar
            (lambda (dir) (my-subdirectory-p dir subdir))
            dirs)))

(defun my-filter-any-subdir-paths (dirs paths)
  "Given a list of strings PATHS, return a list of all those
that are subdirectories of any directory in DIRS, in the order
they were found."
  (cond
   ((null paths)
    paths)
   ((my-subdirectory-any-p dirs (car paths))
    (cons (car paths) (my-filter-any-subdir-paths dirs (cdr paths))))
   (t
    (my-filter-any-subdir-paths dirs (cdr paths)))))

(defun my-remove-subdir-paths (dir paths)
  "Given a list of strings PATHS, return a list of all those
that are not subdirectories of DIR, in the order they were found."
  (let* ((stdpaths (mapcar #'expand-file-name paths))
         (rempaths (my-filter-subdir-paths dir stdpaths)))
    (cl-set-difference stdpaths rempaths :test #'string=)))

(defun my-remove-all-subdir-paths (dirs paths)
  "Given a list of strings PATHS, return a list of all those
that are not subdirectories of any directory in DIRS, in the
order they were found."
  (if (null dirs)
      paths
    (my-remove-all-subdir-paths
     (cdr dirs)
     (my-remove-subdir-paths (car dirs) paths))))

(defun my-sort-subdir-paths (dir paths)
  "Given a list of strings PATHS representing directories, sort them
such that all subdirectories of DIR come first."
  (let* ((dots (my-filter-regex-paths "^\\." paths))
         (nodots (cl-set-difference paths dots :test #'string=))
         (stdpaths (mapcar #'expand-file-name nodots))
         (apps (my-filter-subdir-paths dir stdpaths))
         (others (cl-set-difference stdpaths apps :test #'string=)))
    (append apps others dots)))

(defun my-filter-regex-paths (regex paths)
  "Given a list of strings PATHS, return a list of all those
matching REGEX, in the order they were found."
  (cond
   ((null paths)
    paths)
   ((string-match-p regex (car paths))
    (cons (car paths) (my-filter-regex-paths regex (cdr paths))))
   (t
    (my-filter-regex-paths regex (cdr paths)))))

(defun my-remove-regex-paths (regex paths)
  "Given a list of strings PATHS, return a list of all those
that do not match REGEX, in the order they were found."
  (let* ((stdpaths (mapcar #'expand-file-name paths))
         (rempaths (my-filter-regex-paths regex stdpaths)))
    (cl-set-difference stdpaths rempaths :test #'string=)))

(defun my-sort-regex-paths (regex paths)
  "Given a list of strings PATHS representing directories, sort them
such that all directories matching REGEX come first."
  (let* ((dots (my-filter-regex-paths "^\\." paths))
         (nodots (cl-set-difference paths dots :test #'string=))
         (stdpaths (mapcar #'expand-file-name nodots))
         (apps (my-filter-regex-paths regex stdpaths))
         (others (cl-set-difference stdpaths apps :test #'string=)))
    (append apps others dots)))

(defun my-convert-windows-drive-letter (winpath)
  "Convert a Windows path with initial drive letter `c:/Users' to a
Unix-like path `/c/Users'."
  (replace-regexp-in-string "\\([A-Za-z]\\):" "/\\1" winpath))
  ;; (let ((path (seq-remove (lambda (elt) (equal ?: elt)) winpath)))
  ;;   (concat (cons ?/ path))))

(defun my-write-bash-env-file (extra-paths &optional nospaces filename)
  "Write a file `bash-env.sh' to the `site' directory that adds
EXTRA-PATHS to the default path. Used with environment variable
`BASH_ENV', this adds paths to a non-login bash shell such as the
one made by `shell-command'. Setting NOSPACE to non-nil removes
filenames from EXTRA-PATHS that contain spaces."
  (let* ((filename (or filename "site/bash-env.sh"))
         (path (expand-file-name filename user-emacs-directory))
         (content (concat
                   "#!/usr/bin/bash\nexport PATH="
                   (string-join
                    (mapcar
                     #'my-file-name-windows-to-unixy
                     extra-paths)
                    ":")
                   "\n")))
    (with-temp-file path
      (insert content))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions to print lists in readable form

(defun print-path-list (paths)
  "Print the paths in a list of strings like exec-path."
  (dolist (path paths)
    (when path
      (prin1 path)
      (princ "\n"))))

(defun print-path-string (path-str)
  "Print the paths in a string like $PATH."
  (let* ((separator (if (memq window-system '(mac ns x)) ":" ";"))
         (paths (split-string path-str separator)))
    (print-path-list paths)))

(defun print-paths (paths)
  "Print a set of paths, either a string or a list of strings."
  (if (stringp paths)
      (print-path-string paths)
    (if (listp paths)
        (print-path-list paths)
      (princ "Argument `paths` is not a string or list of strings"))))

(defun pprint--stringify (obj &optional show-paths)
  "Convert OBJ into a list of strings suitable for printing. If
SHOW-PATHS is non-nil, expand path strings into lists."
  (cond
   ((null obj)
    "nil")
   ((stringp obj)
    (if (not show-paths)
        (format "%S" obj)
      (let* ((separator
              (if (member show-paths '(":" ";"))
                  show-paths
                (if (memq window-system '(mac ns x)) ":" ";")))
             (paths (split-string obj separator)))
        (if (length> paths 1)
            (append
             (list "PATH[")
             (mapcar (lambda (path) (format "%S" path)) paths)
             (list "]PATH"))
          (format "%S" obj)))))
   ((and (listp obj) (null (cdr (last obj)))) ; proper list only
    (append
     (list "(")
     (mapcar (lambda (elem) (pprint--stringify elem show-paths)) obj)
     (list ")")))
   ((listp obj)                         ; this must be a cons cell
    (list
     "("
     (pprint--stringify (car obj) show-paths)
     "."
     (pprint--stringify (cdr obj) show-paths)
     ")"))
   (t (format "%S" obj))))

(defun pprint--strings (objs &optional pfx)
  "Pretty print OBJS, using PFX as an indent."
  (let ((indent (concat "  " pfx))
        (delimiters '("(" ")" "PATH[" "]PATH")))
    (cond
     ((listp objs)
      ;; Base case
      (if (cl-every #'stringp objs)                  ; no sublists
          (let ((linestr (string-join objs " ")))
            (if (length< linestr (- fill-column (length pfx)))
                (princ (concat pfx linestr "\n"))    ; print list horizontally
              (dolist (obj objs)                     ; print list vertically
                (if (member obj delimiters)
                    (princ (concat pfx obj "\n"))
                  (princ (concat indent obj "\n"))))))
        ;; Recursive case
        (dolist (obj objs)                           ; print list vertically
          (if (member obj delimiters)
              (pprint--strings obj pfx)
            (pprint--strings obj indent)))))
     ((stringp objs)
      (princ (concat pfx objs "\n")))
   (t
    (user-error "OBJS is neither a string or a list")))))

(defun pprint (objs &optional show-paths)
  "Pretty print OBJS, which is usually a list. If SHOW-PATHS is
non-nil, split a string containing paths into lists for display."
  (if (listp objs) (princ "'"))
  (pprint--strings (pprint--stringify objs show-paths))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement of cursor

;; https://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  ;; Move left-right
  (let ((orig-point (point)))
    (move-beginning-of-line 1)
    (when (= orig-point (point))
      (back-to-indentation))))

(defun scroll-row-down (arg)
  (interactive "p")
  (scroll-up-command arg))

(defun scroll-row-up (arg)
  (interactive "p")
  (scroll-down-command arg))

(defun xah-forward-block (&optional n)
  "Move cursor beginning of next text block.
A text block is separated by blank lines.  This command similar
to `forward-paragraph', but this command's behavior is the same
regardless of syntax table.
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (search-forward-regexp "\n[\t\n ]*\n+" nil "NOERROR" n)))

(defun xah-backward-block (&optional n)
  "Move cursor to previous text block.
See: `xah-forward-block'
URL `http://ergoemacs.org/emacs/emacs_move_by_paragraph.html'
Version 2016-06-15"
  (interactive "p")
  (let ((n (if (null n) 1 n))
        (-i 1))
    (while (<= -i n)
      (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq -i n)))
      (setq -i (1+ -i)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open next(prev) line

(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them
  to autoindent.")

(defun current-line-blank-p ()
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (= (current-indentation) (- end beg))))

(defun previous-line-blank-p ()
  (save-excursion
    (let ((current-line-number (line-number-at-pos)))
      (forward-line -1)
      (if (= current-line-number (line-number-at-pos))
          t ;; at beginning of buffer
        (current-line-blank-p)))))

(defun next-line-blank-p ()
  (save-excursion
    (let ((current-line-number (line-number-at-pos)))
      (forward-line 1)
      (if (= current-line-number (line-number-at-pos))
          t ;; at end of buffer
        (current-line-blank-p)))))

(defun open-previous-line-new (arg)
  "Open a new line `arg' lines before the current one.  If
  `newline-and-indent' is true, indent the new line."
  (forward-line (- 1 arg))
  (beginning-of-line)
  (open-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun open-previous-line (arg)
  "Move to the previous line and then open a new blank line.
  This command behaves differently if it is called repeatedly.
  On the first call, it opens the line above it, whether it is a
  blank line or not.  On subsequent calls, it skips blank lines
  above it to open the previous line ahead of it.  Pressing
  \\[universal-argument] before calling this command causes it to
  behave as if it was being called for the first time.  If
  `newline-and-indent' is true, the new blank lines are indented.
  See `open-next-line'."
  (interactive "P")
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (if (or (not (current-line-blank-p))
            (equal arg '(4)))
        ;; at filled line => open previous line, move to previous line
        (open-previous-line-new 1)
      ;; at blank line
      (if (not (or (eq last-command 'open-next-line)
                   (eq last-command 'open-previous-line)))
          ;; new start => open previous line, move to previous line
          (open-previous-line-new 1)
        ;; repeat
        (delete-region beg (+ 1 end))
        (open-previous-line-new 2)))))

(defun open-next-line-new (arg)
  "Open a new line `arg' lines after the current one.  If
  `newline-and-indent' is true, indent the new line."
  (forward-line (- arg 1))
  (end-of-line)
  (open-line 1)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun open-next-line (arg)
  "Move to the next line and then open a new blank line.
  This command behaves differently if it is called repeatedly.
  On the first call, it opens the line below it, whether it is a
  blank line or not.  On subsequent calls, it skips blank lines
  above it to open the next line below it.  Pressing \\[universal-argument]
  before calling this command causes it to behave as if it was
  being called for the first time.  If `newline-and-indent' is
  true, the new blank lines are indented.  See
  `open-previous-line'."
  (interactive "P")
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (if (or (not (current-line-blank-p))
            (equal arg '(4)))
        ;; at filled line => open next line, move to next line
        (open-next-line-new 1)
      ;; at blank line
      (if (not (or (eq last-command 'open-next-line)
                   (eq last-command 'open-previous-line)))
          ;; new start => open next line, move to next line
          (open-next-line-new 1)
        ;; repeat
        (delete-region beg (+ 1 end))
        (open-next-line-new 1)))))

(defun transpose-next-line ()
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (goto-char (+ col (line-beginning-position)))))

(defun transpose-previous-line ()
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (forward-line -2)
    (goto-char (+ col (line-beginning-position)))))

(defun my-insert-semicolon ()
  (interactive)
  (move-end-of-line 1)
  (unless (looking-back ";" 1)
      (insert ";")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yasnippet stuff (see https://www.emacswiki.org/emacs/Yasnippet)
;; Completing point by some yasnippet key

(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Change CamelCase to camel_case and vice versa
;; https://stackoverflow.com/questions/9288181/converting-from-camelcase-to-in-emacs

(defun toggle-camelcase-underscores ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (upcase-initials-region start end)
            (replace-string "_" "" nil start end)
            (downcase-region start (1+ start)))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clean up the mode matching mechanism

(defun remove-all-matches-from-alist (name alist)
  "Remove any entries matching NAME from ALIST.

This function assumes that ALIST has the same form as
`auto-mode-alist' and friends.  This function recurses until all
entries matching NAME are removed.  The return value is the alist
with matching entries removed."
  (let ((mode (assoc-default name alist 'string-match)))
    (if mode
        (remove-all-matches-from-alist name
         (remove (rassoc mode alist) alist))
      alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use git grep for the grep command. This code is adapted from `vc-git-grep'.

(require 'vc-git)
(defun git-grep (regexp &optional files dir)
  "Run either 'git grep', if inside a project under git version control,
or 'grep' otherwise, searching for REGEXP in FILES in directory DIR.

The search is limited to file names matching shell pattern FILES.
FILES may use abbreviations defined in `grep-files-aliases', e.g.
entering `ch' is equivalent to `*.[ch]'.

With \\[universal-argument] prefix, you can edit the constructed shell command line
before it is executed.

Collect output in a buffer.  While either 'grep' or 'git grep' runs
asynchronously, you can use \\[next-error] (M-x next-error), or \\<grep-mode-map>\\[compile-goto-error] in the grep
output buffer, to go to the lines where grep found matches.

This command shares argument histories with \\[rgrep] and \\[grep]."
  (interactive
   (progn
     (grep-compute-defaults)
     (let* ((regexp (grep-read-regexp))
            (files (grep-read-files regexp))
            (dir (read-directory-name "In directory: " nil default-directory t)))
       (list regexp files dir))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (setq dir (file-name-as-directory (expand-file-name dir)))
    (let ((command regexp)
          (root (vc-git-root dir)))
      (setq command
            (grep-expand-template
             (if (null root)
                 (format "grep -i -nH -I -r -o -E -e %s <F>"
                         (shell-quote-argument (concat ".{0,40}" regexp)))
               "git --no-pager grep -n -e <R> -- <F>")
             regexp files))
      (when (equal current-prefix-arg '(4))
        (setq command
              (read-from-minibuffer "Confirm: " command nil nil 'grep-history)))
      (add-to-history 'grep-history command)
      (let ((default-directory dir)
            (compilation-environment (cons "PAGER=" compilation-environment)))
        (compilation-start command 'grep-mode))
      (if (eq next-error-last-buffer (current-buffer))
          (setq default-directory dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nice little alternative visual bell; Miles Bader <miles /at/ gnu.org>

(defcustom echo-area-bell-string "*DING* " ;"♪"
  "Message displayed in mode-line by `echo-area-bell' function."
  :group 'user)
(defcustom echo-area-bell-delay 0.1
  "Number of seconds `echo-area-bell' displays its message."
  :group 'user)

;; internal variables
(defvar echo-area-bell-cached-string nil)
(defvar echo-area-bell-propertized-string nil)

(defun echo-area-bell ()
  "Briefly display a highlighted message in the echo-area.

    The string displayed is the value of `echo-area-bell-string',
    with a red background; the background highlighting extends to the
    right margin.  The string is displayed for `echo-area-bell-delay'
    seconds.

    This function is intended to be used as a value of `ring-bell-function'."

  (unless (equal echo-area-bell-string echo-area-bell-cached-string)
    (setq echo-area-bell-propertized-string
          (propertize
           (concat
            (propertize
             "x"
             'display
             `(space :align-to (- right ,(+ 2 (length echo-area-bell-string)))))
            echo-area-bell-string)
           'face '(:background "red")))
    (setq echo-area-bell-cached-string echo-area-bell-string))
  (message echo-area-bell-propertized-string)
  (sit-for echo-area-bell-delay)
  (message ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the MSYS bash shell on Windows

;; Adapted from
;; http://stackoverflow.com/questions/235254/how-can-i-run-cygwin-bash-shell-from-within-emacs
;; Perhaps use shell-quote-arguments rather than this?
(defun windows-to-unixy-path (path)
  "Convert a Windows-formatted PATH to one compatible with bash."
  (let* ((path1 (replace-regexp-in-string "\\([A-Za-z]\\):" "/\\1" path))
         (path2 (replace-regexp-in-string "\\\\" "/" path1))
         (path3 (replace-regexp-in-string " " "\\\\ " path2))
         (path4 (replace-regexp-in-string ";" ":" path3)))
    path4))

(defun my-file-name-windows-to-unixy (filename)
  "Convert a Windows-formatted FILENAME to one compatible with bash."
  (let* ((strings (split-string filename ":"))
         (path1 (if (null (cdr strings))
                    filename
                  (string-join
                   (cons "/"
                         (cons (downcase (car strings))
                               (cdr strings))))))
         (path2 (replace-regexp-in-string "\\\\" "/" path1)))
    (shell-quote-argument path2)))

;; Start up a Git for Windows bash shell external to Emacs. I don't
;; know how to make this run inside Emacs like the msys bash shell
;; below. See
;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
;; for possible approaches.
(defun git-bash ()
  "Run the Git for Windows bash shell."
  (interactive)
  ;;(windows-to-unixy-path)
  (setenv "PS1" "\\W$ ")
  (make-comint-in-buffer
   "git-bash" nil (concat my-git-for-windows-dir "git-bash.exe") nil))

;; Start up an MSYS bash shell within Emacs
(defun bash ()
  "Run the MSYS bash shell."
  (interactive)
  (let ((explicit-shell-file-name (expand-file-name "bash" my-msys-binaries-dir))
        (shell-file-name "bash")
        (explicit-bash.exe-args '("--noediting" "--login" "-i"))
        (shell-env (getenv "SHELL")))
    (setenv "SHELL" shell-file-name)
;    (setenv "PS1" "\\W$ ")
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
    (call-interactively 'shell)))

;; Shell scripts to call Windows batch files (or executables?) look like this:
;; #!/bin/sh
;; #
;; # Call lein.bat from an MSYS bash shell.
;;
;; "$SHELL" //c "lein.bat $1 $2 $3 $4 $5"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window creation and movement

(defun my-other-window ()
    "Switch to the next window in the frame.  Used as advice to
    other functions."
    (interactive)
    (other-window 1)
    (pop-to-buffer-same-window (other-buffer)))

(advice-add 'split-window-below :after 'my-other-window)
(advice-add 'split-window-right :after 'my-other-window)

;; Adapted from http://whattheemacsd.com
(defun my-join-lines ()
  "Join the following line onto the current line."
  (interactive)
  (join-line -1))

(defun my-rearrange-windows (&optional toggle)
  "Swap two buffers between two windows in the same frame.  When
  optional argument TOGGLE is non-nil, rotate between over-and-
  under and side-by-side orientation of windows instead."
  (interactive "P")
  (if (not toggle)
      (my-rotate-windows)
    (my-toggle-window-split)))

;; From http://whattheemacsd.com/
(defun my-toggle-window-split ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd
              (not (and (<= (car this-win-edges)
                            (car next-win-edges))
                        (<= (cadr this-win-edges)
                            (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Adapted from http://whattheemacsd.com
(defun my-rotate-windows ()
  "Swap buffers between two adjacent windows."
  (interactive)
  (if (not (> (count-windows) 1))
       (error "You can't rotate a single window!"))
  (let ((i 1)
        (numWindows (count-windows)))
    (while (< i numWindows)
      (let* ((w1 (elt (window-list) i))
             (w2 (elt (window-list) (+ (% i numWindows) 1)))
             (b1 (window-buffer w1))
             (b2 (window-buffer w2))
             (s1 (window-start w1))
             (s2 (window-start w2)))
        (set-window-buffer w1 b2)
        (set-window-buffer w2 b1)
        (set-window-start w1 s2)
        (set-window-start w2 s1)
        (setq i (1+ i))))))

;; From http://whattheemacsd.com
(defun my-comint-delchar-or-eof-or-kill-buffer (arg)
  "Terminate the process in a shell buffer, or if the process in
the buffer is already dead, kill the buffer."
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

;; Fontify eldoc messages. See http://www.emacswiki.org/emacs/ElDoc
(require 'eldoc)
(defun fontify-eldoc-argument-list (string)
  "Upcase and fontify STRING for use with `eldoc-mode'."
  (propertize (upcase string)
              'face 'font-lock-variable-name-face))

;; Toggle function for fill-paragraph
;; http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;; When popping the mark, continue popping until the cursor actually moves
;; http://endlessparentheses.com/faster-pop-to-mark-command.html
(defun modi/multi-pop-to-mark (orig-fun &rest args)
  "Call ORIG-FUN until the cursor moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))

;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))


;; Split the windows sensibly.
;; https://gitlab.com/jabranham/emacs/blob/master/init.el#L2537
(defun my/split-below-last-buffer (prefix)
    "Split the window above/below and display the previous buffer.
If prefix arg is provided, show current buffer twice."
    (interactive "p")
    (split-window-below)
    (other-window 1 nil)
    (if (= prefix 1)
        (switch-to-next-buffer)))

(defun my/split-right-last-buffer (prefix)
  "Split the window left/right and display the previous buffer
If prefix arg is provided, show current buffer twice."
  (interactive "p")
  (split-window-right)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))

;; (global-set-key (kbd "C-x 2")  'my/split-below-last-buffer)
;; (global-set-key (kbd "C-x 3")  'my/split-right-last-buffer)
(setq switch-to-prev-buffer-skip 'this)


;; Always open compilation buffers in the same window.
;; (add-to-list 'display-buffer-alist
;;              (cons (lambda (buffer alist)
;;                      (with-current-buffer buffer
;;                        (eq major-mode 'compilation-mode)))
;;                    (cons 'display-buffer-reuse-major-mode-window
;;                          '((inhibit-same-window . nil)
;;                            (reusable-frames . visible)
;;                            (inhibit-switch-frame . nil)))))

;; From https://emacs.stackexchange.com/questions/31761/how-to-use-a-separate-window-for-compilation-output
(defun display-buffer-reuse-major-mode-window (buffer alist)
  "Return a window displaying a buffer in BUFFER's major mode.
Return nil if no usable window is found.

If ALIST has a non-nil `inhibit-same-window' entry, the selected
window is not eligible for reuse.

If ALIST contains a `reusable-frames' entry, its value determines
which frames to search for a reusable window:
  nil -- the selected frame (actually the last non-minibuffer frame)
  A frame   -- just that frame
  `visible' -- all visible frames
  0   -- all frames on the current terminal
  t   -- all frames.

If ALIST contains no `reusable-frames' entry, search just the
selected frame if `display-buffer-reuse-frames' and
`pop-up-frames' are both nil; search all frames on the current
terminal if either of those variables is non-nil.

If ALIST has a non-nil `inhibit-switch-frame' entry, then in the
event that a window on another frame is chosen, avoid raising
that frame."
  (let* ((alist-entry (assq 'reusable-frames alist))
         (frames (cond (alist-entry (cdr alist-entry))
                       ((if (eq pop-up-frames 'graphic-only)
                            (display-graphic-p)
                          pop-up-frames)
                        0)
                       (display-buffer-reuse-frames 0)
                       (t (last-nonminibuffer-frame))))
         (window (let ((mode (with-current-buffer buffer major-mode)))
                   (if (and (eq mode (with-current-buffer (window-buffer)
                                       major-mode))
                            (not (cdr (assq 'inhibit-same-window alist))))
                       (selected-window)
                     (catch 'window
                       (walk-windows
                        (lambda (w)
                          (and (window-live-p w)
                               (eq mode (with-current-buffer (window-buffer w)
                                          major-mode))
                               (not (eq w (selected-window)))
                               (throw 'window w)))
                        'nomini frames))))))
    (when (window-live-p window)
      (prog1 (window--display-buffer buffer window 'reuse alist)
        (unless (cdr (assq 'inhibit-switch-frame alist))
          (window--maybe-raise-frame (window-frame window)))))))

(defvar regexp-frame-names "^\\(?:MAIN\\|SYSTEM\\|ORG\\|MISCELLANEOUS\\|COMPILATION\\)$"
    "Regexp matching frames with specific names.")

(defvar zweibaranov-buffer-regexp nil
  "Regexp of file / buffer names displayed in frame `COMPILATION`.")
(setq zweibaranov-buffer-regexp '("\\*compilation\\*"))

(defun zweibaranov-display-buffer-pop-up-frame (buffer alist)
  (cond
   ((regexp-match-p zweibaranov-buffer-regexp (buffer-name buffer))
    (if (get-frame "COMPILATION")
        (switch-to-frame "COMPILATION")
      ;; If unnamed frame exists, then take control of it.
      (catch 'break (dolist (frame (frame-list))
                      (if (not (string-match regexp-frame-names (frame-parameter frame 'name)))
                          (throw 'break (progn
                                          (switch-to-frame (frame-parameter frame 'name))
                                          (set-frame-name "COMPILATION"))))))
      ;; If dolist found no unnamed frame, then create / name it.
      (if (not (get-frame "COMPILATION"))
          (progn
            (make-frame)
            (set-frame-name "COMPILATION"))) )
    (set-window-buffer (selected-window) (buffer-name buffer))
    (set-buffer (buffer-name buffer)) )
   (t nil) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; REGEXP FUNCTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun regexp-match-p (regexps string)
;;   "Before the lisp function, define the variable like this:\n
;; (defvar example-regexp nil
;;   \"Regexps matching `buffer-name buffer` for frame name `SYSTEM`.\")
;;     (setq example-regexp '(\"\\(\\*foo\\*\\|\\*bar\\*\\)\"))
;; \nWithin the lisp function, use something like this:\n
;; (regexp-match-p example-regexp (buffer-name buffer))
;; \nOr, this:\n
;; (regexp-match-p example-regexp buffer-filename)"
  ;; (setq case-fold-search nil) ;; take case into consideration
  (catch 'matched
    (dolist (regexp regexps)
      (if (string-match regexp string)
        (throw 'matched t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; FRAME UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://www.emacswiki.org/emacs/frame-fns.el
(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

;; http://www.emacswiki.org/emacs/frame-fns.el
(defun get-frame (frame)
  "Return a frame, if any, named FRAME (a frame or a string).
  If none, return nil.
  If FRAME is a frame, it is returned."
  (cond ((framep frame) frame)
        ((stringp frame)
         (catch 'get-a-frame-found
           (dolist (fr (frame-list))
             (when (string= frame (get-frame-name fr))
               (throw 'get-a-frame-found fr)))
           nil))
        (t
         (error
          "Function `get-frame-name': Arg neither a string nor a frame: `%s'"
          frame))))

;; https://stackoverflow.com/questions/17823448/if-frame-named-xyz-exists-then-switch-to-that-frame
(defun switch-to-frame (frame-name)
  (let ((frames (frame-list)))
    (catch 'break
      (while frames
        (let ((frame (car frames)))
          (if (equal (frame-parameter frame 'name) frame-name)
              (throw 'break (select-frame-set-input-focus frame))
            (setq frames (cdr frames))))))))


(provide 'my-functions)
;;; my-functions.el ends here
