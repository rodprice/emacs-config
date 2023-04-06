;;; my-functions.el --- Functions for use in emacs initialization and customization.
;;; Commentary:
;;; Code:


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

(global-set-key (kbd "C-x 2")  'my/split-below-last-buffer)
(global-set-key (kbd "C-x 3")  'my/split-right-last-buffer)
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


;; (defun make-new-frame (frame-name)
;;   (make-frame `((name . ,frame-name) (width . 80) (height . 20) (fullscreen . nil)))
;;   (select-frame-by-name frame-name))

;; (defun switch-to-frame ()
;;   (interactive)
;;   (let ((frame-name "pytest")
;;         (frames (frame-list)))
;;     (catch 'break
;;       (dolist (frame frames (make-new-frame frame-name))
;;         (if (equal (frame-parameter frame 'name) frame-name)
;;             (throw 'break (select-frame-set-input-focus frame)))))))


;; (define-key ctl-x-map "\C-i"
;;   #'endless/ispell-word-then-abbrev)

;; Seems like you have to have ispell set up for this to work
;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
;; (defun endless/simple-get-word ()
;;   (car-safe (save-excursion (ispell-get-word nil))))

;; (defun endless/ispell-word-then-abbrev (p)
;;   "Call `ispell-word', then create an abbrev for it.
;; With prefix P, create local abbrev. Otherwise it will
;; be global.
;; If there's nothing wrong with the word at point, keep
;; looking for a typo until the beginning of buffer. You can
;; skip typos you don't want to fix with `SPC', and you can
;; abort completely with `C-g'."
;;   (interactive "P")
;;   (let (bef aft)
;;     (save-excursion
;;       (while (if (setq bef (endless/simple-get-word))
;;                  ;; Word was corrected or used quit.
;;                  (if (ispell-word nil 'quiet)
;;                      nil ; End the loop.
;;                    ;; Also end if we reach `bob'.
;;                    (not (bobp)))
;;                ;; If there's no word at point, keep looking
;;                ;; until `bob'.
;;                (not (bobp)))
;;         (backward-word)
;;         (backward-char))
;;       (setq aft (endless/simple-get-word)))
;;     (if (and aft bef (not (equal aft bef)))
;;         (let ((aft (downcase aft))
;;               (bef (downcase bef)))
;;           (define-abbrev
;;             (if p local-abbrev-table global-abbrev-table)
;;             bef aft)
;;           (message "\"%s\" now expands to \"%s\" %sally"
;;                    bef aft (if p "loc" "glob")))
;;       (user-error "No typo at or before point"))))

;; (setq save-abbrevs 'silently)
;; (setq-default abbrev-mode t)


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)

;; (defalias 'compilation-start 'lawlist-compilation-start)

(defun lawlist-compilation-start (command &optional mode name-function highlight-regexp)
  "Run compilation command COMMAND (low level interface).
If COMMAND starts with a cd command, that becomes the `default-directory'.
The rest of the arguments are optional; for them, nil means use the default.

MODE is the major mode to set in the compilation buffer.  Mode
may also be t meaning use `compilation-shell-minor-mode' under `comint-mode'.

If NAME-FUNCTION is non-nil, call it with one argument (the mode name)
to determine the buffer name.  Otherwise, the default is to
reuses the current buffer if it has the proper major mode,
else use or create a buffer with name based on the major mode.

If HIGHLIGHT-REGEXP is non-nil, `next-error' will temporarily highlight
the matching section of the visited source line; the default is to use the
global value of `compilation-highlight-regexp'.

Returns the compilation buffer created."
  (or mode (setq mode 'compilation-mode))
  (let* ((name-of-mode
          (if (eq mode t)
              "compilation"
            (replace-regexp-in-string "-mode\\'" "" (symbol-name mode))))
         (thisdir default-directory)
         (thisenv compilation-environment)
         outwin outbuf)
    (with-current-buffer
        (setq outbuf
              (display-buffer (get-buffer-create
                               (compilation-buffer-name name-of-mode mode name-function))
                              '(zweibaranov-display-buffer-pop-up-frame)))
      (let ((comp-proc (get-buffer-process (current-buffer))))
        (if comp-proc
            (if (or (not (eq (process-status comp-proc) 'run))
                    (eq (process-query-on-exit-flag comp-proc) nil)
                    (yes-or-no-p
                     (format "A %s process is running; kill it? " name-of-mode)))
                (condition-case ()
                    (progn
                      (interrupt-process comp-proc)
                      (sit-for 1)
                      (delete-process comp-proc))
                  (error nil))
              (error "Cannot have two processes in `%s' at once" (buffer-name)))))
      ;; first transfer directory from where M-x compile was called
      (setq default-directory thisdir)
      ;; Make compilation buffer read-only.  The filter can still write it.
      ;; Clear out the compilation buffer.
      (let ((inhibit-read-only t)
            (default-directory thisdir))
        ;; Then evaluate a cd command if any, but don't perform it yet, else
        ;; start-command would do it again through the shell: (cd "..") AND
        ;; sh -c "cd ..; make"
        (cd (cond
             ((not (string-match "\\`\\s *cd\\(?:\\s +\\(\\S +?\\|'[^']*'\\|\"\\(?:[^\"`$\\]\\|\\\\.\\)*\"\\)\\)?\\s *[;&\n]"
                                 command))
              default-directory)
             ((not (match-end 1)) "~")
             ((eq (aref command (match-beginning 1)) ?\')
              (substring command (1+ (match-beginning 1)) (1- (match-end 1))))
             ((eq (aref command (match-beginning 1)) ?\")
              (replace-regexp-in-string "\\\\\\(.\\)" "\\1"
                                        (substring command (1+ (match-beginning 1))
                                                   (1- (match-end 1)))))
             ;; Try globbing as well (bug#15417).
             (t (let* ((substituted-dir
                        (substitute-env-vars (match-string 1 command)))
                       ;; FIXME: This also tries to expand `*' that were
                       ;; introduced by the envvar expansion!
                       (expanded-dir
                        (file-expand-wildcards substituted-dir)))
                  (if (= (length expanded-dir) 1)
                      (car expanded-dir)
                    substituted-dir)))))
        (erase-buffer)
        ;; Select the desired mode.
        (if (not (eq mode t))
            (progn
              (buffer-disable-undo)
              (funcall mode))
          (setq buffer-read-only nil)
          (with-no-warnings (comint-mode))
          (compilation-shell-minor-mode))
        ;; Remember the original dir, so we can use it when we recompile.
        ;; default-directory' can't be used reliably for that because it may be
        ;; affected by the special handling of "cd ...;".
        ;; NB: must be done after (funcall mode) as that resets local variables
        (set (make-local-variable 'compilation-directory) thisdir)
        (set (make-local-variable 'compilation-environment) thisenv)
        (if highlight-regexp
            (set (make-local-variable 'compilation-highlight-regexp)
                 highlight-regexp))
        (if (or compilation-auto-jump-to-first-error
                (eq compilation-scroll-output 'first-error))
            (set (make-local-variable 'compilation-auto-jump-to-next) t))
        ;; Output a mode setter, for saving and later reloading this buffer.
        (insert "-*- mode: " name-of-mode
                "; default-directory: "
                (prin1-to-string (abbreviate-file-name default-directory))
                " -*-\n"
                (format "%s started at %s\n\n" mode-name
                        (substring (current-time-string) 0 19))
                ;; The command could be split into several lines, see
                ;; `rgrep' for example.  We want to display it as one
                ;; line.
                (apply 'concat (split-string command (regexp-quote "\\\n") t))
                "\n")
        (setq thisdir default-directory))
      (set-buffer-modified-p nil))
    ;; Pop up the compilation buffer.
    ;; http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01638.html
    (setq outwin (display-buffer outbuf))
    (with-current-buffer outbuf
      (let ((process-environment
             (append compilation-environment
                     (if (if (boundp 'system-uses-terminfo) ;`If' for compiler warning.
                             system-uses-terminfo)
                         (list "TERM=dumb" "TERMCAP="
                               (format "COLUMNS=%d" (window-width)))
                       (list "TERM=emacs"
                             (format "TERMCAP=emacs:co#%d:tc=unknown:" (window-width))))
                     ;; Set the EMACS variable, but
                     ;; don't override users' setting of $EMACS.
                     (unless (getenv "EMACS") (list "EMACS=t"))
                     (list "INSIDE_EMACS=t")
                     (copy-sequence process-environment))))
        (set (make-local-variable 'compilation-arguments)
             (list command mode name-function highlight-regexp))
        (set (make-local-variable 'revert-buffer-function)
             'compilation-revert-buffer)
        (set-window-start outwin (point-min))

        ;; Position point as the user will see it.
        (let ((desired-visible-point
               ;; Put it at the end if `compilation-scroll-output' is set.
               (if compilation-scroll-output
                   (point-max)
                 ;; Normally put it at the top.
                 (point-min))))
          (if (eq outwin (selected-window))
              (goto-char desired-visible-point)
            (set-window-point outwin desired-visible-point)))

        ;; The setup function is called before compilation-set-window-height
        ;; so it can set the compilation-window-height buffer locally.
        (if compilation-process-setup-function
            (funcall compilation-process-setup-function))
        (compilation-set-window-height outwin)
        ;; Start the compilation.
        (if (fboundp 'start-process)
            (let ((proc
                   (if (eq mode t)
                       ;; comint uses `start-file-process'.
                       (get-buffer-process
                        (with-no-warnings
                          (comint-exec
                           outbuf (downcase mode-name)
                           (if (file-remote-p default-directory)
                               "/bin/sh"
                             shell-file-name)
                           nil `("-c" ,command))))
                     (start-file-process-shell-command (downcase mode-name)
                                                       outbuf command))))
              ;; Make the buffer's mode line show process state.
              (setq mode-line-process
                    '(:propertize ":%s" face compilation-mode-line-run))

              ;; Set the process as killable without query by default.
              ;; This allows us to start a new compilation without
              ;; getting prompted.
              (when compilation-always-kill
                (set-process-query-on-exit-flag proc nil))

              (set-process-sentinel proc 'compilation-sentinel)
              (unless (eq mode t)
                ;; Keep the comint filter, since it's needed for proper
                ;; handling of the prompts.
                (set-process-filter proc 'compilation-filter))
              ;; Use (point-max) here so that output comes in
              ;; after the initial text,
              ;; regardless of where the user sees point.
              (set-marker (process-mark proc) (point-max) outbuf)
              (when compilation-disable-input
                (condition-case nil
                    (process-send-eof proc)
                  ;; The process may have exited already.
                  (error nil)))
              (run-hook-with-args 'compilation-start-hook proc)
              (setq compilation-in-progress (cons proc compilation-in-progress)))
          ;; No asynchronous processes available.
          (message "Executing `%s'..." command)
          ;; Fake mode line display as if `start-process' were run.
          (setq mode-line-process
                '(:propertize ":run" face compilation-mode-line-run))
          (force-mode-line-update)
          (sit-for 0)           ; Force redisplay
          (save-excursion
            ;; Insert the output at the end, after the initial text,
            ;; regardless of where the user sees point.
            (goto-char (point-max))
            (let* ((inhibit-read-only t) ; call-process needs to modify outbuf
                   (compilation-filter-start (point))
                   (status (call-process shell-file-name nil outbuf nil "-c"
                                         command)))
              (run-hooks 'compilation-filter-hook)
              (cond ((numberp status)
                     (compilation-handle-exit
                      'exit status
                      (if (zerop status)
                          "finished\n"
                        (format "exited abnormally with code %d\n" status))))
                    ((stringp status)
                     (compilation-handle-exit 'signal status
                                              (concat status "\n")))
                    (t
                     (compilation-handle-exit 'bizarre status status)))))
          (set-buffer-modified-p nil)
          (message "Executing `%s'...done" command)))
      ;; Now finally cd to where the shell started make/grep/...
      (setq default-directory thisdir)
      ;; The following form selected outwin ever since revision 1.183,
      ;; so possibly messing up point in some other window (bug#1073).
      ;; Moved into the scope of with-current-buffer, though still with
      ;; complete disregard for the case when compilation-scroll-output
      ;; equals 'first-error (martin 2008-10-04).
      (when compilation-scroll-output
        (goto-char (point-max))))

    ;; Make it so the next C-x ` will use this buffer.
    (setq next-error-last-buffer outbuf)))


(provide 'my-functions)
;;; my-functions.el ends here
