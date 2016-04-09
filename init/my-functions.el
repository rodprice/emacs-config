;;; my-functions.el --- Functions for use in emacs initialization and customization.
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Movement of cursor

(defun scroll-viewport (n)
  (let ((top (line-number-at-pos (window-start)))
        (cur (line-number-at-pos (point))))
    (recenter (+ (- cur top) n))))

(defun scroll-row-down (arg)
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-viewport (- arg)))

(defun scroll-row-up (arg)
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-viewport arg))

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

(provide 'my-functions)
;;; my-functions.el ends here
