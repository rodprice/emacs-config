;;; functions.el --- Lisp functions used in initialization.

;; Copyright (C) 2010

;; Author:  <rod@thirdoption.info>
;; Keywords: lisp, local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Rod Price, April 2010

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions used in init.el

(defun current-directory ()
  "Returns the directory in which this function is executing."
  (file-name-directory (or (buffer-file-name) load-file-name)))

;; Tell emacs about a new directory containing elisp code.
(defun add-to-path-and-autoload (dirname &optional search)
  "Add this directory to emacs' load-path. If SEARCH is non-nil,
search files within the directory (nonrecursively) for autoload
functions. If the directory does not exist, return nil; otherwise
return the absolute path to the directory."
  (setq full-dirname (concat dotfiles-dir dirname))
  (let ((generated-autoload-file autoload-file))
    (if (not (and (file-exists-p full-dirname)
                  (file-directory-p full-dirname)))
        nil
      ;; directory exists
      (add-to-list 'load-path full-dirname)
      (message "Updating autoloads...")
      (let (emacs-lisp-mode-hook)
        (update-directory-autoloads full-dirname))
      full-dirname)))

(defun include (feature)
  "Catch errors when `requiring' a package."
  (let ((name (symbol-name feature)))
    (message "Loading %s..." name)
    (condition-case msg
        (progn
          (require feature)
          (message "Loading %s... done" name))
      (error
       (message "Loading %s... %s: %s" name (car msg) (cdr msg))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to set frame appearance

(defun x-toggle-full-screen ()
  "Make emacs frame toggle between maximized and normal."
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (setq full-screen-p (not full-screen-p)))

(defun x-maximize-frame ()
  "Maximize the current frame (X only)"
  (unless full-screen-p
    (x-toggle-full-screen)))

(defun x-restore-frame ()
  "Restore the current frame (X only)"
  (when full-screen-p
    (x-toggle-full-screen)))

(defun w32-maximize-frame ()
  "Maximize the current frame (windows only)"
  (interactive)
  (w32-send-sys-command 61488))

(defun w32-restore-frame ()
  "Restore a minimized/maximized frame (windows only)"
  (interactive)
  (w32-send-sys-command 61728))

(defun maximize-frame ()
  "Maximize the current frame."
  (interactive)
  (cond
   ((eq 'x (window-system))
    (x-maximize-frame))
   ((eq 'w32 (window-system))
    (w32-maximize-frame))
   (t
    (message
     "(maximize-frame) doesn't work with window system %s"
     (window-system)))))

(defun restore-frame ()
  "Restore the current frame to its original size."
  (interactive)
  (cond
   ((eq 'x (window-system))
    (x-restore-frame))
   ((eq 'w32 (window-system))
    (w32-restore-frame))
   (t
    (message
     "(restore-frame) doesn't work with window system %s"
     (window-system)))))


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
;; Show popups at appropriate times
;; See http://emacs-fu.blogspot.com/2009/11/showing-pop-ups.html

(defun show-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played."
  (interactive)
  (when sound
    (shell-command (concat "aplay " sound " 2> /dev/null")))
  (if (eq window-system 'x)
      (let ((cmd (concat "notify-send -t 0 "
                         (if icon (concat "-i " icon) "")
                         " '" title "' '" msg "'")))
        (condition-case nil
            (shell-command cmd)
          (error
           (message "Shell command failed:\n  %s" cmd))))
    (message (concat title ": " msg))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Below is steve yegge's stuff
;; from http://www.cabochon.com/~stevey/blog-rants/my-dot-emacs-file.html

;; someday might want to rotate windows if more than 2 of them
(defun swap-windows ()
  "If you have two windows, swap them."
  (interactive)
  (cond
   ((not (= (count-windows) 2))
    (message "You need exactly two windows open to do this."))
   (t
    (let* ((w1 (first (window-list)))
           (w2 (second (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1)))))

;;
;;  Never understood why Emacs doesn't have this function.
;;
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;
;;  Never understood why Emacs doesn't have this function, either.
;;
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn
        (copy-file filename newname 1)
        (delete-file filename)
        (set-visited-file-name newname)
        (set-buffer-modified-p nil)
        t))))

;; From Dan Schmidt, http://dfan.org/blog/tag/emacs/
(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions often bound directly to keys

;; As-yet unknown function
(defun undefined ()
  "Undefined function or variable, usually a placeholder."
  (interactive)
  (error "Function not yet defined"))

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun flexible-join-line ()
  "If at the end of a line, join the following line to this one;
otherwise, join this line to the preceding line."
  (interactive)
  (when (looking-at "\\(?:[[:blank:]]*$\\)")
    (next-line))
  (join-line))

(defun insert-lf-at-eol ()
  "Move to end of line and insert linefeed."
  (interactive)
  (move-end-of-line nil)
  (insert "\n"))

(defun insert-semicolon-at-eol ()
  "Move to end of line and insert semicolon."
  (interactive)
  (move-end-of-line nil)
  (insert ";"))

(defun insert-semicolon-lf-at-eol ()
  "Move to end of line and insert semicolon and linefeed."
  (interactive)
  (move-end-of-line nil)
  (insert ";\n"))

(defun duplicate-line-or-region (raw-prefix)
  "Duplicate the current line or region, leaving point at the
beginning of the duplicated text."
  (interactive "*P")
  (whole-line-or-region-call-with-region 'kill-ring-save 1 nil raw-prefix)
  (whole-line-or-region-yank raw-prefix))

(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill
line. Deletes whitespace at join."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (delete-indentation t)
    (kill-line arg)))

(defun forward-kill-sexp (&optional arg)
  "Kill the sexp after point."
  (interactive)
  (backward-kill-sexp -1))

(defun split-window-horizontally-and-go-there (arg)
  "Split selected window into two windows side by side, put the
  previous buffer in the buffer list in the new window, then
  select the new window, unless the prefix argument is present."
  (interactive "P")
  (split-window-and-go-there arg t))

(defun split-window-vertically-and-go-there (arg)
  "Split selected window into two windows one above the other,
  put the previous buffer in the buffer list in the new window,
  then select the new window, unless the prefix argument is
  present."
  (interactive "P")
  (split-window-and-go-there arg nil))

(defun split-window-and-go-there (arg horizontal-p)
  "Helper function for split-window-?-and-go-there functions."
  (select-window (split-window nil nil horizontal-p))
  (set-window-buffer (selected-window) (other-buffer))
  (when arg
    (select-window (next-window))))

(defun switch-to-minibuffer-window ()
  "If the minibuffer window is active, switch to it."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defun back-to-beginning ()
  "Toggle between beginning of line and first indentation."
  (interactive)
  (let ((oldpt (point))
        (newpt (save-excursion (move-beginning-of-line 1) (point))))
    (cond
     ((eq last-command 'move-beginning-of-line)
      (back-to-indentation)
      (setq this-command 'back-to-indentation))
     ((eq last-command 'back-to-indentation)
      (move-beginning-of-line 1)
      (setq this-command 'move-beginning-of-line))
     ((= oldpt newpt)
      (back-to-indentation)
      (setq this-command 'back-to-indentation))
     (t
      (move-beginning-of-line 1)
      (setq this-command 'move-beginning-of-line)))))

(defun back-to-end ()
  "Toggle between end of line and last non-comment character."
  (interactive)
  (let ((oldpt (point))
        (newpt (save-excursion (move-end-of-line 1) (point))))
    (cond
     ((eq last-command 'move-end-of-line)
      (back-to-end-sans-comments)
      (setq this-command 'back-to-end-sans-comments))
     ((eq last-command 'back-to-end-sans-comments)
      (move-end-of-line 1)
      (setq this-command 'move-end-of-line))
     ((= oldpt newpt)
      (back-to-end-sans-comments)
      (setq this-command 'back-to-end-sans-comments))
     (t
      (move-end-of-line 1)              ; comment
      (setq this-command 'move-end-of-line)))))

;; An awful hack, but it works
(defun back-to-end-sans-comments ()
  "Move to the last non-comment character on a line."
  (interactive)
  (move-end-of-line 1)
  (let ((comment-flag nil))
    (while
        (and (not (bolp))
             (or (looking-back "[ \t\r\n]")
                 (let* ((char-face (get-text-property (- (point) 1) 'face))
                        (flag (or (eq char-face
                                      'font-lock-comment-face)
                                  (eq char-face
                                      'font-lock-comment-delimiter-face))))
                   (when flag
                     (setq comment-flag t)))))
    (backward-char))
  (unless (or comment-flag (eolp))
    (forward-char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use ido for bookmarks and recent files
;; http://blog.kelsin.net/2010/04/22/using-ido-for-bookmarks-and-recent-files/

;; (eval-after-load "ido"
;;   (define-key ido-file-dir-completion-map (kbd "C-t") 'bookmark-ido-find-file))

(defun bookmark-ido-find-file ()
  "Find a bookmark using ido."
  (interactive)
  (let ((bk (ido-completing-read "Choose bookmark: "
                                 (bookmark-all-names)
                                 nil t)))
    (when bk
      (bookmark-jump bk))))

(defun recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let* ((files (mapcar 'lambda (file)
                        (cons (filename-non-directory file) file)
                        recentf-list))
         (file (cdr (assoc (ido-com "Choose recent file: "
                                                (mapcar 'car files)
                                                nil t)
                           files))))
    (when file
      (find-file file))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabs are evil; kill them all

(defun untabify-on-save ()
  "Expand all tabs and remove trailing whitespace when saving a buffer."
  (save-excursion
    ;; Remove trailing whitespace and tabs
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; Expand remaining tabs to spaces
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
  ;; Tell emacs that this function did not save the buffer
  nil)

(defun add-untabify-on-save ()
  "Add `untabify-on-save' to a hook upon entry to certain major modes."
  (add-hook 'write-contents-functions 'untabify-on-save))

(defun remove-untabify-on-save ()
  "Remove `untabify-on-save' from a hook."
  (remove-hook 'write-contents-functions 'untabify-on-save))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulate the help, completions, apropos buffers

(defvar window-config-snapshot nil)

(add-hook 'help-mode-hook 'save-window-configuration)
(defun save-window-configuration ()
  "Save the current window configuration, one hopes before the
help window has been displayed"
  (setq window-config-before-help (current-window-configuration)))

(defun bury-help-buffer ()
  "Get the help buffer out of the way quickly."
  (interactive)
  (pop-to-buffer (get-buffer "*Help*"))
  (bury-buffer)
  (set-window-configuration window-config-snapshot))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A few Haskell-like utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'thingatpt)
(require 'imenu)

;;; General elisp functions

(defun filter (pred elems)
  "Just like the Haskell `filter` function."
  (let ((result nil))
    (dolist (elem elems)
      (when (funcall pred elem)
        (push elem result)))
    (nreverse result)))

;; Ugly, but it works and doesn't blow the stack
(defun zip-with (fn list1 list2)
  "Just like the Haskell `zipWith` function."
  (let (swappedp shortest longest result)
    (if (< (length list1) (length list2))
        (progn
          (setq swappedp nil)
          (setq shortest list1)
          (setq longest list2))
      (setq swappedp t)
      (setq longest list1)
      (setq shortest list2))
    (dolist (elem1 shortest)
      (let ((elem2 (car longest)))
        (setq longest (cdr longest))
        (if swappedp
            (push (funcall fn elem2 elem1) result)
          (push (funcall fn elem1 elem2) result))))
    (nreverse result)))

(defun zip (list1 list2)
  "Just like the Haskell `zip` function (except no tuples)."
  (zip-with (lambda (e1 e2) (list e1 e2)) list1 list2))

(defun foldl (fn id elems)
  "Just like the Haskell `foldl` function."
  (let ((result id))
    (dolist (elem elems)
      (setq result (funcall fn result elem)))
    result))

;; Tail-recursive, but fat lot of good that does in elisp
(defun foldr (fn id elems)
  "Just like the Haskell `foldr` function."
  (cond
   ((null elems) id)
   (t (funcall fn (car elems) (foldr fn id (cdr elems))))))

;; The list in the args must have at least one element.
(defun fold (fn elems)
  "Just like the Haskell `foldl1` function."
  (cond
   ((null elems) (error "Empty list passed to `fold` function."))
   (t (foldl fn (car elems) (cdr elems)))))

(defun all (elems)
  "Returns true iff every element of the list is true."
  (foldl (lambda (e1 e2) (and e1 e2)) t elems))

(defun any (elems)
  "Returns true iff at least one element of the list is true."
  (foldl (lambda (e1 e2) (or e1 e2)) nil elems))

(defun none (elems)
  "Returns true iff no element of the list is true."
  (not (any elems)))

(defun list-of-strings-compare (list1 list2)
  "Determine whether two lists have equal contents."
  (if (= (length list1) (length list2))
      (all (zip-with 'string= list1 list2))
    nil))

(defun stack-list (elems)
  "Show a list of strings in \"stacked\"; i.e. vertical order for
easy reading."
  (interactive "P")
  (insert (concat "\n(\"" (car elems) "\"\n"))
  (dolist (elem (cdr elems))
    (insert (concat " \"" elem "\"\n")))
  (insert ")\n"))

;; Association lists

(require 'assoc)

(defun pop-alist (alist-symbol key)
  "Returns the pair RESULT with key KEY from ALIST, removing
RESULT from ALIST as a side effect."
  (let ((alist (symbol-value alist-symbol)))
    (if (assq key alist)
        (progn
          (asort alist-symbol key)
          (let ((alist-prime (symbol-value alist-symbol)))
            (set alist-symbol (cdr alist-prime))
            (car alist-prime)))
      nil)))

(defun push-alist (alist-symbol pair)
  "Just a synonym for aput, taking key and value as a pair. The
alist is modified as a side effect."
  (aput alist-symbol (car pair) (cdr pair)))

(defun modify-alist (alist delta)
  "Make a new alist from an old alist OLD and a list DELTA of new
key-value pairs.  If the key in a pair in DELTA is found in OLD,
then the value in OLD is replaced with the value in DELTA.  If
the key is not found, then the key-value pair is consed on to the
new list."
  (let ((alist-prime (copy-alist alist)))
    (dolist (pair delta)
      (push-alist 'alist-prime pair))
    alist-prime))

;; Network

(defun view-url ()
  "Open a URL in a web browser."
  (interactive)
  (let ((default (thing-at-point-url-at-point)))
    (if (eq major-mode 'w3m-mode)
        (let ((browse-url-browser-function 'browse-url-generic))
          (browse-url-at-point))
      (browse-url default))))

(defun view-url-raw ()
  "Open a new buffer containing the HTML or XML contents of URL."
  (interactive)
  (let* ((default (thing-at-point-url-at-point))
         (url (read-from-minibuffer "URL: " default)))
    (switch-to-buffer (url-retrieve-synchronously url))
    (rename-buffer url t)
    (tidy-buffer)
    ;; TODO: switch to nxml/nxhtml mode
    (cond ((search-forward "<?xml" nil t) (xml-mode))
          ((search-forward "<html" nil t) (html-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulate path variables

(defun add-to-path (oldpath path)
  "Prepend PATH to OLDPATH, ensuring that colons are inserted
correctly. OLDPATH is modified in place."
  (set oldpath (concat-paths path (symbol-value oldpath))))

(defun concat-paths (path1 path2 &optional separator)
  "Prepend PATH1 to PATH2, ensuring that colons are inserted
correctly. Neither PATH1 nor PATH2 are modified."
  (defun split-path (path sep)
    (if path (split-string path sep)))
  (defun interleave (xs sep)
    (mapconcat (lambda (x) x) xs sep))
  (let ((sep (if separator separator ":")))
    (interleave
     (append
      (split-path path1 sep)
      (split-path path2 sep))
     sep)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenience functions for entering Wanderlust refile rules

(defvar my-refile-rule-alist nil
  "Rules for filing email.")

(defun add-to-refile-rules (ruleset header pattern folder)
  "Add a refile rule to RULESET."
  (cond
   ((not ruleset)                       ; header not found
    (list (list header (cons pattern folder))))
   ((string= header (caar ruleset))     ; header matched
    (cons
     (cons header
           (cons (cons pattern folder) (cdar ruleset)))
     (cdr ruleset)))
   (t                                   ; header not matched yet
     (cons
      (car ruleset)
      (add-to-refile-rules (cdr ruleset) header pattern folder)))))

(defun add-refile-rules (folder &rest pairs)
  "Add zero or more refile rules to MY-MAILING-LISTS."
  (dolist (pair pairs)
    (setq my-refile-rule-alist
          (add-to-refile-rules
           my-refile-rule-alist         ; ruleset
           (car pair)                   ; header
           (cdr pair)                   ; pattern
           folder))))                   ; folder

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoinsert template functions

(require 'autoinsert)

(defun replace-autoinsert-action (new-action mode)
  "Replace the stock auto-insert action with NEW-ACTION for MODE."
  (let ((elt (assoc mode auto-insert-alist))
        (action new-action)) ; [select-template auto-update-*]
    (if elt
        (setcdr elt action)
      (push (cons mode action) auto-insert-alist))))

(defun select-template (prompt template-directory regex)
  "Choose a template name and insert the contents of the
appropriate template file."
  (interactive)
  ;; read in names of available templates matching regex
  (let* ((templates (directory-files template-directory t regex))
         (choices (mapcar 'file-name-nondirectory templates)))
    (cond
     ((> (length choices) 1)
      ;; Let the user choose the template
      (let ((choice (ido-completing-read prompt choices nil t)))
        ;; Copy the contents of the template to the buffer
        (insert-file-contents (concat template-directory choice))
        ;; Run auto-update-* to customize the buffer
        nil))
     ((eq (length choices) 1)
      ;; If only one choice, don't bother the user
      (insert-file-contents
       (concat template-directory (car choices))))
     (t
      ;; If no templates are available, tell the user and quit
      (message "No templates found in directory %s" template-directory)))))

(defun get-tentative-title ()
  "Use the buffer file name to make a suggested title for the document."
  (let ((stem (file-name-nondirectory
               (file-name-sans-extension buffer-file-name))))
    (concat "\\title{" stem  "}")))

(defun auto-update-document ()
  (save-excursion
    (while (search-forward "\\title{}" nil t)
      (save-restriction
        (narrow-to-region (match-beginning 0) (match-end 0))
        (replace-match (get-tentative-title))
        (subst-char-in-region (point-min) (point-max) ?. ?_)))))

(defun define-auto-insert (condition action &optional after)
  "Associate CONDITION with (additional) ACTION in `auto-insert-alist'.
Optional AFTER means to insert action after all existing actions for CONDITION,
or if CONDITION had no actions, after all other CONDITIONs."
  (let ((elt (assoc condition auto-insert-alist)))
    (if elt
        (setcdr elt
                (if (vectorp (cdr elt))
                    (vconcat (if after (cdr elt))
                             (if (vectorp action) action (vector action))
                             (if after () (cdr elt)))
                  (if after
                      (vector (cdr elt) action)
                    (vector action (cdr elt)))))
      (if after
          (nconc auto-insert-alist (list (cons condition action)))
        (push (cons condition action) auto-insert-alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface to web browsers

(defun browse-url-chrome (url &rest args)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((browse-url-browser-function 'browse-url-generic)
        (browse-url-generic-program "chromium")
        (browse-url-generic-args '("--enable-user-stylesheet")))
    (apply #'browse-url url args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the MSYS bash shell on Windows

;; Adapted from http://stackoverflow.com/questions/235254/how-can-i-run-cygwin-bash-shell-from-within-emacs
(defun windows-to-unixy-path (path)
  "Convert a Windows-formatted PATH to one compatible with bash."
  (let* ((path1 (replace-regexp-in-string "\\([A-Za-z]\\):" "/\\1" path))
         (path2 (replace-regexp-in-string "\\\\" "/" path1))
         (path3 (replace-regexp-in-string " " "\\\\ " path2))
         (path4 (replace-regexp-in-string ";" ":" path3)))
    path4))

;; Start up a bash shell within Emacs
(defun bash ()
  "Run the MSYS bash shell."
  (interactive)
  (let ((explicit-shell-file-name (concat msys-home "bash")))
    ;;(windows-to-unixy-path)
    (setenv "PS1" "\\W$ ")
    (call-interactively 'shell)))

;; Shell scripts to call Windows batch files (or executables?) look like this:
;; #!/bin/sh
;; #
;; # Call lein.bat from an MSYS bash shell.
;;
;; "$SHELL" //c "lein.bat $1 $2 $3 $4 $5"


(provide 'functions)
;;; functions.el ends here
