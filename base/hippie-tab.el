;;; hippie-tab.el --- Intelligent tab completion and indentation.

;; Copyright (C) 2010

;; Author:  <rod@thirdoption.info>
;; Keywords: convenience, abbrev

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

;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup hippie-tab nil
  "Customization group for hippie-tab minor mode."
  :group 'editing-basics
  :group 'convenience)

;;;###autoload
(defcustom hippie-tab-mode nil
  "Enable or disable hippie-tab minor mode.  Setting this
  variable directly does not take effect; use either
  \\[customize] or the function `hippie-tab-mode'."
  :set (lambda (symbol value) (hippie-tab-mode (or value 0)))
  :initialize 'custom-initialize-default
  :version "21.3"
  :type 'boolean
  :group 'hippie-tab
  :require 'hippie-tab)

(defcustom hippie-tab-mode-line-string " Hip"
  "String to display in the mode line when `hippie-tab' is
  active."
  :type 'string
  :group 'hippie-tab)

(defcustom hippie-tab-load-hook nil
  "Hook to run when this package is loaded."
  :type 'hook
  :group 'hippie-tab)

(defcustom hippie-tab-on-hook nil
  "Hook to run when `hippie-tab' mode is turned on."
  :type 'hook
  :group 'hippie-tab)

(defcustom hippie-tab-off-hook nil
  "Hook to run when `hippie-tab' mode is turned off."
  :type 'hook
  :group 'hippie-tab)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minor mode functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun hippie-tab-mode (&optional arg)
  "Hippie-tab minor mode."
  (interactive "P")
  ;; toggle on and off
  (setq hippie-tab-mode
        (if (null arg)
            (not hippie-tab-mode)
          (> (prefix-numeric-value arg) 0)))
  ;; turn mode on or off
  (run-hooks (if hippie-tab-mode
                 'hippie-tab-on-hook
               'hippie-tab-off-hook)))

;; Convenience function for use in hooks
(defun set-hippie-tab-mode ()
  "Turn on hippie-tab-mode."
  (hippie-tab-mode 1))

;; Convenience function for use in hooks
(defun unset-hippie-tab-mode ()
  "Turn off hippie-tab-mode."
  (hippie-tab-mode 0))

;; Allow the user to set or unset hippie-tab-mode on a per-buffer
;; basis.
(make-variable-buffer-local 'hippie-tab-mode)

;; Create the hippie-tab keymap and bind the tab key there
(unless (and (boundp 'hippie-tab-mode-map) hippie-tab-mode-map)
  (setq hippie-tab-mode-map (make-sparse-keymap))
  (define-key hippie-tab-mode-map "\t" 'hippie-tab)
  (define-key hippie-tab-mode-map [tab] 'hippie-tab))

;; Tell emacs about the hippie-tab minor mode
(unless (assq 'hippie-tab-mode minor-mode-alist)
  (let ((elem (list 'hippie-tab-mode 'hippie-tab-mode-line-string)))
    (add-to-list 'minor-mode-alist elem)))

;; Tell emacs about the hippie-tab key bindings
(unless (assq 'hippie-tab-mode minor-mode-map-alist)
  (let ((elem (cons 'hippie-tab-mode 'hippie-tab-mode-map)))
    (add-to-list 'minor-mode-map-alist elem)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatch tab keypress to appropriate function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'hippie-exp)

(defvar hippie-tab-last-action nil
  "State variable to record the last successful action inside
hippie-tab.")

(defun hippie-tab (arg)
  "When the tab key is pressed, this function does one of three
things: it completes the word preceding point, it indents the
current line, or it moves point to the next place that input
could reasonably be expected."
  (interactive "*P")
  (cond
   ;; Read-only buffer; just move point
   (buffer-read-only
      (hippie-tab-move-point))
   ;; A region is defined; indent the region
   ((use-region-p)
    (indent-region (region-beginning) (region-end)))
   ;; Special cases where we don't want to attempt completion
   ((hippie-tab-skip-expand-p)
    (unless (indent-current-line)
      (hippie-tab-move-point)))
   ;; Default action is: try to expand, try to indent, move point
   (t
    (unless (expand-current-word nil)
      (unless (indent-current-line)
        (hippie-tab-move-point))))))

(defun expand-current-word (arg)
  "Attempt to expand the expression before point in a
mode-specific way.  If a candidate expansion is found, return t;
otherwise return nil."
  (setq hippie-tab-last-action 'expand-current-word)
  (let ((mode (symbol-name major-mode)))
    (cond
     ((string= mode "org-mode")
      ;; call org-mode tab functions here
      (my-hippie-expand arg)) ; placeholder for now
     (t
      (my-hippie-expand arg)))))

(defun indent-current-line ()
  "Attempt to indent the current line.  If the position of the
text on the current line is changed (the indentation succeeds)
return t; otherwise return nil."
  (setq hippie-tab-last-action 'indent-current-line)
  (let ((col (current-indentation)))
    (indent-according-to-mode)
    (not (eq col (current-indentation)))))

;; Just a wrapper to include state variable updates
(defun hippie-tab-move-point ()
  "Move point to the next place that input could reasonably be
  expected."
  (setq hippie-tab-last-action 'hippie-tab-move-point)
  (cdlatex-tab-rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To expand or not to expand, that is the question
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hippie-tab-skip-expand-p ()
  "Returns t if conditions to skip expansion are met; nil
  otherwise."
  (or
   ;; Point should be at the end of a word for completion
   (not (or
         (point-at-end-of-syntax-p ?w)
         (point-at-end-of-syntax-p ?_)))
   ;; Don't try to expand a keyword
   (point-at-end-of-font-p 'font-lock-keyword-face)
   ;; Stop accidental expansions when point is at eol
   (and
    (eq hippie-tab-last-action 'hippie-tab-move-point)
    (eq last-command 'hippie-tab)
    (eolp)
    (not (eobp)))))

(defun last-command-attempted-to-insert-p ()
  "Attempt to divine the user's intent by examining the last
command he or she executed for context."
  (or (eq last-command 'self-insert-command)
      (eq last-command 'hippie-tab)))

(defun point-at-end-of-font-p (face)
  "Is point at the end of a word defined by a font-lock-face?"
  (let ((face-before (get-text-property (1- (point)) 'face))
        (face-after  (get-text-property     (point)  'face)))
    (and (or (bobp)      (eq face-before face))
         (or (eobp) (not (eq face-after  face))))))

(defun point-at-end-of-syntax-p (syntax)
  "Is point at the end of a word defined by a syntax type?"
  (and (or (bobp)      (= syntax (char-syntax (char-before))))
       (or (eobp) (not (= syntax (char-syntax (char-after)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define cursor motion upon tab keypress
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lifted from CDLaTeX package -- modified a bit
(defun cdlatex-tab-rules ()
  "This function is intended to do many cursor movements.

Then it jumps to the next point in a buffer where one would
reasonably expect that more input can be put in.  To do that, the
cursor is moved according to the following rules:

The cursor stops...
- before closing brackets if preceding-char is any of -({[]})
- after closing brackets, unless following-char is any of ({[_^
- just after \", if the cursor was before that \"
- at end of non-empty lines
- at the beginning of empty lines
- before a SPACE at beginning of line
- after first of several SPACEs

Sounds strange? Try it out.
"
  (interactive)
  (catch 'stop
    ;; Behavior at space or delimiter
    (cond
     ((looking-at "}\\|\\]\\|)")
      (forward-char 1)
      (if (looking-at "[^_\\^({\\[]")
          ;; stop after closing bracket, unless ^_[{( follow
          (throw 'stop t)))
     ((= (following-char) ?\ )
      ;; stop after first of many spaces
      (forward-char 1)
      (re-search-forward "[^ ]")
      (if (/= (preceding-char) ?\n) (forward-char -1)))
     (t
      (forward-char 1)))
    ;; Move to next possible stopping site and check out the place
    (while (re-search-forward "[ )}\n]\\|\\]" (point-max) t)
      (forward-char -1)
      (cond
       ((= (following-char) ?\ )
        ;; stop at first space or b-o-l
        (if (not (bolp)) (forward-char 1)) (throw 'stop t))
       ((= (following-char) ?\n)
        ;; stop at line end, but not after \\
        (if (and (bolp) (not (eobp)))
            (throw 'stop t)
          (if (equal "\\\\" (buffer-substring-no-properties
                             (- (point) 2) (point)))
              (forward-char 1)
            (throw 'stop t))))
       (t
        ;; Stop before )}] if preceding-char is any parenthesis
        (if (or (= (char-syntax (preceding-char)) ?\()
                (= (char-syntax (preceding-char)) ?\))
                (= (preceding-char) ?-))
            (throw 'stop t)
          (forward-char 1)
          (if (looking-at "[^_\\^({\\[]")
              ;; stop after closing bracket, unless ^_[{( follow
              (throw 'stop t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions that check out the surroundings

(defun cdlatex-quotes-balanced-to-here (&optional from)
  ;; Return t if the quotes are balanced between start of paragraph and point.
  (save-excursion
    (let ((answer t) (pos (point)))
      (if from
          (goto-char from)
        (backward-paragraph 1))
      (if (not (bobp)) (backward-char 1))
      (while (re-search-forward "[^\\]\"+" pos t)
        (if (/= (char-after (match-beginning 0)) ?\\)
            (setq answer (not answer))))
      (setq answer answer))))

(defun cdlatex-number-of-backslashes-is-odd ()
  ;; Count backslashes before point and return t if number is odd.
  (let ((odd nil))
    (save-excursion
      (while (equal (preceding-char) ?\\)
        (progn
          (forward-char -1)
          (setq odd (not odd)))))
    (setq odd odd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modified hippie-expand function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-hippie-expand (arg)
  "Try to expand text before point, using multiple methods.
The expansion functions in `hippie-expand-try-functions-list' are
tried in order, until a possible expansion is found.  Repeated
application of `hippie-expand' inserts successive possible
expansions.  With a positive numeric argument, jumps directly to
the ARG next function in this list.  With a negative argument or
just \\[universal-argument], undoes the expansion."
  (interactive "P")
  (if (or (not arg)
          (and (integerp arg) (> arg 0)))
      (let ((first (or (= he-num -1)
                       (not (equal this-command last-command)))))
        (if first
            (progn
              (setq he-num -1)
              (setq he-tried-table nil)))
        (if arg
            (if (not first) (he-reset-string))
          (setq arg 0))
        ;; Loop through try functions
        (let ((i (max (+ he-num arg) 0)))
          (while (not (or (>= i (length hippie-expand-try-functions-list))
                          (apply (nth i hippie-expand-try-functions-list)
                                 (list (= he-num i)))))
            (setq i (1+ i)))
          (setq he-num i))
        (if (>= he-num (length hippie-expand-try-functions-list))
            (progn
              (setq he-num -1)
              nil)  ; didn't find any expansions; return nil
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p (selected-window))))
              (message "Using %s"
                       (nth he-num hippie-expand-try-functions-list))
            t)))    ; found an expansion; be sure to return t
    (if (and (>= he-num 0)
             (eq (marker-buffer he-string-beg) (current-buffer)))
        (progn
          (setq he-num -1)
          (he-reset-string)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p (selected-window))))
              (message "Undoing expansions"))))))


;; Run load-time hooks for this minor mode
(run-hooks 'hippie-tab-load-hook)


(provide 'hippie-tab)
;;; hippie-tab.el ends here
