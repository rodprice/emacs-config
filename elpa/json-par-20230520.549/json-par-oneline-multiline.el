;;; json-par-oneline-multiline.el --- Converting single-line format and multiline format in JSON Par mode -*- lexical-binding: t -*-

;; Copyright (C) 2021 taku0
;;
;; Author: taku0 (http://github.com/taku0)
;; URL: https://github.com/taku0/json-par

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; Functions for converting single-line format and multiline format in JSON Par
;; mode.

;;; Code:

(require 'json-par-motion)
(require 'json-par-insert)
(require 'json-par-delete)
(require 'json-par-indent)
(require 'json-par-utils)

(defvar json-par--fixup-adviced-functions nil
  "Functions to be adviced with `json-par--fixup-advice'.")

;;; Customizations

(defcustom json-par-action-when-breaking-line-at-just-inside-brackets
  'break-each-member
  "Action when breaking a line at just inside brackets.

- `break-inside-brackets': break the lines at just inside brackets.

  Example (`|' is the point):

  [ |1, 2, 3 ]
  ↓ (newline t)
  [
    |1, 2, 3
  ]

- `break-each-member': break lines after each members.

  Example (`|' is the point):

  [ |1, 2, 3 ]
  ↓ (newline t)
  [
    |1,
    2,
    3
  ]

This affects functions invoking `post-self-insert-hook', including `newline'."
  :type '(choice (const :tag "Break just inside brackets"
                        break-inside-brackets)
                 (const :tag "Break lines after each members"
                        break-each-member))
  :group 'json-par
  :safe #'symbolp)

(defcustom json-par-action-when-breaking-line-after-first-member
  'break-each-member
  "Action when breaking a line after the first member.

- `just-break': just break the line.

  Example (`|' is the point):

  [
    1, |2, 3
  ]
  ↓ (newline t)
  [
    1,
    |2, 3
  ]

- `break-each-member': break lines after each members.

  Example (`|' is the point):

  [
    1, |2, 3
  ]
  ↓ (newline t)
  [
    1,
    |2,
    3
  ]

This affects functions invoking `post-self-insert-hook', including `newline'."
  :type '(choice (const :tag "Just break the line"
                        just-break)
                 (const :tag "Break lines after each members"
                        break-each-member))
  :group 'json-par
  :safe #'symbolp)

(defcustom json-par-action-when-joining-non-empty-lines 'just-delete
  "Action when joining non-empty lines.

- `just-delete': just delete the line break.

  Example (`|' is the point):

  [
    1,
  |  2,
    3
  ]
  ↓ `json-par-delete-backward-char'
  [
    1,|  2,
    3
  ]

- `delete-line-breaks-between-members': delete all line breaks between members
  (but not before/after members) only if the each element is on its own line.

  Example (`|' is the point):

  [
    1,
  |  2,
    3
  ]
  ↓ `json-par-delete-backward-char'
  [
    1,|  2, 3
  ]

  [
    1,
  |  2,
    3, 4
  ]
  ↓ `json-par-delete-backward-char'
  [
    1,|  2,
    3, 4
  ]

- `delete-line-breaks-inside-brackets': delete all line breaks inside the
  current array/object, only if the each element is on its own line.

  Example (`|' is the point):

  [
    1,
  |  2,
    3
  ]
  ↓ `json-par-delete-backward-char'
  [ 1,|  2, 3 ]

  [
    1,
  |  2,
    3, 4
  ]
  ↓ `json-par-delete-backward-char'
  [
    1,|  2,
    3, 4
  ]

This affects `json-par-delete-backward-char', `json-par-delete-forward-char',
and `json-par-join-line'."
  :type '(choice
          (const :tag "Just delete" just-delete)
          (const :tag "Delete all line breaks between members"
                 delete-line-breaks-between-members)
          (const :tag "Delete line breaks inside the current array/object"
                 delete-line-breaks-inside-brackets))
  :group 'json-par
  :safe #'symbolp)


(defcustom json-par-long-line-threshold
  1000
  "Threshold of line length to ask if pretty print the buffer."
  :type 'number
  :group 'json-par
  :safe #'numberp)

;;; Functions

(defun json-par-multiline (&optional max-level)
  "Ensure at most one member for each lines in the current member.

If the value of the current member is a string, replace escape sequence \"\\n\"
in the string to line breaks.

If the member contains objects/arrays as values, recursively process the values,
up to MAX-LEVEL depth.

If MAX-LEVEL is omitted, it is considered as infinity.

Insert a space after colon if not exists."
  (interactive "P")
  (setq max-level
        (if (null max-level) 1.0e+INF (prefix-numeric-value max-level)))
  (save-excursion
    (json-par-beginning-of-object-value)
    (let* ((next-token (json-par-forward-token-or-list))
           (json-par--already-out-of-comment t)
           (json-par--already-out-of-atom t)
           (start (json-par-token-start next-token))
           (end (json-par-token-end next-token)))
      (goto-char start)
      (json-par--huge-edit start end
        (cond
         ;; Before an array or an object
         ((json-par-token-matching-brackets-p next-token)
          (json-par--multiline-after max-level)
          (json-par-indent-region start (point)))

         ;; Before string
         ((json-par-token-string-p next-token)
          (setq end (copy-marker end))
          (while (and (< (point) end)
                      (search-forward "\\n" end t))
            (replace-match "\n"))
          (json-par--free-marker end)))))))

(push #'json-par-multiline json-par--fixup-adviced-functions)

(defun json-par--multiline-after (max-level)
  "Ensure at most one member for each lines in the object/array after the point.

If the object/array contains objects/arrays as values, recursively process the
values, up to MAX-LEVEL depth.

Insert a space after colon if not exists."
  (let (token)
    (if (<= max-level 0)
        (progn
          (setq token (json-par-forward-token-or-list))
          (when (json-par-token-open-bracket-p token)
            ;; The bracket is not closed.
            (goto-char (point-max))))
      ;; Insert newline after the open bracket.
      (json-par-forward-token)
      (json-par--newline-unless-end-of-line)
      (while (progn
               (setq token (json-par-forward-token))
               (not (or (json-par-token-close-bracket-p token)
                        (json-par-token-outside-of-buffer-p token))))
        (cond
         ;; Comma
         ((json-par-token-comma-p token)
          (json-par--newline-unless-end-of-line))

         ;; Colon
         ((json-par-token-colon-p token)
          (unless (memq (char-after) '(?\s ?\t ?\n nil))
            (insert-char ?\s)))

         ;; Nested array/object
         ((json-par-token-open-bracket-p token)
          (goto-char (json-par-token-start token))
          (json-par--multiline-after (1- max-level)))))
      ;; Insert newline before the close bracket.
      (save-excursion
        (goto-char (json-par-token-start token))
        (json-par--backward-spaces)
        (json-par--newline-unless-end-of-line)))))

(defun json-par--newline-unless-end-of-line ()
  "Insert line break unless the point is at the end of a line.

Move to the end of the line."
  (json-par--forward-spaces t)
  (unless (or (eolp) (looking-at "//"))
    (delete-horizontal-space t)
    (insert-char ?\n)))

(defun json-par-oneline (&optional min-level)
  "Delete line breaks in the current member.

If the value of the current member is a string, escape line breaks in the
string instead.

If the object/array contains objects/arrays as values, recursively process the
values.  If MIN-LEVEL is more than zero, leave members less than MIN-LEVEL deep
as is.

If MIN-LEVEL is omitted, it is considered as zero.

If the current member is an array/object and has no line breaks, delete spaces
between tokens."
  (interactive "P")
  (setq min-level (if (null min-level) 0 (prefix-numeric-value min-level)))
  (save-excursion
    (json-par-beginning-of-member)
    (let* ((parsed (json-par--parse-member-forward))
           (parent-token (json-par--parent-token))
           (inside-object (json-par-token-open-curly-bracket-p parent-token))
           (json-par--already-out-of-comment t)
           (json-par--already-out-of-atom t)
           (start (gethash :start-of-member parsed))
           (end (gethash :end-of-member parsed))
           object-value-start
           next-token)
      (json-par--huge-edit start end
        (when inside-object
          (json-par-beginning-of-object-value nil parsed)
          (when (zerop min-level)
            ;; Delete line breaks around the key and colon.
            (setq object-value-start (point-marker))
            (goto-char start)
            (while (progn
                     (setq next-token (json-par-forward-token))
                     (< (point) object-value-start))
              (when (and (json-par-token-colon-p next-token)
                         (eq (char-after) ?\n))
                (insert-char ?\s))
              (json-par--delete-newline-and-following-spaces))
            (goto-char (json-par--free-marker object-value-start))))
        (setq next-token (json-par-forward-token))
        (cond
         ;; Before an array or an object
         ((json-par-token-open-bracket-p next-token)
          (goto-char (json-par-token-start next-token))
          (json-par--oneline-after min-level nil t))

         ;; Before string
         ((json-par-token-string-p next-token)
          (goto-char (json-par-token-start next-token))
          (setq end (copy-marker (json-par-token-end next-token)))
          (while (and (< (point) end)
                      (search-forward "\n" end t))
            (replace-match "\\n" t t))
          (json-par--free-marker end)))))))

(push #'json-par-oneline json-par--fixup-adviced-functions)

(defun json-par--oneline-after
    (min-level &optional keep-newlines-around-brackets delete-spaces-if-oneline)
  "Delete line breaks in the object/array after the point.

If the object/array contains objects/arrays as values, recursively process the
values.  If MIN-LEVEL is more than zero, leave members less than MIN-LEVEL deep
as is.

If KEEP-NEWLINES-AROUND-BRACKETS is non-nil, keep line breaks around brackets.
It is used by `json-par-join-line'.

If the current member is an array/object and has no line breaks, and if
DELETE-SPACES-IF-ONELINE is non-nil, delete spaces between tokens."
  (let* ((whole-list (save-excursion (json-par-forward-token-or-list)))
         (one-line (json-par-token-one-line-p whole-list))
         token)
    (if (and one-line delete-spaces-if-oneline)
        (json-par--delete-spaces-between-tokens
         (json-par-token-start whole-list)
         (json-par-token-end whole-list))
      (json-par-forward-token)
      (when (and (zerop min-level) (not keep-newlines-around-brackets))
        (when (and (eq (char-after) ?\n)
                   (save-excursion
                     (skip-chars-forward "\s\t\n")
                     (not (memq (char-after) '(?\] ?\) ?} nil)))))
          (insert-char ?\s))
        (json-par--delete-newline-and-following-spaces))
      (while (progn
               (setq token (json-par-forward-token))
               (not
                (or (json-par-token-close-bracket-p token)
                    (json-par-token-outside-of-buffer-p token))))
        (cond
         ;; Comma or colon
         ((memq (json-par-token-type token) '(\, :))
          (when (and (zerop min-level) (eq (char-after) ?\n))
            (insert-char ?\s)))

         ;; Nested array/object
         ((json-par-token-open-bracket-p token)
          (goto-char (json-par-token-start token))
          (json-par--oneline-after (max 0 (1- min-level)) nil nil)))
        (when (and (zerop min-level)
                   (not (and keep-newlines-around-brackets
                             (save-excursion
                               (json-par--forward-spaces)
                               (memq (char-after) '(?\] ?\) ?}))))))
          (json-par--delete-newline-and-following-spaces)))
      (when (and (zerop min-level)
                 (json-par-token-close-bracket-p token))
        (save-excursion
          (goto-char (json-par-token-start token))
          (unless (memq (char-before) '(?\[ ?\( ?{ ?\s ?\t))
            (insert-char ?\s)))))))

(defun json-par--delete-newline-and-following-spaces ()
  "Delete line breaks and following spaces after the point."
  (while (progn
           (skip-chars-forward "\s\t")
           (when (eq (char-after) ?\n)
             (forward-char)
             (delete-horizontal-space)
             (delete-char -1))
           (or (and (looking-at "/[*/]")
                    (forward-comment 1))
               (eq (char-after) ?\n)))
    (when (and (eq (char-before) ?/)
               (eq (char-before (1- (point))) ?*)
               (eq (char-after) ?\n))
      (insert-char ?\s))
    (when (and (bolp) (not (eq (char-after) ?\n)))
      (json-par-indent-line))))

(defun json-par--post-newline ()
  "Smart newline for arrays and objects.

`json-par-action-when-breaking-line-at-just-inside-brackets' and
`json-par-action-when-breaking-line-after-first-member' affects the behavior.

Examples (`|' is the point):

  [|]
  ↓
  [
    |
  ]

  [| 1, 2, 3 ]
  ↓
  [
    |1, 2, 3
  ]

  [
    1, |2, 3
  ]
  ↓
  [
    1,
    |2,
    3
  ]"
  (let ((previous-token (save-excursion (json-par-backward-token)))
        (next-token (save-excursion (json-par-forward-token)))
        whole-list-token
        end)
    (cond
     ;; After the first member.
     ((and (eq json-par-action-when-breaking-line-after-first-member
               'break-each-member)
           (json-par--before-second-member-p previous-token)
           (json-par--all-members-on-same-line-after-point-p))
      (save-excursion
        (json-par-up-backward)
        (json-par-multiline 1)))

     ;; Just inside brackets and the array/object was single line.
     ((and (eq json-par-action-when-breaking-line-at-just-inside-brackets
               'break-each-member)
           (or (and (json-par-token-open-bracket-p previous-token)
                    ;; This array/object was single line.
                    (= (save-excursion
                         (goto-char (json-par-token-start previous-token))
                         (forward-line)
                         (line-beginning-position))
                       (save-excursion
                         (goto-char (json-par-token-start previous-token))
                         (unless (json-par-token-matching-brackets-p
                                  (json-par-forward-token-or-list))
                           (goto-char (point-max)))
                         (line-beginning-position))))
               (and (json-par-token-close-bracket-p next-token)
                    ;; This array/object was single line.
                    (= (save-excursion
                         (goto-char (json-par-token-end next-token))
                         (forward-line -1)
                         (line-beginning-position))
                       (save-excursion
                         (goto-char (json-par-token-end next-token))
                         (unless (json-par-token-matching-brackets-p
                                  (json-par-backward-token-or-list))
                           (goto-char (point-min)))
                         (line-beginning-position))))))
      (save-excursion
        (json-par-up-backward)
        (json-par-multiline 1)))

     ;; [
     ;;   |1, 2, 3 ]
     ;; ↓
     ;; [
     ;;   1, 2, 3
     ;; ]
     ;;
     ;; [ 1, 2, 3
     ;; |]
     ;; ↓
     ;; [
     ;;   1, 2, 3
     ;; ]
     ;;
     ;; [ 1, 2,
     ;;   |3 ]
     ;; ↓
     ;; [
     ;;   1, 2,
     ;;   3
     ;; ]
     ((progn
        (setq whole-list-token
              (save-excursion
                (json-par-up-backward)
                (json-par-forward-token-or-list)))
        (json-par-token-matching-brackets-p whole-list-token))
      (setq end (copy-marker (json-par-token-end whole-list-token)))
      (save-excursion
        (goto-char (json-par-token-start whole-list-token))
        (forward-char)
        (when (save-excursion
                (skip-chars-forward "\s\t")
                (not (eolp)))
          (json-par--open-line-and-indent-both)))
      (json-par-indent-line)
      (save-excursion
        (goto-char end)
        (backward-char)
        (when (save-excursion
                (skip-chars-backward "\s\t")
                (not (bolp)))
          (json-par--open-line-and-indent-both)))
      (json-par--free-marker end))

     (t
      nil))))

(defun json-par--pretty-print-long-line ()
  "Format buffer containing long JSON lines.

Lightweight version of `json-pretty-print-buffer'.

This function is less accurate if the JSON lines is already formatted
partially."
  (json-par--huge-edit (point-min) (point-max)
    (save-excursion
      (goto-char (point-min))
      (let ((inhibit-changing-match-data t)
            (inhibit-modification-hooks t)
            (level 0)
            (offset (if (boundp 'js-indent-level)
                        (symbol-value 'js-indent-level)
                      2))
            char
            match-start
            match-end
            empty)
        (while (search-forward-regexp "[][\"{}:,/]" nil t)
          (setq char (char-before))
          (cond
           ((or (eq char ?\[)
                (eq char ?{))
            (setq match-end (point))
            (skip-chars-forward "\s\t")
            (setq empty (or (eq (char-after) ?\])
                            (eq (char-after) ?\})))
            (unless empty
              (setq level (1+ level)))
            (unless (eolp)
              (delete-region match-end (point))
              (insert ?\n)
              (indent-to (* level offset)))
            (when empty
              (forward-char)))

           ((or (eq char ?\])
                (eq char ?}))
            (setq level (1- level))
            (backward-char)
            (setq match-start (point))
            (skip-chars-backward "\s\t")
            (unless (bolp)
              (delete-region (point) match-start)
              (insert ?\n)
              (indent-to (* level offset)))
            (forward-char))

           ((eq char ?,)
            (setq match-end (point))
            (skip-chars-forward "\s\t")
            (unless (eolp)
              (delete-region match-end (point))
              (insert ?\n)
              (indent-to (* level offset))))

           ((eq char ?:)
            (unless (or (eq (char-after) ?\n)
                        (eq (char-after) ?\s)
                        (eq (char-after) ?\t))
              (insert ?\s)))

           ((eq char ?\")
            (json-par--skip-string))

           ((and (eq char ?/)
                 (eq (char-after) ?/))
            (end-of-line))

           ((and (eq char ?/)
                 (eq (char-after) ?*))
            (forward-char)
            (json-par--skip-multiline-comment))))))))

(defun json-par--check-long-line ()
  "If the first line of the buffer is long, pretty print it.

The user is asked to do so if the first line is longer than
`json-par-long-line-threshold'."
  (save-excursion
    (goto-char (point-min))
    (when (and (> (buffer-size) json-par-long-line-threshold)
               (not (search-forward "\n"
                                    (+ (point) json-par-long-line-threshold)
                                    t))
               (y-or-n-p "The buffer seems long.  Pretty print it? "))
      (json-par--pretty-print-long-line))))

(provide 'json-par-oneline-multiline)

;;; json-par-oneline-multiline.el ends here
