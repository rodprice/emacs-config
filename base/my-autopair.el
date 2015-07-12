;;; Textmate-like behavior for matching parens, etc.
;; Rod Price, Nov 2009


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoloads and requires
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'autopair)
(autopair-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;(global-set-key [backspace] 'autopair-backspace)

;; (global-set-key "("  'autopair-insert)
;; (global-set-key ")"  'autopair-insert)
;; (global-set-key "["  'autopair-insert)
;; (global-set-key "]"  'autopair-insert)
;; (global-set-key "{"  'autopair-insert)
;; (global-set-key "}"  'autopair-insert)
;; (global-set-key "\"" 'autopair-insert)
;; ;; (global-set-key "\'" 'autopair-insert)

;; (global-set-key (kbd "C-o") 'open-next-line)
;; (global-set-key (kbd "M-o") 'open-previous-line)
;; (global-set-key (kbd "C-<return>") 'open-next-line)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Make autopairs work in shell script mode, too
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'sh-script)

;; (defun rm-local-pair-bindings ()
;;   "Remove key bindings for matching parens, etc that sh-script creates."
;;   (setq skeleton-pair-alist autopair-default-alist
;;         skeleton-pair-default-alist autopair-default-alist)
;;   (mapcar 'local-unset-key autopair-keys))

;; ;; Stop shell script mode from stealing autopair's keys
;; (setq sh-mode-hook 'rm-local-pair-bindings)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; From http://www.emacswiki.org/emacs/AutoPairs
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Pairs to match
;; (setq autopair-default-alist '((?\( _ ?\))
;;                                (?[  _ ?])
;;                                (?{  _ ?})
;;                                (?\" _ ?\")
;;                                (?\' _ ?\')))

;; ;; Use emacs' builtin skeleton functionality
;; (setq skeleton-pair t
;;       skeleton-pair-alist autopair-default-alist
;;       skeleton-pair-default-alist autopair-default-alist
;;       autopair-keys (mapcar (lambda (elem) (string (car elem)))
;;                             skeleton-pair-alist))

;; (defun autopair-insert (arg)
;;   "Automatically insert the closing character of an autopair when
;; the opening character is inserted."
;;   (interactive "P")
;;   (let (pair)
;;     (cond
;;      ((assq last-command-char skeleton-pair-alist)
;;       (autopair-open arg))
;;      (t
;;       (autopair-close arg)))))

;; ;; TODO ((sexp) -> ((sexp)) on second open paren
;; ;; TODO but (()) -> ((())) on second open paren
;; (defun autopair-open (arg)
;;   "Insert the opening character of an autopair at point."
;;   (interactive "P")
;;   (let* ((pair (assq last-command-char skeleton-pair-alist))
;;          (open (car pair))
;;          (close (car (last pair))))
;;     (cond
;;      ((and (not mark-active)
;;            (eq open close)
;;            (eq open (char-after)))
;;       (autopair-close arg))
;;      ((autopair-enclose-p open close)
;;       (delete-char 1)
;;       (forward-sexp)
;;       (insert (string close)))
;;      ((autopair-in-string-p)
;;       (autopair-close arg))
;;      (t
;;       (skeleton-pair-insert-maybe arg)))))

;; (defun autopair-in-string-p ()
;;   "Determine whether point is inside a string."
;;   (and (eq 'font-lock-string-face
;;            (get-text-property (point) 'face))
;;        (eq ?\" last-command-char)))

;; (defun autopair-enclose-p (open close)
;;   "Determine whether point is inside an empty autopair."
;;   (unless (eq open close)
;;     (let ((first (char-after (point)))
;;           (second (char-after (1+ (point)))))
;;       (and (eq first close)
;;            (eq second open)))))

;; (defun autopair-close (arg)
;;   "Insert close character unless it has already been inserted by
;; autopair."
;;   (interactive "P")
;;   (cond
;;    (mark-active
;;     (let (pair open)
;;       (dolist (pair skeleton-pair-alist)
;;         (when (eq last-command-char (car (last pair)))
;;           (setq open (car pair))))
;;       (setq last-command-char open)
;;       (skeleton-pair-insert-maybe arg)))
;;    ((looking-at
;;      (concat "[ \t\n]*" (regexp-quote (string last-command-char))))
;;     (replace-match (string last-command-char)))
;;    (t
;;     (self-insert-command (prefix-numeric-value arg)))))

;; (defadvice backward-delete-char-untabify
;;   (before autopair-backspace (arg) activate)
;;   "If inside an empty pair, delete both open and close characters."
;;   (interactive "p")
;;   (if (eq (char-after)
;;           (car (last (assq (char-before) skeleton-pair-alist))))
;;       (and (char-after) (delete-char 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Try, try again
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; All languages:
;; (setq skeleton-pair t)
;; (global-set-key "(" 'skeleton-pair-insert-maybe)
;; (global-set-key "[" 'skeleton-pair-insert-maybe)
;; (global-set-key "{" 'skeleton-pair-insert-maybe)
;; (global-set-key "\"" 'skeleton-pair-insert-maybe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From http://www.emacswiki.org/emacs/OpenNextLine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions below to
  cause them to autoindent.")

;; behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then open a line.

 See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.

  See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(provide 'my-autopair)
