;;; treesit-fold.el --- Code folding using tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Junyi Hou
;; Copyright (C) 2021-2023  Shen, Jen-Chieh

;; Created date 2021-08-11 14:12:37

;; Author: Junyi Hou <junyi.yi.hou@gmail.com>
;;         Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-tree-sitter/treesit-fold
;; Version: 0.2.1
;; Package-Requires: ((emacs "29") (fringe-helper "1.0.1"))
;; Keywords: convenience folding tree-sitter

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides a code-folding mechanism based on tree-sitter
;; package.  Turn on the minor-mode `treesit-fold-mode' to enable
;; this mechanism.  Note that all functionalities provided here based on the
;; `tree-sitter-mode', and thus it should be enabled before
;; `treesit-fold-mode' can properly fold codes.

;;; Code:

(require 'seq)
(require 'subr-x)

(require 'treesit-fold-util)
(require 'treesit-fold-parsers)
(require 'treesit-fold-summary)

;;
;; (@* "Customization" )
;;

(defgroup treesit-fold nil
  "Code folding using builtin treesit."
  :group 'treesit-fold
  :prefix "treesit-fold-")

;; TODO(everyone): This is a bit messy, but try to keep this alist
;; alphabetically sorted
(defcustom treesit-fold-range-alist
  `((agda-mode              . ,(treesit-fold-parsers-agda))
    (arduino-mode           . ,(treesit-fold-parsers-arduino))
    (awk-mode               . ,(treesit-fold-parsers-c))
    (awk-ts-mode            . ,(treesit-fold-parsers-c))
    (fasm-mode              . ,(treesit-fold-parsers-asm))
    (masm-mode              . ,(treesit-fold-parsers-asm))
    (nasm-mode              . ,(treesit-fold-parsers-asm))
    (bash-ts-mode           . ,(treesit-fold-parsers-bash))
    (beancount-mode         . ,(treesit-fold-parsers-beancount))
    (beancount-ts-mode      . ,(treesit-fold-parsers-beancount))
    (c-mode                 . ,(treesit-fold-parsers-c))
    (c-ts-mode              . ,(treesit-fold-parsers-c))
    (c++-mode               . ,(treesit-fold-parsers-c++))
    (c++-ts-mode            . ,(treesit-fold-parsers-c++))
    (caml-mode              . ,(treesit-fold-parsers-ocaml))
    (caml-ts-mode           . ,(treesit-fold-parsers-ocaml))
    (cmake-mode             . ,(treesit-fold-parsers-cmake))
    (cmake-ts-mode          . ,(treesit-fold-parsers-cmake))
    (clojure-mode           . ,(treesit-fold-parsers-clojure))
    (clojure-ts-mode        . ,(treesit-fold-parsers-clojure))
    (csharp-mode            . ,(treesit-fold-parsers-csharp))
    (csharp-ts-mode         . ,(treesit-fold-parsers-csharp))
    (css-mode               . ,(treesit-fold-parsers-css))
    (css-ts-mode            . ,(treesit-fold-parsers-css))
    (dart-mode              . ,(treesit-fold-parsers-dart))
    (dart-ts-mode           . ,(treesit-fold-parsers-dart))
    (emacs-lisp-mode        . ,(treesit-fold-parsers-elisp))
    (elixir-mode            . ,(treesit-fold-parsers-elixir))
    (elixir-ts-mode         . ,(treesit-fold-parsers-elixir))
    (erlang-mode            . ,(treesit-fold-parsers-erlang))
    (erlang-ts-mode         . ,(treesit-fold-parsers-erlang))
    (ess-r-mode             . ,(treesit-fold-parsers-r))
    (fish-mode              . ,(treesit-fold-parsers-fish))
    (gdscript-mode          . ,(treesit-fold-parsers-gdscript))
    (gdscript-ts-mode       . ,(treesit-fold-parsers-gdscript))
    (glsl-mode              . ,(treesit-fold-parsers-glsl))
    (go-mode                . ,(treesit-fold-parsers-go))
    (go-ts-mode             . ,(treesit-fold-parsers-go))
    (go-mod-ts-mode         . ,(treesit-fold-parsers-go))
    (groovy-mode            . ,(treesit-fold-parsers-groovy))
    (jenkinsfile-mode       . ,(treesit-fold-parsers-groovy))
    (haskell-mode           . ,(treesit-fold-parsers-haskell))
    (haskell-ts-mode        . ,(treesit-fold-parsers-haskell))
    (hlsl-mode              . ,(treesit-fold-parsers-hlsl))
    (hlsl-ts-mode           . ,(treesit-fold-parsers-hlsl))
    (html-mode              . ,(treesit-fold-parsers-html))
    (html-ts-mode           . ,(treesit-fold-parsers-html))
    (jai-mode               . ,(treesit-fold-parsers-jai))
    (java-mode              . ,(treesit-fold-parsers-java))
    (java-ts-mode           . ,(treesit-fold-parsers-java))
    (javascript-mode        . ,(treesit-fold-parsers-javascript))
    (js-mode                . ,(treesit-fold-parsers-javascript))
    (js-ts-mode             . ,(treesit-fold-parsers-javascript))
    (js2-mode               . ,(treesit-fold-parsers-javascript))
    (js3-mode               . ,(treesit-fold-parsers-javascript))
    (json-mode              . ,(treesit-fold-parsers-json))
    (json-ts-mode           . ,(treesit-fold-parsers-json))
    (jsonc-mode             . ,(treesit-fold-parsers-json))
    (jsonnet-mode           . ,(treesit-fold-parsers-jsonnet))
    (julia-mode             . ,(treesit-fold-parsers-julia))
    (julia-ts-mode          . ,(treesit-fold-parsers-julia))
    (kotlin-mode            . ,(treesit-fold-parsers-kotlin))
    (latex-mode             . ,(treesit-fold-parsers-latex))
    (latex-ts-mode          . ,(treesit-fold-parsers-latex))
    (lisp-mode              . ,(treesit-fold-parsers-lisp))
    (lisp-interaction-mode  . ,(treesit-fold-parsers-lisp))
    (lua-mode               . ,(treesit-fold-parsers-lua))
    (lua-ts-mode            . ,(treesit-fold-parsers-lua))
    (makefile-mode          . ,(treesit-fold-parsers-make))
    (makefile-ts-mode       . ,(treesit-fold-parsers-make))
    (makefile-automake-mode . ,(treesit-fold-parsers-make))
    (makefile-gmake-mode    . ,(treesit-fold-parsers-make))
    (makefile-makepp-mode   . ,(treesit-fold-parsers-make))
    (makefile-bsdmake-mode  . ,(treesit-fold-parsers-make))
    (makefile-imake-mode    . ,(treesit-fold-parsers-make))
    (markdown-mode          . ,(treesit-fold-parsers-markdown))
    (noir-mode              . ,(treesit-fold-parsers-noir))
    (nix-mode               . ,(treesit-fold-parsers-nix))
    (ocaml-mode             . ,(treesit-fold-parsers-ocaml))
    (ocaml-ts-mode          . ,(treesit-fold-parsers-ocaml))
    (org-mode               . ,(treesit-fold-parsers-org))
    (pascal-mode            . ,(treesit-fold-parsers-pascal))
    (pascal-ts-mode         . ,(treesit-fold-parsers-pascal))
    (perl-mode              . ,(treesit-fold-parsers-perl))
    (perl-ts-mode           . ,(treesit-fold-parsers-perl))
    (php-mode               . ,(treesit-fold-parsers-php))
    (php-ts-mode            . ,(treesit-fold-parsers-php))
    (python-mode            . ,(treesit-fold-parsers-python))
    (python-ts-mode         . ,(treesit-fold-parsers-python))
    (rjsx-mode              . ,(treesit-fold-parsers-javascript))
    (rst-mode               . ,(treesit-fold-parsers-rst))
    (ruby-mode              . ,(treesit-fold-parsers-ruby))
    (ruby-ts-mode           . ,(treesit-fold-parsers-ruby))
    (rust-mode              . ,(treesit-fold-parsers-rust))
    (rust-ts-mode           . ,(treesit-fold-parsers-rust))
    (rustic-mode            . ,(treesit-fold-parsers-rust))
    (scheme-mode            . ,(treesit-fold-parsers-scheme))
    (scheme-ts-mode         . ,(treesit-fold-parsers-scheme))
    (sh-mode                . ,(treesit-fold-parsers-bash))
    (scala-mode             . ,(treesit-fold-parsers-scala))
    (sql-mode               . ,(treesit-fold-parsers-sql))
    (swift-mode             . ,(treesit-fold-parsers-swift))
    (toml-mode              . ,(treesit-fold-parsers-toml))
    (toml-ts-mode           . ,(treesit-fold-parsers-toml))
    (conf-toml-mode         . ,(treesit-fold-parsers-toml))
    (tuareg-mode            . ,(treesit-fold-parsers-ocaml))
    (typescript-mode        . ,(treesit-fold-parsers-typescript))
    (tsx-ts-mode            . ,(treesit-fold-parsers-typescript))
    (verilog-mode           . ,(treesit-fold-parsers-verilog))
    (vhdl-mode              . ,(treesit-fold-parsers-vhdl))
    (nxml-mode              . ,(treesit-fold-parsers-xml))
    (xml-ts-mode            . ,(treesit-fold-parsers-xml))
    (yaml-mode              . ,(treesit-fold-parsers-yaml))
    (k8s-mode               . ,(treesit-fold-parsers-yaml))
    (zig-mode               . ,(treesit-fold-parsers-zig)))
  "An alist of (major-mode . (foldable-node-type . function)).

FUNCTION is used to determine where the beginning and end for FOLDABLE-NODE-TYPE
in MAJOR-MODE.  It should take a single argument (the syntax node with type
FOLDABLE-NODE-TYPE) and return the buffer positions of the beginning and end of
the fold in a cons cell.  See `treesit-fold-range-python-def' for an example."
  :type '(alist :key-type symbol
          :value-type (alist :key-type symbol :value-type function))
  :group 'treesit-fold)

(defcustom treesit-fold-mode-hook nil
  "Hook to run when enabling `treesit-fold-mode`."
  :type 'hook
  :group 'treesit-fold)

(defcustom treesit-fold-on-next-line t
  "If non-nil, we leave ending keywords on the next line.

This is only used in languages that uses keyword to end the scope.
For example, Lua, Ruby, etc."
  :type 'boolean
  :group 'treesit-fold)

(defcustom treesit-fold-replacement "..."
  "Show this string instead of the folded text."
  :type 'string
  :group 'treesit-fold)

(defface treesit-fold-replacement-face
  '((t :foreground "#808080" :box '(:line-width -1 :style 'pressed-button)))
  "Face used to display the fold replacement text."
  :group 'treesit-fold)

(defface treesit-fold-fringe-face
  '((t ()))
  "Face used to display fringe contents."
  :group 'treesit-fold)

(defcustom treesit-fold-modes '(prog-mode conf-mode python-ts-mode)
  "Modes in which `treesit-fold-mode' gets enabled.")

;;
;; (@* "Externals" )
;;

;;
;; (@* "Entry" )
;;

(defun treesit-fold--enable ()
  "Start folding minor mode."
  (setq-local line-move-ignore-invisible t)
  (add-to-invisibility-spec '(treesit-fold . t))

  (when (bound-and-true-p hs-minor-mode)
    (hs-minor-mode -1))

  ;; evil integration
  (when (bound-and-true-p evil-fold-list)
    (add-to-list 'evil-fold-list
                 '((treesit-fold-mode)
                   :toggle treesit-fold-toggle
                   :open treesit-fold-open
                   :close treesit-fold-close
                   :open-rec treesit-fold-open-recursively
                   :open-all treesit-fold-open-all
                   :close-all treesit-fold-close-all))))

(defun treesit-fold--disable ()
  "Stop folding minor mode."
  (remove-from-invisibility-spec '(treesit-fold . t))
  (treesit-fold-open-all))

(defun treesit-fold-ready-p ()
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (ignore-errors (treesit-buffer-root-node))
       t))

(defun treesit-fold--trigger ()
  "Turn `treesit-fold-mode' on and off when the current mode is treesit-fold compatible."
  (if (and (treesit-fold-ready-p) (treesit-fold-usable-mode-p))
      (treesit-fold-mode 1)
    (treesit-fold-mode -1)))

;;;###autoload
(define-minor-mode treesit-fold-mode
  "Folding code using tree sitter."
  :group 'treesit-fold
  :init-value nil
  :lighter "Treesit-Fold"
  (if treesit-fold-mode (treesit-fold--enable) (treesit-fold--disable)))

;;;###autoload
(define-minor-mode global-treesit-fold-mode
  "Use `treesit-fold-mode' wherever possible."
  :group 'treesit-fold
  :init-value nil
  :lighter nil
  :global t
  (if global-treesit-fold-mode
      (progn
        (dolist (hook (mapcar (lambda (m) (intern (format "%s-hook" m))) treesit-fold-modes))
          (add-hook hook #'treesit-fold--trigger))
        ;; Try to turn on in all buffers.
        (dolist (buf (buffer-list)) (with-current-buffer buf (treesit-fold--trigger))))
    (dolist (hook (mapcar (lambda (m) (intern (format "%s-hook" m))) treesit-fold-modes))
      (remove-hook hook #'treesit-fold--trigger))))

(defun treesit-fold-usable-mode-p (&optional mode)
  "Return non-nil if `treesit-fold' has defined folds for MODE."
  (let ((mode (or mode major-mode)))
    (alist-get mode treesit-fold-range-alist)))

;;
;; (@* "Core" )
;;

(defun treesit-fold--range-on-same-line (range)
  "Return non-nil if RANGE is on the same line."
  (let ((beg (car range))
        (end (cdr range))
        (lbp) (lep))
    (save-excursion
      (goto-char beg)
      (setq lbp (line-beginning-position)
            lep (line-end-position)))
    (and (<= lbp beg) (<= beg lep)
         (<= lbp end) (<= end lep))))

(defun treesit-fold--get-fold-range (node)
  "Return the beginning (as buffer position) of fold for NODE.
Return nil if there is no fold to be made."
  (when-let* ((fold-alist (alist-get major-mode treesit-fold-range-alist))
              (fold-func (alist-get (intern (treesit-node-type node)) fold-alist)))
    (cond ((functionp fold-func) (funcall fold-func node (cons 0 0)))
          ((listp fold-func) (funcall (nth 0 fold-func) node (cons (nth 1 fold-func) (nth 2 fold-func))))
          (t (user-error "Bad folding function for node")))))

(defun treesit-fold--non-foldable-node-p (node mode-ranges)
  "Return non-nil if NODE is a non-foldable in MODE-RANGES."
  (or (not (alist-get (intern (treesit-node-type node)) mode-ranges))  ; Not registered, continue.
      (let ((range (treesit-fold--get-fold-range node)))
        (or (not range)                                   ; Range not defined, continue.
            (treesit-fold--range-on-same-line range)))))       ; On same line, continue.

(defun treesit-fold--foldable-node-at-pos (&optional pos)
  "Return the smallest foldable node at POS.  If POS is nil, use `point'.

Return nil if no valid node is found.

This function is borrowed from `tree-sitter-node-at-point'."
  (let* ((pos (or pos (point)))
         (mode-ranges (alist-get major-mode treesit-fold-range-alist))
         (root (treesit-buffer-root-node))
         (node (treesit-node-descendant-for-range root pos pos))
         ;; Used for looping
         (current node))
    (while (and current
                (treesit-fold--non-foldable-node-p current mode-ranges))
      (setq current (treesit-node-parent current)))
    current))

;;
;; (@* "Overlays" )
;;

(defun treesit-fold--create-overlay (range)
  "Create invisible overlay in RANGE."
  (when range
    (let* ((beg (car range))
           (end (cdr range))
           (ov (make-overlay beg end)))
      (overlay-put ov 'creator 'treesit-fold)
      (overlay-put ov 'invisible 'treesit-fold)
      (overlay-put ov 'display (or (and treesit-fold-summary-show
                                        (treesit-fold-summary--get (buffer-substring beg end)))
                                   treesit-fold-replacement))
      (overlay-put ov 'face 'treesit-fold-replacement-face)
      (overlay-put ov 'isearch-open-invisible #'treesit-fold--isearch-open))))

(defun treesit-fold--isearch-open (ov)
  "Open overlay OV during `isearch' session."
  (delete-overlay ov))

(defun treesit-fold-overlay-at (node)
  "Return the treesit-fold overlay at NODE if NODE is foldable and folded.
Return nil otherwise."
  (when-let* ((range (treesit-fold--get-fold-range node)))
    (thread-last (overlays-in (car range) (cdr range))
                 (seq-filter (lambda (ov)
                               (and (eq (overlay-get ov 'invisible) 'treesit-fold)
                                    (= (overlay-start ov) (car range))
                                    (= (overlay-end ov) (cdr range)))))
                 car)))

;;
;; (@* "Commands" )
;;

(defmacro treesit-fold--ensure-treesit (&rest body)
  "Run BODY only when `treesit' is available."
  (declare (indent 0))
  `(when (treesit-fold-ready-p) ,@body))

;;;###autoload
(defun treesit-fold-close (&optional node)
  "Fold the syntax node at `point` if it is foldable.

Foldable nodes are defined in `treesit-fold-range-alist' for the
current `major-mode'.

If no NODE is found in point, do nothing."
  (interactive)
  (treesit-fold--ensure-treesit
    (when-let* ((node (or node (treesit-fold--foldable-node-at-pos))))
      ;; make sure I do not create multiple overlays for the same fold
      (when-let* ((ov (treesit-fold-overlay-at node)))
        (delete-overlay ov))
      (when-let* ((range (treesit-fold--get-fold-range node)))
        (treesit-fold--create-overlay range)))))

;;;###autoload
(defun treesit-fold-open ()
  "Open the fold of the syntax node in which `point' resides.
If the current node is not folded or not foldable, do nothing."
  (interactive)
  (treesit-fold--ensure-treesit
    (when-let* ((node (treesit-fold--foldable-node-at-pos))
                (ov (treesit-fold-overlay-at node)))
      (delete-overlay ov)
      t)))

;;;###autoload
(defun treesit-fold-open-recursively ()
  "Open recursively folded syntax NODE that are contained in the node at point."
  (interactive)
  (treesit-fold--ensure-treesit
    (when-let* ((node (treesit-fold--foldable-node-at-pos))
                (beg (treesit-node-start node))
                (end (treesit-node-end node)))
      (thread-last (overlays-in beg end)
                   (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'treesit-fold)))
                   (mapc #'delete-overlay)))))

;;;###autoload
(defun treesit-fold-close-all ()
  "Fold all foldable syntax nodes in the buffer."
  (interactive)
  (treesit-fold--ensure-treesit
    (let* ((node (treesit-buffer-root-node))
           (patterns (seq-mapcat (lambda (fold-range) `((,(car fold-range)) @name))
                                 (alist-get major-mode treesit-fold-range-alist)))
           (query (treesit-query-compile (treesit-node-language node) patterns))
           (nodes-to-fold (treesit-query-capture node query nil nil t)))
      (thread-last nodes-to-fold (mapc #'treesit-fold-close)))))

;;;###autoload
(defun treesit-fold-open-all ()
  "Unfold all syntax nodes in the buffer."
  (interactive)
  (treesit-fold--ensure-treesit
    (thread-last (overlays-in (point-min) (point-max))
                 (seq-filter (lambda (ov) (eq (overlay-get ov 'invisible) 'treesit-fold)))
                 (mapc #'delete-overlay))))

;;;###autoload
(defun treesit-fold-toggle ()
  "Toggle the syntax node at `point'.
If the current syntax node is not foldable, do nothing."
  (interactive)
  (treesit-fold--ensure-treesit
    (if-let* ((node (treesit-fold--foldable-node-at-pos (point)))
              (ov (treesit-fold-overlay-at node)))
        (progn (delete-overlay ov) t)
      (treesit-fold-close))))

(defun treesit-fold--after-command (&rest _)
  "Function call after interactive commands.")

(let ((commands '(treesit-fold-close
                  treesit-fold-open
                  treesit-fold-open-recursively
                  treesit-fold-close-all
                  treesit-fold-open-all
                  treesit-fold-toggle)))
  (dolist (command commands)
    (advice-add command :after #'treesit-fold--after-command)))

;;
;; (@* "Rule Helpers" )
;;

(defun treesit-fold--next-prev-node (node next)
  "Return previous/next sibling node starting from NODE.

If NEXT is non-nil, return next sibling.  Otherwirse, return previouse sibling."
  (if next (treesit-node-next-sibling node) (treesit-node-prev-sibling node)))

(defun treesit-fold--next-prev-node-skip-newline (node next)
  "Like function `treesit-fold--next-prev-node'.

For arguments NODE and NEXT, please see the function `treesit-fold--next-prev-node'
for more information."
  (let ((iter-node (treesit-fold--next-prev-node node next)))
    (while (and iter-node
                (equal "\n" (treesit-node-text iter-node)))
      (setq iter-node (treesit-fold--next-prev-node iter-node next)))
    iter-node))

(defun treesit-fold--continuous-node-prefix (node prefix next)
  "Iterate through node starting from NODE and compare node-text to PREFIX;
then return the last iterated node.

Argument NEXT is a boolean type.  If non-nil iterate forward; otherwise iterate
in backward direction."
  (let* ((iter-node node)
         (last-node node)
         (last-line (treesit-node-start node))
         (line-range 1)
         (last-line-range 1)
         (indentation (treesit-fold--indentation (treesit-node-start iter-node)))
         max-line-range line text break next-indentation)
    (while (and iter-node (not break))
      (setq text (string-trim (treesit-node-text iter-node))
            line (treesit-node-start iter-node)
            line-range (1+ (treesit-fold--count-matches "\n" text))
            max-line-range (max line-range last-line-range)
            next-indentation (treesit-fold--indentation (treesit-node-start iter-node)))
      (if (and (treesit-fold--in-range-p line (- last-line max-line-range) (+ last-line max-line-range))
               (string-prefix-p prefix text)
               (= indentation next-indentation))
          (setq last-node iter-node last-line line
                last-line-range (1+ (treesit-fold--count-matches "\n" text)))
        (setq break t))
      (setq iter-node (treesit-fold--next-prev-node-skip-newline iter-node next)))
    last-node))

(defun treesit-fold-range-seq (node offset)
  "Return the fold range in sequence starting from NODE.

Argument OFFSET can be used to tweak the final beginning and end position."
  (let ((beg (1+ (treesit-node-start node)))
        (end (1- (treesit-node-end node))))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-markers (node offset start-seq &optional end-seq)
  "Return the fold range for NODE with an OFFSET where the range starts at
the end of the first occurence of START-SEQ and ends at the end of the node
or the start of the last occurence of the optional parameter LAST-SEQ.

START-SEQ and LAST-SEQ can be named tree-sitter nodes or anonomous nodes.

If no occurence is found for START-SEQ or END-SEQ or the
occurences overlap, then the range returned is nil."
  (when start-seq
    (when-let ((beg-node (car (treesit-fold-find-children node start-seq)))
               (end-node (if end-seq
                             (car (last (treesit-fold-find-children node end-seq)))
                           node))
               (beg (treesit-node-end beg-node))
               (end (if end-seq
                        (treesit-node-start end-node)
                      (1- (treesit-node-end node)))))
      (unless (> beg end) (treesit-fold--cons-add (cons beg end) offset)))))

(defun treesit-fold-range-line-comment (node offset prefix)
  "Define fold range for line comment.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information.

Argument PREFIX is the comment prefix in string."
  (save-excursion
    (when-let* ((first-node (treesit-fold--continuous-node-prefix node prefix nil))
                (last-node (treesit-fold--continuous-node-prefix node prefix t))
                (prefix-len (length prefix))
                (beg (+ (treesit-node-start first-node) prefix-len))
                (end (treesit-node-end last-node)))
      (treesit-fold--cons-add (cons beg end) offset))))

(defun treesit-fold-range-block-comment (node offset)
  "Define fold range for block comment.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (treesit-fold-range-seq node (treesit-fold--cons-add '(1 . -1) offset)))

(defun treesit-fold-range-c-like-comment (node offset)
  "Define fold range for C-like comemnt.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let ((text (treesit-node-text node)))
    (if (and (string-match-p "\n" text) (string-prefix-p "/*" text))
        (treesit-fold-range-block-comment node offset)
      (if (string-prefix-p "///" text)
          (treesit-fold-range-line-comment node offset "///")
        (treesit-fold-range-line-comment node offset "//")))))

;;
;; (@* "Languages" )
;;

(defun treesit-fold-range-asm--find-last-instruction (node)
  "Find the last instruction node by starting NODE."
  (let* ((iter-node (treesit-fold--next-prev-node-skip-newline node t))
         (last iter-node))
    (while (and iter-node (not (member (treesit-node-type iter-node) '("label"))))
      (setq last iter-node
            iter-node (treesit-fold--next-prev-node-skip-newline iter-node t)))
    last))  ; return last insturction node

(defun treesit-fold-range-asm-label (node offset)
  "Define fold range for `label' in Assembly.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((beg (treesit-node-end node))
              (end (treesit-fold-range-asm--find-last-instruction node))
              (end (treesit-node-end end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-beancount-transaction (node offset)
  "Define fold range for `transaction' in Beancount.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((beg (treesit-node-start node))
              (beg (treesit-fold--eol beg))
              (end (1- (treesit-node-end node))))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-c-preproc-if (node offset)
  "Define fold range for `if' preprocessor.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let* ((named-node (treesit-node-child-by-field-name node "condition"))
         (else (or (treesit-node-child-by-field-name node "alternative")
                   (car (treesit-fold-find-children node "#endif"))))
         (beg (treesit-node-end named-node))
         (end (1- (treesit-node-start else))))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-c-preproc-ifdef (node offset)
  "Define fold range for `ifdef' and `ifndef' preprocessor.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((named-node (treesit-node-child-by-field-name node "name"))
              (else (or (treesit-node-child-by-field-name node "alternative")
                        (car (treesit-fold-find-children node "#endif"))))
              (beg (treesit-node-end named-node))
              (end (1- (treesit-node-start else))))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-c-preproc-elif (node offset)
  "Define fold range for `elif' preprocessor.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((named-node (treesit-node-child-by-field-name node "condition"))
              (parent (or (treesit-fold-find-parent node "preproc_if")
                          (treesit-fold-find-parent node "preproc_ifdef")))
              (next (or (treesit-node-child-by-field-name node "alternative")
                        (car (treesit-fold-find-children parent "#endif"))))
              (beg (treesit-node-end named-node))
              (end (1- (treesit-node-start next))))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-c-preproc-else (node offset)
  "Define fold range for `else' preprocessor.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((else-str (car (split-string (treesit-node-text node) "\n")))
              (parent (or (treesit-fold-find-parent node "preproc_if")
                          (treesit-fold-find-parent node "preproc_ifdef")))
              (next (car (treesit-fold-find-children parent "#endif")))
              (beg (+ (treesit-node-start node) (length else-str)))
              (end (1- (treesit-node-start next))))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-clojure-function (node offset)
  "Return the fold range for `list_lit' NODE in Clojure.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((param-node (car (treesit-fold-find-children node "vec_lit")))
              (next-node (treesit-node-next-sibling param-node))
              (beg (treesit-node-start next-node))
              (end (1- (treesit-node-end node))))
    (unless treesit-fold-on-next-line  ; display nicely
      (setq beg (treesit-fold--last-eol beg)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-elisp-function (node offset)
  "Return the fold range for `macro_definition' and `function_definition' NODE
in Elisp.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((param-node (treesit-node-child node 4))
              (beg (treesit-node-start param-node))
              (end (1- (treesit-node-end node))))
    (unless treesit-fold-on-next-line  ; display nicely
      (setq beg (treesit-fold--last-eol beg)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-elixir (node offset)
  "Return the fold range for `function' `module' NODE in Elixir.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((end-child (treesit-fold-last-child node))
              (do-child (treesit-node-child node 1))
              (beg (treesit-node-start do-child))
              (beg (treesit-fold--last-eol beg))
              (end (treesit-node-start end-child)))
    (when treesit-fold-on-next-line  ; display nicely
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-erlang-signature (node offset start)
  "Return the fold range for generic signature NODE in Erlang.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information.

Argument START is a string to target for the first node we use to find the
start of the position."
  (when-let* ((start-node (car (treesit-fold-find-children node start)))
              (beg (treesit-node-end start-node))
              (end (treesit-node-end node)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-erlang-clause-body (node offset)
  "Return the fold range for `clause_body' NODE in Erlang.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (treesit-fold-range-erlang-signature node offset "->"))

(defun treesit-fold-range-erlang-type-guards (node offset)
  "Return the fold range for `type_guards' NODE in Erlang.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (treesit-fold-range-erlang-signature node offset "when"))

(defun treesit-fold-range-fish-function (node offset)
  "Define fold range for `function' in Fish.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((func-name (treesit-node-child node 1))
              (beg (treesit-node-end func-name))
              (end (treesit-node-end node))
              (end (- end 3)))
    (when treesit-fold-on-next-line  ; display nicely
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-fish-if (node offset)
  "Define fold range for `if_statement' in Fish.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((beg (treesit-node-start node))
              (beg (treesit-fold--eol beg))
              (end (treesit-node-end node))
              (end (- end 3)))
    (when treesit-fold-on-next-line  ; display nicely
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-fish-case (node offset)
  "Define fold range for `case_clause' in Fish.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((beg (treesit-node-start node))
              (beg (treesit-fold--eol beg))
              (end (treesit-node-end node))
              (end (1- end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-groovy-block (node offset)
  "Define fold range for `block' in Groovy.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((open-bracket (car (treesit-fold-find-children node "{")))
              (beg (treesit-node-start open-bracket))
              (beg (1+ beg))
              (end (treesit-node-end node))
              (end (1- end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-haskell-function (node offset)
  "Define fold range for `function' in Haskell.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((beg (treesit-node-start node))
              (beg (treesit-fold--eol beg))
              (end-node (treesit-fold-last-child node))
              (end (treesit-node-end end-node)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-html (node offset)
  "Define fold range for tag in HTML.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let* ((beg (treesit-node-end (treesit-node-child node 0)))
         (end-node (treesit-fold-last-child node))
         (end (treesit-node-start end-node)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-julia (node offset)
  "Return the fold range for a NODE in Julia.

It excludes the NODE's first child and the `end' keyword.  For
argument OFFSET, see function `treesit-fold-range-seq' for more
information."
  (let* ((identifier (treesit-node-child node 0 t))
         (end-position (treesit-node-end identifier))
         (start-position (treesit-node-start node))
         (fold-begin (1- (- end-position start-position))))
    (treesit-fold-range-seq node (treesit-fold--cons-add (cons fold-begin -2) offset))))

(defun treesit-fold-range-kotlin-when (node offset)
  "Return the fold range for `when' NODE in Kotlin.

It excludes the NODE's first child and the `end' keyword.  For
argument OFFSET, see function `treesit-fold-range-seq' for more
information."
  (when-let* ((open-bracket (car (treesit-fold-find-children node "{")))
              (beg (treesit-node-end open-bracket))
              (end (1- (treesit-node-end node))))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-lisp-function (node offset)
  "Define fold range for function in Lisp .

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((header (car (treesit-fold-find-children node "defun_header")))
              (body (treesit-node-next-sibling header))
              (beg (treesit-node-start body))
              (end (1- (treesit-node-end node))))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-lua-comment (node offset)
  "Define fold range for Lua comemnt.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let ((text (treesit-node-text node)))
    (if (and (string-match-p "\n" text) (string-prefix-p "--[[" text))
        (treesit-fold-range-block-comment node
                                          ;; XXX: Add 2 to for ]] at the end
                                          (treesit-fold--cons-add (cons 2 0) offset))
      (treesit-fold-range-line-comment node offset "--"))))

(defun treesit-fold-range-lua-function (node offset)
  "Define fold range for Lua `function' declaration.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let* ((params (treesit-node-child-by-field-name node "parameters"))
         (beg (treesit-node-end params))
         (end (- (treesit-node-end node) 3)))  ; fit identifier `end'
    (when treesit-fold-on-next-line  ; display nicely
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-lua-if (node offset)
  "Define fold range for Lua `if' statement.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let* ((then (car (treesit-fold-find-children node "then")))
         (beg (treesit-node-end then))
         (next (or (treesit-fold-find-children-traverse node "elseif_statement")
                   (treesit-fold-find-children-traverse node "else_statement")))
         (end (if next
                  (treesit-node-start (car next))
                (- (treesit-node-end node) 3))))
    (when treesit-fold-on-next-line  ; display nicely
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-lua-elseif (node offset)
  "Define fold range for Lua `elseif' statement.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let* ((then (car (treesit-fold-find-children node "then")))
         (beg (treesit-node-end then))
         (next (treesit-node-next-sibling node))
         (end (if next
                  (treesit-node-start next)
                (treesit-node-end node))))
    (when treesit-fold-on-next-line  ; display nicely
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-lua-else (node offset)
  "Define fold range for Lua `else' statement.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let* ((beg (+ (treesit-node-start node) 4))  ; fit `else', 4 letters
         (next (treesit-node-next-sibling node))          ; the `end' node
         (end (treesit-node-start next)))
    (when treesit-fold-on-next-line  ; display nicely
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-lua-do-loop (node offset)
  "Define fold range for Lua `while' and `for' statement.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let* ((do (car (treesit-fold-find-children node "do")))
         (beg (treesit-node-end do))
         (end (- (treesit-node-end node) 3)))
    (when treesit-fold-on-next-line  ; display nicely
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-lua-repeat (node offset)
  "Define fold range for Lua `repeat' statement.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let* ((beg (+ (treesit-node-start node) 6))  ; fit `repeat', 6 letters
         (until (car (treesit-fold-find-children node "until")))
         (end (treesit-node-start until)))
    (when treesit-fold-on-next-line  ; display nicely
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-make-recipe (node offset)
  "Define fold range for `recipe' in Make.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((last-child (treesit-fold-last-child node))
              (beg (treesit-node-start node))
              (end (treesit-node-end last-child)))
    (treesit-fold--cons-add (cons beg end) offset)))

;;+ OCaml

(defun treesit-fold-range-ocaml-comment (node offset)
  "Define fold range for `comment'.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((text (treesit-node-text node))
              (beg  (+ (if (string-prefix-p "(* " text) 2 3)
                       (treesit-node-start node)))
              (end  (- (treesit-node-end node) 2)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-ocaml-module-definition (node offset)
  "Define fold range for `module_definition'.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let*
      ((module-binding (treesit-node-child node 0 t))
       (body           (treesit-node-child-by-field-name module-binding "body"))
       ;; body is struct ... end
       (beg            (+ 6 (treesit-node-start body)))
       (end            (- (treesit-node-end node) 3)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-ocaml-type-definition (node offset)
  "Define fold range for `type_definition'.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let*
      ((type-definition (treesit-node-child node 0 t))
       (body            (treesit-node-child-by-field-name type-definition "body"))
       (text            (treesit-node-text (treesit-node-child body 0)))
       (beg
        (if (string-equal "{" text)
            (1+ (treesit-node-start body))
          (treesit-node-end (treesit-node-prev-sibling body))))
       (end
        (if (string-equal "{" text)
            (1- (treesit-node-end node))
          (treesit-node-end node))))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-ocaml-value-definition (node offset)
  "Define fold range for `value_definition'.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let*
      ((let-binding  (treesit-node-child node 0 t))
       (body         (treesit-node-child-by-field-name let-binding "body"))
       (beg          (treesit-node-end (treesit-node-prev-sibling body)))
       (end          (treesit-node-end node)))
    (treesit-fold--cons-add (cons beg end) offset)))

;;- OCaml

(defun treesit-fold-range-org-body (node offset)
  "Define fold range for `body' in Org.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let*
      ((parent (treesit-node-parent node))
       (parent (treesit-node-parent parent)))
    (treesit-fold--cons-add (cons -1 0) (treesit-fold-range-seq node offset))))

(defun treesit-fold-range-pascal-comment (node offset)
  "Define fold range for `comment' in Pascal.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let ((text (treesit-node-text node)))
    (cond ((string-prefix-p "{" text)
           (treesit-fold-range-seq node offset))
          ((string-prefix-p "(*" text)
           (treesit-fold-range-seq node (treesit-fold--cons-add '(1 . -1) offset)))
          (t
           (treesit-fold-range-c-like-comment node offset)))))

(defun treesit-fold-range-python-def (node offset)
  "Define fold range for `function_definition' and `class_definition'.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((named-node (or (treesit-node-child-by-field-name node "superclasses")
                              (treesit-node-child-by-field-name node "return_type")
                              (treesit-node-child-by-field-name node "parameters")
                              (treesit-node-child-by-field-name node "name")))
              ;; the colon is an anonymous node after return_type or parameters node
              (beg (treesit-node-end (treesit-node-next-sibling named-node)))
              (end (treesit-node-end node)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-python-expression-statement (node offset)
  "Define fold range for `expression_statement'.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((string-node (car (treesit-fold-find-children-traverse node "string")))
              ;; the colon is an anonymous node after return_type or parameters node
              (beg (treesit-node-start string-node))
              (end (treesit-node-end node)))
    (treesit-fold--cons-add (cons (+ beg 3) (- end 3)) offset)))

(defun treesit-fold-range-rst-body (node offset)
  "Define fold range for `body' in reStructuredText.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (let* ((first (car (treesit-fold-get-children node)))
         (beg (treesit-node-end first))
         (end (treesit-node-end node))
         (same-pos (= beg end))
         (beg (if same-pos (treesit-node-start node) beg)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-ruby-class-def (node offset)
  "Define fold range for `method' and `class' in Ruby.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((named-node (or (treesit-node-child-by-field-name node "superclass")
                              (treesit-node-child-by-field-name node "parameters")
                              (treesit-node-child-by-field-name node "name")))
              (beg (treesit-node-end named-node))
              (end (treesit-node-end node))
              (end (- end 3)))
    (when treesit-fold-on-next-line  ; display nicely
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-ruby-if (node offset)
  "Define fold range for `if' (then), `elsif', and `else' in Ruby.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((beg (treesit-node-start node))
              (end (cond ((when-let ((next (treesit-node-next-sibling node)))
                            (treesit-node-start next)))
                         ((when-let ((parent (treesit-fold-find-parent node "if")))
                            (- (treesit-node-end parent) 3))))))
    (when treesit-fold-on-next-line  ; display nicely
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-rust-macro (node offset)
  "Return the fold range for `macro_definition' in Rust.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((last_bracket (treesit-fold-last-child node))
              (first_bracket (treesit-node-child node 2))
              (beg (treesit-node-start first_bracket))
              (end (1+ (treesit-node-start last_bracket))))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-sql-block (node offset)
  "Return the fold range for `block' in SQL.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((beg-node (car (treesit-fold-find-children node "keyword_begin")))
              (end-node (car (treesit-fold-find-children node "keyword_end")))
              (beg (treesit-node-end beg-node))
              (end (treesit-node-start end-node)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-toml-table (node offset)
  "Return the fold range for `table' in TOML.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((close-bracket (car (treesit-fold-find-children node "]")))
              (beg (treesit-node-end close-bracket))
              (end-child (treesit-fold-last-child node))
              (end (treesit-node-end end-child)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-verilog-initial-construct (node offset)
  "Return the fold range for `initial' in Verilog.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((beg (treesit-node-start node))
              (beg (treesit-fold--eol beg))
              (end-child (treesit-fold-last-child node))
              (end (treesit-node-end end-child))
              (end (treesit-fold--bol end)))
    (when treesit-fold-on-next-line
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-verilog-list (node offset)
  "Return the fold range for `list' in Verilog.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((prev (treesit-node-prev-sibling node))
              (next (treesit-node-next-sibling node))
              (beg (treesit-node-end prev))
              (end (treesit-node-start next)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-verilog-module (node offset)
  "Return the fold range for `module' in Verilog.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((close-bracket (car (treesit-fold-find-children node ";")))
              (beg (treesit-node-end close-bracket))
              (end-child (treesit-fold-last-child node))
              (end (treesit-node-end end-child))
              (end (treesit-fold--bol end)))
    (when treesit-fold-on-next-line
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-vhdl-package (node offset)
  "Return the fold range for `package' in VHDL.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((start-child (car (treesit-fold-find-children node "declarative_part")))
              (beg (treesit-node-start start-child))
              (beg (treesit-fold--last-eol beg))
              (end-child (car (treesit-fold-find-children node "end")))
              (end (treesit-node-start end-child)))
    (when treesit-fold-on-next-line
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))

(defun treesit-fold-range-vhdl-type (node offset)
  "Return the fold range for `type' in VHDL.

For arguments NODE and OFFSET, see function `treesit-fold-range-seq' for
more information."
  (when-let* ((start-child (car (treesit-fold-find-children node "record_type_definition")))
              (record (car (treesit-fold-find-children start-child "record")))
              (beg (treesit-node-end record))
              (end-child (car (treesit-fold-find-children start-child "end")))
              (end (treesit-node-start end-child)))
    (when treesit-fold-on-next-line
      (setq end (treesit-fold--last-eol end)))
    (treesit-fold--cons-add (cons beg end) offset)))


(provide 'treesit-fold)
;;; treesit-fold.el ends here
