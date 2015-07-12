;;; bindings.el --- Global key bindings

;; Copyright (C) 2010

;; Author:  <rod@thirdoption.info>
;; Keywords: convenience

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
;; A place to gather up global key bindings for easy reference and
;; de-confliction among packages.

;;; Code:

(require 'functions)
(require 'settings)

;; Bind shift tab to backtab everywhere
(define-key function-key-map [(shift tab)] [backtab])

;; Pin a buffer to a window. Nice way to keep one window always visible
(global-set-key [pause] 'toggle-current-window-dedication)

;; TODO Make tab act like meta iff tab is held down simultaneously
;; with another key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control-<key> direct key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO Change C-' to cycle through toggle-trun.. and linum-mode?
(global-set-key (kbd "C-'") 'toggle-truncate-lines)

;; Modify kill-line behavior to include join
(global-set-key (kbd "C-k") 'kill-and-join-forward)

;; Go to first non-white space character on the line
(global-set-key (kbd "C-a") 'back-to-beginning)

;; Toggle between end of line and last non-comment character
(global-set-key (kbd "C-e") 'back-to-end)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Use regex searches by default.
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\C-r" 'isearch-backward-regexp)

;; Put comment key in easier place to reach
(global-set-key (kbd "C-;") 'comment-dwim)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meta-<key> direct key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "M-l") 'linum-mode)

;; Move across paragraphs
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; Run through compile errors with PgUp and PgDn
(global-set-key (kbd "<M-prior>") 'previous-error)
(global-set-key (kbd "<M-next>") 'next-error)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)

;; File finding
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control-X <key> bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Split window vertically and select the new window
(global-set-key (kbd "C-x 2") 'split-window-vertically-and-go-there)

;; Split window horizontally and select the new window
(global-set-key (kbd "C-x 3") 'split-window-horizontally-and-go-there)

;; Glue two lines back together on one line
(global-set-key (kbd "C-x j") 'flexible-join-line)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x a r") 'align-regexp)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key "\C-x\C-i" 'ido-imenu)

;; File finding
(global-set-key (kbd "C-x M-f")   'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f")     'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p")   'find-file-at-point)
(global-set-key (kbd "C-c y")     'bury-buffer)
(global-set-key (kbd "C-c r")     'revert-buffer)
(global-set-key (kbd "C-x C-b")   'ibuffer)

;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key "\C-xO" (lambda () (interactive) (other-window -1)))
(global-set-key "\C-x\C-o" (lambda () (interactive) (other-window 2)))

;; Indentation help
(global-set-key (kbd "C-x ^") 'join-line)

;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

;; If you want to be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; View a URL in a web browser
(global-set-key (kbd "C-x u") 'browse-url)

;; Fetch the contents at a URL, display it raw.
(global-set-key (kbd "C-x h") 'view-url-raw)

;; Start up the Magit package
(global-set-key (kbd "C-x g") 'magit-status)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control-C <key> bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; Should be able to eval-and-replace anywhere.
;;(global-set-key (kbd "C-c e") 'eval-and-replace)

;; Turn whitespace mode on and off
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; Applications
(global-set-key (kbd "C-c j")
                (lambda ()
                  (interactive)
                  (switch-or-start 'jabber-connect "*-jabber-*")))
(global-set-key (kbd "C-c g")
                (lambda ()
                  (interactive)
                  (switch-or-start 'gnus "*Group*")))
(global-set-key (kbd "C-c i")
                (lambda ()
                  (interactive)
                  (switch-or-start
                   (lambda ()
                     (rcirc-connect "irc.freenode.net"))
                   "*irc.freenode.net*")))

(global-set-key (kbd "C-c J")   'jabber-send-presence)
(global-set-key (kbd "C-c M-j") 'jabber-disconnect)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control-H <key> bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Get the help buffer out of the way quickly
(global-set-key (kbd "C-h q") 'bury-help-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Meta-Control-<key> bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Delete complete S-expressions.
(global-set-key (kbd "C-M-<backspace>") 'forward-kill-sexp)
(global-set-key (kbd "C-M-<delete>") 'backward-kill-sexp)

;; You know, like Readline.
(global-set-key "\C-\M-h" 'backward-kill-word)

;; Use regex searches by default.
(global-set-key "\C-\M-s" 'isearch-forward)
(global-set-key "\C-\M-r" 'isearch-backward)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Textmate key bindings (win key = meta, alt key = Mac's cmd key)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new
(global-set-key (kbd "A-n")         'ido-find-file)
;; new project
(global-set-key (kbd "A-S-n")       'undefined)
;; open
(global-set-key (kbd "A-o")         'ido-find-file)
;; save
(global-set-key (kbd "A-s")         'save-buffer)
;; save as
(global-set-key (kbd "A-S-s")       'ido-write-file)
;; save all
(global-set-key (kbd "A-M-s")       'save-some-buffers)
;; save project
(global-set-key (kbd "A-C-s")       'undefined)
;; save project as
(global-set-key (kbd "A-C-S-s")     'undefined)
;; reveal in project
(global-set-key (kbd "A-C-r")       'undefined)
;; print
(global-set-key (kbd "A-p")         'ps-print-buffer)
;; help
(global-set-key (kbd "A-?")         'undefined)
;; undo
(global-set-key (kbd "A-z")         'undo)
;; redo
(global-set-key (kbd "A-C-z")       'undefined)
;; cut
(global-set-key (kbd "A-x")         'kill-region)
;; copy
(global-set-key (kbd "A-c")         'copy-region-as-kill)
;; paste
(global-set-key (kbd "A-v")         'yank)
;; paste without re-indent
(global-set-key (kbd "A-C-v")       'undefined)
;; paste previous
(global-set-key (kbd "A-S-v")       'undefined)
;; duplicate line / selection
(global-set-key (kbd "C-S-d")       'duplicate-line-or-selection)
;; freehand editing
(global-set-key (kbd "A-M-e")       'undefined)
;; overwrite mode
(global-set-key (kbd "A-M-o")       'undefined)
;; select word
;(global-set-key (kbd "C-w")       'undefined)
;; select line
(global-set-key (kbd "A-S-l")       'undefined)
;; select enclosing brackets
(global-set-key (kbd "A-S-b")       'undefined)
;; select current scope
(global-set-key (kbd "C-M-b")       'undefined)
;; select all
(global-set-key (kbd "A-a")         'undefined)
;; completion
(global-set-key (kbd "A-ESC")       'undefined)
;; find
(global-set-key (kbd "A-f")         'undefined)
;; find in project
(global-set-key (kbd "A-S-f")       'occur)
;; find next
(global-set-key (kbd "A-g")         'undefined)
;; find previous
(global-set-key (kbd "A-S-g")       'undefined)
;; replace all in selection/file
(global-set-key (kbd "A-C-f")       'replace-regexp)
;; replace and find
(global-set-key (kbd "A-M-f")       'undefined)
;; use selection for find
(global-set-key (kbd "A-e")         'undefined)
;; use selection for replace
(global-set-key (kbd "A-S-e")       'undefined)
;; jump to selection
(global-set-key (kbd "A-j")         'undefined)
;; spelling
(global-set-key (kbd "A-:")         'undefined)
;; correct spelling
(global-set-key (kbd "A-;")         'undefined)
;; check spelling as you type
(global-set-key (kbd "A-M-;")       'undefined)
;; special characters
(global-set-key (kbd "A-M-t")       'undefined)
;; bigger font
(global-set-key (kbd "A-+")         'undefined)
;; smaller font
(global-set-key (kbd "A--")         'undefined)
;; show/hide bookmarks
(global-set-key (kbd "A-M-b")       'undefined)
;; show/hide line numbers
(global-set-key (kbd "A-M-l")       'undefined)
;; soft wrap
(global-set-key (kbd "A-M-w")       'undefined)
;; show/hide invisibles
(global-set-key (kbd "A-M-i")       'undefined)
;; fold current block
(global-set-key (kbd "A-<f1>")      'undefined)
;; convert to uppercase
(global-set-key (kbd "A-u")         'undefined)
;; convert to lowercase
(global-set-key (kbd "A-S-u")       'undefined)
;; convert to titlecase
(global-set-key (kbd "A-M-u")       'undefined)
;; move line up
(global-set-key (kbd "A-C-<up>")    'undefined)
;; move line down
(global-set-key (kbd "A-C-<down>")  'undefined)
;; move column left
(global-set-key (kbd "A-C-<left>")  'undefined)
;; move column right
(global-set-key (kbd "A-C-<right>") 'undefined)
;; shift left
(global-set-key (kbd "A-[")         'undefined)
;; shift right
(global-set-key (kbd "A-]")         'undefined)
;; reformat paragraph
(global-set-key (kbd "M-q")         'fill-paragraph)
;; unwrap paragraph
(global-set-key (kbd "M-S-q")       'unfill-paragraph)
;; execute line inserting result
(global-set-key (kbd "C-r")         'undefined)
;; filter through command
(global-set-key (kbd "A-M-r")       'undefined)
;; add/remove bookmark
(global-set-key (kbd "A-<f2>")      'undefined)
;; go to next bookmark
(global-set-key (kbd "<f2>")        'undefined)
;; go to previous bookmark
(global-set-key (kbd "A-<f2>")      'undefined)
;; navigate buffers
(global-set-key (kbd "A-M-<left>")  'previous-buffer)
;; navigate buffers
(global-set-key (kbd "A-M-<right>") 'next-buffer)
;; go to header/source
(global-set-key (kbd "A-M-<up>")    'undefined)
;; go to file
(global-set-key (kbd "A-t")         'undefined)
;; go to symbol
(global-set-key (kbd "A-S-t")       'undefined)
;; go to line
(global-set-key (kbd "A-l")         'goto-line)
;; go to middle visible line
(global-set-key (kbd "A-S-j")       'undefined)
;; source context
;; comment line / selection
(global-set-key (kbd "A-/")         'undefined)
;; look up definition (etags)
(global-set-key (kbd "C-]")         'undefined)
;; comment banner
(global-set-key (kbd "C-S-b")       'undefined)
;; move to eol and insert lf
(global-set-key (kbd "A-<return>")  'insert-lf-at-eol)
;; move to eol and insert ";"
(global-set-key (kbd "A-M-<return>") 'insert-semicolon-at-eol)
;; move to eol and insert ";" + lf
(global-set-key (kbd "A-S-<return>") 'insert-semicolon-lf-at-eol)
;; newline: \n
;(global-set-key (kbd "C-n")         'undefined)
;; convert source to info
(global-set-key (kbd "A-S-h")       'undefined)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keycodes from current X keymap, obtained with
;; xev | grep -A2 --line-buffered '^KeyRelease' | \
;; sed -n '/keycode /s/^.*keycode \([0-9]*\).* (.*, \(.*\)).*$/\1 \2/p'
(setq caps-lock    66
      scroll-lock  78
      num-lock     77
      ctrl-right   105
      menu         135
      super-right  134
      alt-right    108
      space        65
      alt-left     64
      super-left   133
      ctrl-left    37
      escape       9
      insert       118
      home         110
      prior        112
      delete       119
      end          115
      next         117
      left         113
      right        114
      up           111
      down         116
      keypad-enter 104
      shift-left   50
      shift-right  62
      pause        127)


(provide 'bindings)
;;; bindings.el ends here
