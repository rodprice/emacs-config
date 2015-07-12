;;; my-pager.el --- convenience

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

;; Make emacs paging behavior bearable.

;;; Code:

(require 'pager)
(global-set-key "\C-v"       'pager-page-down)
(global-set-key [next]       'pager-page-down)
(global-set-key "\ev"        'pager-page-up)
(global-set-key [prior]      'pager-page-up)
(global-set-key '[M-up]      'scroll-row-up)
(global-set-key '[M-kp-up]   'pager-row-up)
(global-set-key '[M-down]    'scroll-row-down)
(global-set-key '[M-kp-down] 'pager-row-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scroll viewport while leaving point in place in text

;; http://www.hodique.info/blog/2006/20061612_scrolling_emacs_viewport
(defun scroll-viewport (n)
  (let ((top (line-number-at-pos (window-start)))
        (cur (line-number-at-pos (point))))
    (recenter (+ (- cur top) n))))

(defun scroll-row-up (arg)
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-viewport (- arg)))

(defun scroll-row-down (arg)
  (interactive "p")
  (or arg (setq arg 1))
  (scroll-viewport arg))


(provide 'my-pager)
;;; my-pager.el ends here
