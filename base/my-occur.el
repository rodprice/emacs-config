;;; Occur mode customizations
;; Rod Price, October 2009

;; Adapted from
;; http://ignaciopp.wordpress.com/2009/06/10/customizing-emacs-occur/


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autoloads and requires
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (kbd `C-o') Call occur on the current buffer.
(define-key global-map (kbd "C-o") 'my-occur)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-specific key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (kbd `q') Leave occur mode.
(define-key occur-mode-map (kbd "q") 'my-occur-mode-quit)
;; (kbd `C-q') Leave occur mode.
(define-key occur-mode-map (kbd "C-q") 'my-occur-mode-quit)
;; (kbd `C-g') Leave occur mode.
(define-key occur-mode-map (kbd "C-g") 'my-occur-mode-quit)
;; (kbd `C-RET') Jump to occur link in other window.
(define-key occur-mode-map (kbd "C-RET") 'occur-mode-goto-occurrence-other-window)
;; (kbd `C-<up>') Jump to occur link in other window.
(define-key occur-mode-map (kbd "C-<up>") 'occur-mode-goto-occurrence-other-window)
;; (kbd `RET') Show occur link without leaving the occur window.
(define-key occur-mode-map (kbd "RET") 'occur-mode-display-occurrence)
;; (kbd `p') Move up one line in the occur window.
(define-key occur-mode-map (kbd "p") 'previous-line)
;; (kbd `n') Move down one line in the occur window.
(define-key occur-mode-map (kbd "n") 'next-line)
;; (kbd `TAB') Move to the next link in the occur window.
(define-key occur-mode-map (kbd "TAB") 'occur-next)
;; (kbd `S-TAB') Move to the previous link in the occur window.
(define-key occur-mode-map [(shift iso-lefttab)] 'occur-prev)
;; (kbd `C-o') Activate occur easily inside isearch.
(define-key isearch-mode-map (kbd "C-o") 'my-occur-inside-isearch)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-specific custom variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-specific functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar source-buffer nil
  "The buffer being searched.")

(defun my-occur (&optional arg)
  "Open the occur buffer in a narrow vertical window on the
right-hand side of the frame.  Returns the new occur buffer."
  (interactive
   (list (if (and (symbolp 'arg)
                  (boundp 'arg)
                  (> (length arg) 0))
             (read-string (format "Regexp: %s" arg) nil nil arg)
           (read-string "Regexp: "))))
  (setq source-buffer (current-buffer))
  (window-configuration-to-register ?y)
  (occur arg)
  (let ((occur-buffer (get-buffer "*Occur*")))
    (if (not occur-buffer)
        (message "There are no results.")
      (delete-other-windows)
      (let ((window (split-window-horizontally 95)))
        (setq buffer (pop-to-buffer occur-buffer t))
        (setq truncate-lines t)
        (my-occur-linum-off)
        occur-buffer))))

(defun my-occur-linum-off ()
  "Turn off linum for this buffer."
  (remove-hook 'post-command-hook 'linum-update-current t)
  (remove-hook 'post-command-hook 'linum-schedule t)
  (remove-hook 'window-size-change-functions 'linum-after-size t)
  (remove-hook 'window-scroll-functions 'linum-after-scroll t)
  (remove-hook 'after-change-functions 'linum-after-change t)
  (remove-hook 'window-configuration-change-hook 'linum-after-config t)
  (remove-hook 'change-major-mode-hook 'linum-delete-overlays t)
  (linum-delete-overlays))

(defun my-occur-mode-quit ()
  "Quit and close occur window."
  (interactive)
  (if source-buffer                ;; go to the main window
      (switch-to-buffer source-buffer))
  (point-to-register ?1)            ;; store the latest cursor position
  (switch-to-buffer "*Occur*")      ;; go back to the occur window
  (kill-buffer "*Occur*")           ;; delete it
  (jump-to-register ?y)             ;; reset the original frame state
  (register-to-point ?1)            ;; re-position cursor
  (setq source-buffer nil))

(defun my-occur-inside-isearch ()
  "Activate occur easily inside an incremental search."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search)
        (search-point nil)
        (occur-point nil)
        (occur-buffer nil))
    (setq source-buffer (current-buffer))
    (setq search-point     ;; get point from buffer being searched
          (save-excursion
            (set-buffer source-buffer)
            (point)))
    (setq occur-buffer     ;; send the search string to my-occur
          (my-occur (if isearch-regexp
                        isearch-string
                      (regexp-quote isearch-string))))
    (when occur-buffer     ;; make sure occur returned results
      (condition-case ()   ;; catch errors from occur-next
          (progn
            (while (progn  ;; move point in occur buffer to first match
                     (occur-next)
                     (setq occur-point ;; get what occur thinks is point
                           (save-excursion
                             (set-buffer occur-buffer)
                             (get-text-property (point) 'occur-target)))
                     (and (> search-point occur-point)
                          (my-occur-string-bounds-p
                           occur-point search-point isearch-string))))
            (isearch-done) ;; stop the search
            (isearch-clean-overlays))
        (message "No more matches"))))) ;; in case of error in occur-next

(defun my-occur-string-bounds-p (start end string)
  "Returns true iff start and end point to the beginning or
ending, respectively, of `string' inside `source-buffer'."
  (save-excursion
    (set-buffer source-buffer)
    (let target-string ((buffer-substring-no-properties start end))
         (string= string target-string))))
    

(provide 'my-occur)
