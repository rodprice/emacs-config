;;; ulisp-mode.el --- Interaction mode for uLisp over serial  -*- lexical-binding: t -*-

;;; Commentary:
;; A comint-based REPL for uLisp, driven by `arduino-cli monitor'
;; as a subprocess.  This gives comint a real process to talk to,
;; so input/output work correctly without any stty or serial hacks.

;;; Code:

(require 'comint)

;;; Customization

(defgroup ulisp nil
  "Interaction with uLisp over serial via arduino-cli."
  :group 'tools)

(defcustom ulisp-serial-port "/dev/cu.usbmodem214301"
  "Serial port for the uLisp device."
  :type 'string
  :group 'ulisp)

(defcustom ulisp-baud-rate 115200
  "Baud rate for the uLisp serial connection."
  :type 'integer
  :group 'ulisp)

(defcustom ulisp-other-flags "--quiet"
  "Misc flags for the arduino-cli executable."
  :type 'string
  :group 'ulisp)

(defcustom ulisp-buffer-name "*uLisp*"
  "Name of the uLisp REPL buffer."
  :type 'string
  :group 'ulisp)

(defcustom ulisp-auto-connect nil
  "If non-nil, connect automatically when `ulisp-minor-mode' is enabled."
  :type 'boolean
  :group 'ulisp)

(defcustom ulisp-send-delay 0.01
  "Seconds between lines when sending a multi-line form."
  :type 'float
  :group 'ulisp)

(defcustom ulisp-prompt-regexp "^[0-9]+> "
  "Regexp matching the uLisp prompt, e.g. \"24900> \"."
  :type 'string
  :group 'ulisp)

(defcustom ulisp-arduino-cli-executable "arduino-cli"
  "Path to the arduino-cli executable."
  :type 'string
  :group 'ulisp)

;;; Internal state

(defvar ulisp--process nil)

;;; Output filters

(defun ulisp--strip-cr (string)
  "Strip carriage returns from serial output."
  (replace-regexp-in-string "\r" "" string))

;;; Connection

(defun ulisp-connect (&optional port baud)
  "Start arduino-cli monitor as a comint subprocess."
  (interactive)
  (let* ((port (or port ulisp-serial-port))
         (baud (or baud ulisp-baud-rate))
         (buf  (get-buffer-create ulisp-buffer-name))
         (cmd  ulisp-arduino-cli-executable)
         (args (list "monitor"
                     "--port" port
                     "--config" (format "baudrate=%d,dtr=off,rts=off" baud)
                     ulisp-other-flags)))
    (when (and ulisp--process (process-live-p ulisp--process))
      (delete-process ulisp--process))
    (with-current-buffer buf
      (unless (derived-mode-p 'ulisp-repl-mode)
        (ulisp-repl-mode))
      (comint-exec buf "uLisp" cmd nil args)
      (setq ulisp--process (get-buffer-process buf))
      ;; Give arduino-cli time to connect, then send a newline
      ;; to provoke a fresh prompt from uLisp.
      (run-with-timer 1.5 nil
                      (lambda ()
                        (when (process-live-p ulisp--process)
                          (comint-send-string ulisp--process "\n")))))
    (ulisp-show-repl)
    (with-current-buffer ulisp-buffer-name
      (goto-char (point-max)))
    (message "uLisp connected on %s at %d baud" port baud)))

(defun ulisp-disconnect ()
  "Disconnect from the uLisp device."
  (interactive)
  (if (and ulisp--process (process-live-p ulisp--process))
      (progn (delete-process ulisp--process)
             (setq ulisp--process nil)
             (message "uLisp disconnected"))
    (message "uLisp not connected")))

(defun ulisp-reconnect ()
  "Reconnect to the uLisp device."
  (interactive)
  (ulisp-disconnect)
  (sit-for 0.5)
  (ulisp-connect))

(defun ulisp--ensure-connected ()
  "Connect if not already connected."
  (unless (and ulisp--process (process-live-p ulisp--process))
    (ulisp-connect)))

(defun ulisp-repl-send-input ()
  "Send input to uLisp without blocking."
  (interactive)
  (let* ((pmark (process-mark ulisp--process))
         (input (string-trim
                 (buffer-substring-no-properties pmark (point-max)))))
    (goto-char (point-max))
    (insert "\n")
    (set-marker (process-mark ulisp--process) (point-max))
    (unless (string-empty-p input)
      (comint-add-to-input-history input))
    (comint-send-string ulisp--process (concat input "\n"))))


(defun ulisp--send-string (str)
  "Send STR to uLisp one line at a time."
  (ulisp--ensure-connected)
  (dolist (line (split-string str "\n"))
    (let ((trimmed (string-trim line)))
      (unless (string-empty-p trimmed)
        (comint-send-string ulisp--process (concat trimmed "\n"))
        (sit-for ulisp-send-delay)))))

(defun ulisp-send-sexp ()
  "Send the sexp before point to uLisp."
  (interactive)
  (ulisp--ensure-connected)
  (let* ((end (save-excursion (skip-chars-backward " \t\n") (point)))
         (beg (save-excursion (goto-char end) (backward-sexp) (point)))
         (sexp (string-trim (buffer-substring-no-properties beg end))))
    (ulisp--send-string sexp)
    (ulisp-show-repl)
    (message "Sent: %s" (truncate-string-to-width sexp 60))))

(defun ulisp-send-top-level-sexp ()
  "Send the top-level defun containing point to uLisp."
  (interactive)
  (ulisp--ensure-connected)
  (save-excursion
    (beginning-of-defun)
    (let ((beg (point)))
      (end-of-defun)
      (ulisp--send-string
       (string-trim (buffer-substring-no-properties beg (point))))
      (ulisp-show-repl)
      (message "Sent top-level form"))))

(defun ulisp-send-region (beg end)
  "Send region to uLisp."
  (interactive "r")
  (ulisp--ensure-connected)
  (ulisp--send-string
   (string-trim (buffer-substring-no-properties beg end)))
  (ulisp-show-repl)
  (message "Sent region (%d chars)" (- end beg)))

(defun ulisp-send-buffer ()
  "Send the current buffer to uLisp."
  (interactive)
  (ulisp--ensure-connected)
  (ulisp--send-string (buffer-string))
  (ulisp-show-repl)
  (message "Sent buffer: %s" (buffer-name)))

(defun ulisp-send-file (file)
  "Send FILE to uLisp."
  (interactive "fFile to send: ")
  (ulisp--ensure-connected)
  (ulisp--send-string
   (with-temp-buffer
     (insert-file-contents file)
     (buffer-string)))
  (ulisp-show-repl)
  (message "Sent: %s" file))

;;; REPL window

(defun ulisp-show-repl ()
  "Show the uLisp REPL buffer at the bottom."
  (interactive)
  (display-buffer
   (get-buffer-create ulisp-buffer-name)
   '((display-buffer-reuse-window
      display-buffer-in-side-window)
     (side . bottom)
     (window-height . 0.3))))

(defun ulisp-clear-repl ()
  "Clear the uLisp REPL buffer."
  (interactive)
  (with-current-buffer (get-buffer-create ulisp-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;;; ulisp-repl-mode

(defvar ulisp-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map (kbd "RET")     #'ulisp-repl-send-input)
    (define-key map (kbd "C-c C-c") #'ulisp-disconnect)
    (define-key map (kbd "C-c C-r") #'ulisp-reconnect)
    (define-key map (kbd "C-c C-k") #'ulisp-clear-repl)
    map)
  "Keymap for `ulisp-repl-mode'.")

(define-derived-mode ulisp-repl-mode comint-mode "uLisp"
  "Comint REPL for uLisp, driven by arduino-cli monitor.
RET sends input; M-p/M-n cycle history."
  (setq comint-prompt-regexp ulisp-prompt-regexp)
  (setq comint-prompt-read-only t)
  (setq comint-process-echoes t)
  (setq-local truncate-lines t)
  (add-hook 'comint-preoutput-filter-functions #'ulisp--strip-cr nil t))

;;; ulisp-minor-mode — for source editing buffers

(defvar ulisp-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'ulisp-send-sexp)
    (define-key map (kbd "C-M-x")   #'ulisp-send-top-level-sexp)
    (define-key map (kbd "C-c C-c") #'ulisp-send-top-level-sexp)
    (define-key map (kbd "C-c C-r") #'ulisp-send-region)
    (define-key map (kbd "C-c C-b") #'ulisp-send-buffer)
    (define-key map (kbd "C-c C-l") #'ulisp-send-file)
    (define-key map (kbd "C-c C-z") #'ulisp-show-repl)
    (define-key map (kbd "C-c C-k") #'ulisp-clear-repl)
    map)
  "Keymap for `ulisp-minor-mode'.")

(define-minor-mode ulisp-minor-mode
  "Edit uLisp source and send forms to the device over serial.

\\{ulisp-minor-mode-map}"
  :lighter " uLisp"
  :keymap ulisp-minor-mode-map
  (when (and ulisp-minor-mode ulisp-auto-connect)
    (unless (and ulisp--process (process-live-p ulisp--process))
      (ulisp-connect))))

(defun ulisp-setup ()
  "Enable `ulisp-minor-mode' in the current buffer."
  (interactive)
  (ulisp-minor-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ul\\'" . lisp-mode))
(add-hook 'lisp-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string= (file-name-extension buffer-file-name) "ul"))
              (ulisp-minor-mode 1))))

(provide 'ulisp-mode)
;;; ulisp-mode.el ends here
