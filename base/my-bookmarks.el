;;; Customize emacs' bookmarks facilities
;; Rod Price  Dec 2010


(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

;; M$ Visual Studio key setup.
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;; Click on fringe to toggle bookmarks, and use mouse wheel to move
;; between them.
(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)

;; Make sure that bm.el doesn't pollute my home directory
(setq bm-repository-file (concat dotfiles-dir "bm-repository"))

;; Use a mark in the fringe to show bookmarks
(setq bm-highlight-style 'bm-highlight-only-fringe)

(provide 'my-bookmarks)