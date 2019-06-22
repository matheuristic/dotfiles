;;; init-ui-hydra.el --- Emacs config Hydra layer -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Set up hydra package

;;; Code:

(require 'cl-extra)

(defgroup init-ui-hydra-el nil
  "Hydra-specific settings."
  :group 'convenience)

(defcustom init-ui-hydra-load-frames-windows-hydra nil
  "Whether to load frames and windows hydras.
Note that `hyperbole' provides `hycontrol' which has similar functionality."
  :type 'boolean
  :group 'init-ui-hydra-el)

(defun my-open-finder (&optional path)
  "Opens a new Finder window to PATH, or the current buffer file or directory in that order if PATH is nil. Mac OS X-only."
  (interactive)
  (let* ((my-path (cl-some 'identity (list path (buffer-file-name) default-directory)))
         (my-full-path (expand-file-name my-path))
         (my-process-args (list "my-open-finder" nil "open" "-R" my-full-path)))
    (if (eq system-type 'darwin)
        (apply 'start-process my-process-args)
      (message "my-open-in-finder is Mac OS X-only"))))

;; framework for temporary and repeatable bindings
(use-package hydra
  :pin "MELPA"
  :config
  (defhydra my-hydra/bookmarks (:color teal :columns 3)
    "Bookmarks"
    ("s" bookmark-set "set")
    ("d" bookmark-delete "delete")
    ("l" list-bookmarks "list")
    ("j" bookmark-jump "jump")
    ("i" bookmark-insert "insert")
    ("I" bookmark-insert-location "insert-loc")
    ("L" bookmark-load "load")
    ("W" bookmark-write "write")
    ("q" nil "quit"))
  (defhydra my-hydra/buffer (:color amaranth :columns 5)
    "Buffer"
    ("p" previous-buffer "previous")
    ("n" next-buffer "next")
    ("R" revert-buffer "revert")
    ("B" bury-buffer "bury")
    ("U" unbury-buffer "unbury")
    ("s" save-buffer "save")
    ("S" save-some-buffers "save-all")
    ("k" kill-this-buffer "kill")
    ("K" kill-matching-buffers "kill-match")
    ("b" switch-to-buffer "switch" :exit t)
    ("e" my-open-finder "open-finder" :exit t)
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/desktop (:color teal)
    "Desktop"
    ("c" desktop-clear "clear")
    ("s" desktop-save "save")
    ("r" desktop-read "read")
    ("R" desktop-revert "revert")
    ("d" desktop-change-dir "dir")
    ("q" nil "quit"))
  (defhydra my-hydra/help (:color teal :columns 4)
    "Help"
    ("a" apropos-command "apropos-cmd")
    ("d" apropos-documentation "apropos-doc")
    ("f" describe-function "desc-fun")
    ("v" describe-variable "desc-var")
    ("c" describe-key-briefly "desc-key-brief")
    ("k" describe-key "desc-key")
    ("b" describe-bindings "desc-bind")
    ("m" describe-mode "desc-mode")
    ("p" describe-package "desc-pkg")
    ("y" describe-syntax "desc-syntax")
    ("e" view-echo-area-messages "messages")
    ("l" view-lossage "lossage")
    ("i" info "info")
    ("s" info-lookup-symbol "info-symbol")
    ("w" where-is "where-is")
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/navigation (:color amaranth :columns 4)
    "Navigation"
    ("h" backward-char "left")
    ("j" next-line "down")
    ("k" previous-line "up")
    ("l" forward-char "right")
    ("b" backward-word "wd-left")
    ("w" forward-word "wd-right")
    ("0" move-beginning-of-line "ln-begin")
    ("$" move-end-of-line "ln-end")
    ("(" backward-sentence "sent-left")
    (")" forward-sentence "sent-right")
    ("{" backward-paragraph "par-left")
    ("}" forward-paragraph "par-right")
    ("," backward-sexp "sexp-left")
    ("." forward-sexp "sexp-right")
    ("[" backward-list "list-left")
    ("]" forward-list "list-right")
    ("S-SPC" scroll-down "pg-up")
    ("SPC" scroll-up "pg-down")
    ("<" scroll-right "pg-left")
    (">" scroll-left "pg-right")
    ("C-SPC" set-mark-command "set-mark")
    ("x" exchange-point-and-mark "xchg-mark")
    ("gg" beginning-of-buffer "beg-buf")
    ("G" end-of-buffer "end-buf")
    ("gl" goto-line "goto-line")
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/registers (:color teal :columns 4)
    "Registers"
    ("SPC" point-to-register "save-point")
    ("w" window-configuration-to-register "save-windows")
    ("f" frameset-to-register "save-frames")
    ("j" jump-to-register "jump")
    ("s" copy-to-register "copy-region")
    ("a" append-to-register "append-region")
    ("p" prepend-to-register "prepend-region")
    ("r" copy-rectangle-to-register "copy-rect")
    ("i" insert-register "insert")
    ("l" list-registers "list")
    ("v" view-register "view")
    ("q" nil "quit"))
  (defhydra my-hydra/search (:color teal :columns 3)
    "Search"
    ("gg" grep "grep")
    ("gr" rgrep "rgrep")
    ("gl" lgrep "lgrep")
    ("gf" grep-find "grep-find")
    ("gz" rzgrep "rzgrep")
    ("oo" occur "occur")
    ("om" multi-occur "multi-occur")
    ("ob" multi-occur-in-matching-buffers "multi-occur-match-buf")
    ("oO" org-occur "org-occur")
    ("rs" query-replace "replace string")
    ("rr" query-replace-regexp "replace regexp")
    ("kg" kill-grep "kill-grep")
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/visual (:color amaranth :columns 4)
    "Visual"
    ("b" blink-cursor-mode "blink-cursor")
    ("F" follow-mode "follow-mode")
    ("f" font-lock-mode "font-lock")
    ("H" highlight-changes-mode "hl-changes")
    ("h" hl-line-mode "hl-line")
    ("l" display-line-numbers-mode "line-numbers")
    ("p" show-paren-mode "show-paren")
    ("S" toggle-scroll-bar "scroll-bar")
    ("T" transient-mark-mode "transient-mark")
    ("t" toggle-truncate-lines "truncate-lines")
    ("v" visual-line-mode "visual-line")
    ("N" narrow-to-region "narrow-region" :exit t)
    ("P" narrow-to-page "narrow-page" :exit t)
    ("D" narrow-to-defun "narrow-defun" :exit t)
    ("W" widen "narrow-widen" :exit t)
    ("=" text-scale-increase "zoom-in")
    ("-" text-scale-decrease "zoom-out")
    ("0" (text-scale-adjust 0) "zoom-reset")
    ("q" nil "quit" :exit t))
  (global-set-key (kbd "C-c s-b m") 'my-hydra/bookmarks/body)
  (global-set-key (kbd "C-c s-b f") 'my-hydra/buffer/body)
  (global-set-key (kbd "C-c s-h h") 'my-hydra/help/body)
  (global-set-key (kbd "C-c s-d k") 'my-hydra/desktop/body)
  (global-set-key (kbd "C-c s-s") 'my-hydra/search/body)
  (global-set-key (kbd "C-c s-n") 'my-hydra/navigation/body)
  (global-set-key (kbd "C-c s-r r") 'my-hydra/registers/body)
  (global-set-key (kbd "C-c s-v i") 'my-hydra/visual/body))

;; load frames and windows hydras
(when init-ui-hydra-load-frames-windows-hydra
  (defun my-transpose-windows (selector)
    "Transpose buffers between current window and window after calling SELECTOR."
    (let ((from-win (selected-window))
          (from-buf (window-buffer)))
      (funcall selector)
      (set-window-buffer from-win (window-buffer))
      (set-window-buffer (selected-window) from-buf)))
  (defun my-enlarge-frame (w h)
    "Enlarge width, height of selected frame by W, H lines (shrink if negative)."
    (let ((this-frame (selected-frame)))
      (set-frame-width this-frame (+ (frame-width this-frame) w))
      (set-frame-height this-frame (+ (frame-height this-frame) h))))
  (defun my-move-frame (x y)
    "Move selected frame by X pixels horizontally and Y pixels vertically."
    (let* ((this-frame (selected-frame))
           (fpos (frame-position this-frame)))
      (set-frame-position this-frame (+ (car fpos) x) (+ (cdr fpos) y))))
  (defun my-move-frame-pct (x y)
    "Move selected frame within display by X% horizontally and Y% vertically."
    (my-move-frame (* x (/ (x-display-pixel-width) 100))
                   (* y (/ (x-display-pixel-height) 100))))
  (defhydra my-hydra/frame (:color amaranth :columns 4)
    "Frame"
    ("p" (other-frame -1) "previous")
    ("n" other-frame "next")
    ("s" select-frame-by-name "select")
    ("M" toggle-frame-maximized "maximize")
    ("+" (lambda (n) (interactive "p") (my-enlarge-frame 0 n)) "enlarge-v")
    ("-" (lambda (n) (interactive "p") (my-enlarge-frame 0 (- n))) "shrink-v")
    (">" (lambda (n) (interactive "p") (my-enlarge-frame n 0)) "enlarge-h")
    ("<" (lambda (n) (interactive "p") (my-enlarge-frame (- n) 0)) "shrink-h")
    ("}" (lambda (n) (interactive "p") (my-move-frame-pct 0 n)) "move-d")
    ("{" (lambda (n) (interactive "p") (my-move-frame-pct 0 (- n))) "move-u")
    (")" (lambda (n) (interactive "p") (my-move-frame-pct n 0)) "move-r")
    ("(" (lambda (n) (interactive "p") (my-move-frame-pct (- n) 0)) "move-l")
    ("m" make-frame "make")
    ("d" delete-frame "delete")
    ("o" delete-other-frames "only")
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/window (:color amaranth :columns 4)
    "Window"
    ("n" next-multiframe-window "next")
    ("p" previous-multiframe-window "previous")
    ("v" split-window-right "split-v")
    ("s" split-window-below "split-h")
    ("h" windmove-left "left")
    ("j" windmove-down "down")
    ("k" windmove-up "up")
    ("l" windmove-right "right")
    ("H" (my-transpose-windows 'windmove-left) "transpose-l")
    ("J" (my-transpose-windows 'windmove-down) "transpose-d")
    ("K" (my-transpose-windows 'windmove-up) "transpose-u")
    ("L" (my-transpose-windows 'windmove-right) "transpose-r")
    ("-" shrink-window "shrink-v")
    ("+" enlarge-window "enlarge-v")
    ("<" shrink-window-horizontally "shrink-h")
    (">" enlarge-window-horizontally "enlarge-h")
    ("M" minimize-window "minimize")
    ("m" maximize-window "maximize")
    ("=" balance-windows "balance")
    ("_" balance-windows-area "balance-area")
    ("o" delete-other-windows "only")
    ("d" delete-window "delete")
    ("D" kill-buffer-and-window "delete-buf")
    ("q" nil "quit" :exit t))
  (global-set-key (kbd "C-c s-f") 'my-hydra/frame/body)
  (global-set-key (kbd "C-c s-w w") 'my-hydra/window/body))

(provide 'init-ui-hydra)

;;; init-ui-hydra.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
