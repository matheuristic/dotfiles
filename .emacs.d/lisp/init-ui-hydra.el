;;; init-ui-hydra.el --- Emacs config UI layer helper functions -*- lexical-binding: t -*-

;; Author: matheuristic

;;; Commentary:

;; Load Emacs hydra package and set up basic hydras

;;; Code:

;;;;
;; Helper functions
;;;;

(require 'cl-seq)

(defun my-yank-from-kill-ring ()
  "Yank from the kill ring into buffer at point or region.
Uses `completing-read' for selection, which is set by Ido, Ivy, etc."
  (interactive)
  (let ((to_insert (completing-read
                    "Yank : " (cl-delete-duplicates kill-ring :test #'equal))))
    ;; delete selected buffer region (if applicable)
    (if (and to_insert (region-active-p))
      (delete-region (region-beginning) (region-end)))
    ;; insert the selected entry from the kill ring
    (insert to_insert)))

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

;;;;
;; Load packages
;;;;

;; framework for temporary or repeatable bindings - MELPA Stable
(use-package hydra
  :config
  ;; basic hydras
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
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/desktop (:color teal)
    "Desktop"
    ("c" desktop-clear "clear")
    ("s" desktop-save "save")
    ("r" desktop-read "read")
    ("R" desktop-revert "revert")
    ("d" desktop-change-dir "dir")
    ("q" nil "quit"))
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
  (defhydra my-hydra/narrow (:color teal)
    "Narrow"
    ("n" narrow-to-region "region")
    ("p" narrow-to-page "page")
    ("d" narrow-to-defun "defun")
    ("w" widen "widen")
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
    ("r SPC" point-to-register "pt-to-reg")
    ("rj" jump-to-register "jmp-to-reg")
    ("rm" bookmark-set "bmk-set")
    ("rb" bookmark-jump "bmk-jmp")
    ("gg" beginning-of-buffer "beg-buf")
    ("G" end-of-buffer "end-buf")
    ("gl" goto-line "goto-line")
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/search (:color teal :columns 3)
    "Search"
    ("gg" grep "grep")
    ("gr" rgrep "rgrep")
    ("gl" lgrep "lgrep")
    ("gf" grep-find "grep-find")
    ("gz" rzgrep "zrgrep")
    ("gd" grep-find-dired "grep-find-dired")
    ("oo" occur "occur")
    ("om" multi-occur "multi-occur")
    ("ob" multi-occur-in-matching-buffers "multi-occur-match-buf")
    ("oO" org-occur "org-occur")
    ("rs" query-replace "replace string")
    ("rr" query-replace-regexp "replace regexp")
    ("kg" kill-grep "kill-grep")
    ("q" nil "quit" :exit t))
  (defhydra my-hydra/visual (:color amaranth :columns 3)
    "Visual"
    ("b" blink-cursor-mode "blink-cursor")
    ("f" font-lock-mode "font-lock")
    ("l" display-line-numbers-mode "line-numbers")
    ("p" show-paren-mode "show-paren")
    ("T" transient-mark-mode "transient-mark")
    ("t" toggle-truncate-lines "truncate-lines")
    ("v" visual-line-mode "visual-line")
    ("w" whitespace-mode "whitespace")
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
  (defhydra my-hydra/zoom (:color amaranth)
    "Zoom"
    ("-" text-scale-decrease "out")
    ("+" text-scale-increase "in")
    ("0" (text-scale-adjust 0) "reset")
    ("q" nil "quit" :exit t))
  (global-set-key (kbd "H-D") 'my-hydra/desktop/body)
  (global-set-key (kbd "H-N") 'my-hydra/narrow/body)
  (global-set-key (kbd "H-s") 'my-hydra/search/body)
  (global-set-key (kbd "H-Z") 'my-hydra/zoom/body)
  (global-set-key (kbd "H-b") 'my-hydra/buffer/body)
  (global-set-key (kbd "H-f") 'my-hydra/frame/body)
  (global-set-key (kbd "H-n") 'my-hydra/navigation/body)
  (global-set-key (kbd "H-V") 'my-hydra/visual/body)
  (global-set-key (kbd "H-w") 'my-hydra/window/body))

(provide 'init-ui-hydra)

;;; init-ui-hydra.el ends here

;; suppress byte-compiler warnings about assignments to free variables
;; and calls to functions not known or not defined at runtime
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime)
;; End:
