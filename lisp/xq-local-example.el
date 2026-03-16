;;; xq-local-example.el --- Example machine-local overrides -*- lexical-binding: t; -*-

;; Copy the pieces you want into one or both untracked files:
;;   lisp/xq-local-pre.el
;;   lisp/xq-local-post.el

;; Example pre-init overrides:
;; Disable whole shared modules before `init.el` requires them.
;; (xq/disable-module 'xq-org)
;; (xq/disable-module 'xq-terminal)
;;
;; Re-enable a module later in the list, if needed.
;; (xq/enable-module 'xq-org 'xq-terminal)
;;
;; Set variables that package `:init` blocks should read.
;; (setq xq/org-directory (expand-file-name "work-notes" (getenv "HOME")))

;; Example post-init overrides:
;; Override theme, modeline, or package variables after shared config loads.
;; (setq doom-modeline-minor-modes t)
;; (load-theme 'doom-nord t)

(provide 'xq-local-example)
;;; xq-local-example.el ends here
