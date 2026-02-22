;;; xq-ui.el --- UI setup -*- lexical-binding: t; -*-

(use-package emacs
  :init
  (setq inhibit-startup-screen t
        ring-bell-function 'ignore
        initial-frame-alist '((fullscreen . maximized)))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (line-number-mode 1)
  (column-number-mode 1)
  (global-display-line-numbers-mode 1))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :init
  (setq doom-modeline-height 22
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-minor-modes nil)
  :config
  (doom-modeline-mode 1))

(provide 'xq-ui)
;;; xq-ui.el ends here
