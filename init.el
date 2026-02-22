;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;; Keep package.el from activating packages before straight.el bootstraps.
(setq package-enable-at-startup nil)
;; Prefer newer .el files over stale compiled .elc files.
(setq load-prefer-newer t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Keep Customize output out of init.el to reduce merge noise.
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

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

(use-package which-key
  :config
  (which-key-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-prefix-command 'lq/leader-map)
  (define-prefix-command 'lq/window-map)
  (define-key lq/leader-map (kbd "w") #'lq/window-map)
  (define-key lq/window-map (kbd "|") #'split-window-right)
  (define-key lq/window-map (kbd "-") #'split-window-below)
  (define-key lq/window-map (kbd "h") #'windmove-left)
  (define-key lq/window-map (kbd "j") #'windmove-down)
  (define-key lq/window-map (kbd "k") #'windmove-up)
  (define-key lq/window-map (kbd "l") #'windmove-right)
  (evil-define-key '(normal visual motion) 'global (kbd "SPC") lq/leader-map))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;; init.el ends here
