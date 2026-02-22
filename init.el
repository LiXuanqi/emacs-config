;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;; Keep package.el from activating packages before straight.el bootstraps.
(setq package-enable-at-startup nil)
;; Prefer newer .el files over stale compiled .elc files.
(setq load-prefer-newer t)
;; Avoid noisy native-comp warnings from third-party packages like general.el.
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))
(when (boundp 'native-comp-jit-compilation-deny-list)
  (add-to-list 'native-comp-jit-compilation-deny-list "general\\.el\\'"))
(when (boundp 'native-comp-deferred-compilation-deny-list)
  (add-to-list 'native-comp-deferred-compilation-deny-list "general\\.el\\'"))

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
  (evil-mode 1))

(use-package general
  :after evil
  :config
  (general-auto-unbind-keys)
  (general-create-definer lq/leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC")
  (lq/leader-def
    "w"  '(:ignore t :which-key "window")
    "w|" '(split-window-right :which-key "split right")
    "w-" '(split-window-below :which-key "split below")
    "wh" '(windmove-left :which-key "focus left")
    "wj" '(windmove-down :which-key "focus down")
    "wk" '(windmove-up :which-key "focus up")
    "wl" '(windmove-right :which-key "focus right")))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;;; init.el ends here
