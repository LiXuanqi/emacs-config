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
(when (boundp 'native-comp-jit-compilation-deny-list)
  (add-to-list 'native-comp-jit-compilation-deny-list "org-roam\\(?:-.*\\)?\\.el\\'"))
(when (boundp 'native-comp-deferred-compilation-deny-list)
  (add-to-list 'native-comp-deferred-compilation-deny-list "org-roam\\(?:-.*\\)?\\.el\\'"))

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

;; Force built-in Org to avoid mixed-version loading with external Org packages.
(straight-use-package '(org :type built-in))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Keep Customize output out of init.el to reduce merge noise.
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load feature modules.
(require 'xq-defaults)
(require 'xq-theme)
(require 'xq-which-key)
(require 'xq-evil)
(require 'xq-completion)
(require 'xq-treesit)
(require 'xq-lang)
(require 'xq-org)
(require 'xq-keybinds)

;;; init.el ends here
