;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-

;; Ensure all relative paths resolve to this config directory, even in batch mode.
(setq user-emacs-directory
      (file-name-directory (or load-file-name buffer-file-name user-init-file)))

;; Keep package.el from activating packages before straight.el bootstraps.
(setq package-enable-at-startup nil)
;; Prefer newer .el files over stale compiled .elc files.
(setq load-prefer-newer t)

;; Avoid noisy native-comp warnings from third-party packages like general.el.
(when (boundp 'native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors nil))

(defun xq/add-native-comp-deny-pattern (pattern)
  "Add PATTERN to native compilation deny lists when available."
  (when (boundp 'native-comp-jit-compilation-deny-list)
    (add-to-list 'native-comp-jit-compilation-deny-list pattern))
  (when (boundp 'native-comp-deferred-compilation-deny-list)
    (add-to-list 'native-comp-deferred-compilation-deny-list pattern)))

(dolist (pattern '("general\\.el\\'"
                   "go-mode\\.el\\'"
                   "org-roam\\(?:-.*\\)?\\.el\\'"))
  (xq/add-native-comp-deny-pattern pattern))

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
(require 'use-package)

;; Keep Customize output out of init.el to reduce merge noise.
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'nomessage))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load machine-local override support before shared feature modules.
(require 'xq-overrides)
(xq/load-local-file xq/local-pre-init-file)

;; Load feature modules, allowing machine-local files to disable modules first.
(dolist (feature xq/modules)
  (xq/require-module feature))

;; Load machine-local overrides last so they can intentionally win.
(xq/load-local-file xq/local-post-init-file)

;;; init.el ends here
