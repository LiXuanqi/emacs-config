;;; xq-defaults.el --- Sane defaults for general usage -*- lexical-binding: t; -*-

;; On macOS GUI Emacs, import shell PATH so external tools (e.g. LSP servers) resolve.
(when (memq window-system '(mac ns))
  (ignore-errors
    (straight-use-package 'exec-path-from-shell)
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize)))

(defconst xq/backup-directory (expand-file-name "backups/" user-emacs-directory)
  "Directory for backup files (e.g. filename~).")

(defconst xq/auto-save-directory (expand-file-name "auto-saves/" user-emacs-directory)
  "Directory for auto-save files.")

(unless (file-directory-p xq/backup-directory)
  (make-directory xq/backup-directory t))

(unless (file-directory-p xq/auto-save-directory)
  (make-directory xq/auto-save-directory t))

(setq backup-directory-alist `(("." . ,xq/backup-directory))
      auto-save-file-name-transforms `((".*" ,xq/auto-save-directory t))
      auto-save-list-file-prefix (expand-file-name ".saves-" xq/auto-save-directory))

;; Accept y/n answers instead of requiring full yes/no confirmations.
(setq use-short-answers t)

(provide 'xq-defaults)
;;; xq-defaults.el ends here
