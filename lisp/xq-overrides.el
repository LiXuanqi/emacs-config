;;; xq-overrides.el --- Machine-local override support -*- lexical-binding: t; -*-

(defgroup xq nil
  "Personal Emacs configuration."
  :group 'convenience)

(defcustom xq/modules
  '(xq-defaults
    xq-ui
    xq-which-key
    xq-evil
    xq-completion
    xq-treesit
    xq-lang
    xq-format
    xq-git
    xq-code-anchor
    xq-terminal
    xq-org
    xq-keybinds)
  "Ordered list of feature modules loaded from `init.el'."
  :type '(repeat symbol)
  :group 'xq)

(defcustom xq/local-pre-init-file
  (expand-file-name "lisp/xq-local-pre.el" user-emacs-directory)
  "Optional machine-local file loaded before shared modules.

Use this file to enable or disable modules and set variables that package
configuration reads during `:init'."
  :type 'file
  :group 'xq)

(defcustom xq/local-post-init-file
  (expand-file-name "lisp/xq-local-post.el" user-emacs-directory)
  "Optional machine-local file loaded after shared modules.

Use this file for last-mile overrides that should win after shared config has
fully loaded."
  :type 'file
  :group 'xq)

(defun xq/load-local-file (file)
  "Load FILE quietly when it exists."
  (when (file-exists-p file)
    (load file nil 'nomessage)))

(defun xq/module-enabled-p (feature)
  "Return non-nil when FEATURE should be loaded."
  (memq feature xq/modules))

(defun xq/disable-module (feature)
  "Remove FEATURE from `xq/modules'."
  (setq xq/modules (delq feature xq/modules)))

(defun xq/enable-module (feature &optional after)
  "Enable FEATURE in `xq/modules'.

When AFTER is non-nil, insert FEATURE after that module; otherwise append it."
  (unless (xq/module-enabled-p feature)
    (if after
        (let ((tail (memq after xq/modules)))
          (if tail
              (setcdr tail (cons feature (cdr tail)))
            (setq xq/modules (append xq/modules (list feature)))))
      (setq xq/modules (append xq/modules (list feature))))))

(defun xq/require-module (feature)
  "Require FEATURE when it is enabled in `xq/modules'."
  (when (xq/module-enabled-p feature)
    (require feature)))

(provide 'xq-overrides)
;;; xq-overrides.el ends here
