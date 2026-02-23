;;; xq-git.el --- Git workflow setup -*- lexical-binding: t; -*-

(use-package magit
  :commands (magit-status magit-log-current)
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-save-repository-buffers 'dontask
        git-commit-summary-max-length 72))

(provide 'xq-git)
;;; xq-git.el ends here
