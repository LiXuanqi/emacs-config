;;; xq-terminal.el --- Terminal experience improvements -*- lexical-binding: t; -*-

(defun xq/terminal-project-root ()
  "Return current project root directory, or `default-directory'."
  (if-let* ((project (project-current nil))
            (root (car (project-roots project))))
      root
    default-directory))

(defun xq/terminal-vterm-here ()
  "Open `vterm' in current project root."
  (interactive)
  (let ((default-directory (xq/terminal-project-root)))
    (vterm (generate-new-buffer-name "*xq-vterm*"))))

(use-package vterm
  :commands (vterm)
  :init
  (setq vterm-max-scrollback 10000))

(provide 'xq-terminal)
;;; xq-terminal.el ends here
