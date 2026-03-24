;;; xq-terminal.el --- Terminal experience improvements -*- lexical-binding: t; -*-

(require 'project)

(eval-and-compile
  (declare-function vterm "vterm")
  (declare-function evil-local-mode "evil")
  (declare-function evil-normal-state "evil")
  (declare-function evil-emacs-state "evil")
  (defvar vterm-max-scrollback))

(defvar-local xq/vterm-command-mode nil
  "Non-nil when the current `vterm' buffer routes keys to Emacs/Evil.")

(defun xq/vterm-terminal-mode ()
  "Route keys to the terminal program in the current `vterm' buffer."
  (when (bound-and-true-p evil-local-mode)
    (evil-local-mode -1))
  (when (fboundp 'evil-emacs-state)
    (evil-emacs-state))
  (setq-local xq/vterm-command-mode nil)
  (message "vterm: terminal mode"))

(defun xq/vterm-command-mode ()
  "Route keys to Emacs/Evil in the current `vterm' buffer."
  (when (fboundp 'evil-local-mode)
    (evil-local-mode 1))
  (when (fboundp 'evil-normal-state)
    (evil-normal-state))
  (setq-local xq/vterm-command-mode t)
  (message "vterm: command mode"))

(defun xq/vterm-toggle-input-mode ()
  "Toggle whether `vterm' keys go to the terminal or Emacs/Evil."
  (interactive)
  (if xq/vterm-command-mode
      (xq/vterm-terminal-mode)
    (xq/vterm-command-mode)))

(defun xq/terminal-project-root ()
  "Return current project root directory, or `default-directory'."
  (if-let* ((project (project-current nil))
            (root (project-root project)))
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
  (setq vterm-max-scrollback 10000)
  :config
  (add-hook 'vterm-mode-hook #'xq/vterm-terminal-mode)
  (define-key vterm-mode-map (kbd "C-c C-z") #'xq/vterm-toggle-input-mode))

(provide 'xq-terminal)
;;; xq-terminal.el ends here
