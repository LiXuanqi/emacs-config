;;; ai-config.el --- AI-related configurations -*- lexical-binding: t; -*-

;;; Commentary:

;; AI-related configurations for enhanced development experience

;;; Code:

;;; Copilot configuration
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(provide 'ai-config)
;;; ai-config.el ends here
