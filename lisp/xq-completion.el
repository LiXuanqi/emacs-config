;;; xq-completion.el --- Completion and search stack -*- lexical-binding: t; -*-

(use-package savehist
  :init
  (savehist-mode 1))

(use-package vertico
  :init
  (vertico-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode 1))

(use-package consult
  :after vertico)

(provide 'xq-completion)
;;; xq-completion.el ends here
