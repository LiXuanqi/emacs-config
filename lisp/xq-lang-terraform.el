;;; xq-lang-terraform.el --- Terraform language defaults -*- lexical-binding: t; -*-

(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers ".terraform.lock.hcl"))

(defun xq/lang-terraform-setup ()
  "Enable Terraform defaults and lsp-mode in current buffer."
  (setq-local tab-width 2
              indent-tabs-mode nil)
  (when (fboundp 'lsp-deferred)
    (lsp-deferred)))

(provide 'xq-lang-terraform)
;;; xq-lang-terraform.el ends here
