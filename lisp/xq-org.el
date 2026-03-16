;;; xq-org.el --- Org mode setup for notes -*- lexical-binding: t; -*-

(defcustom xq/org-directory (expand-file-name "notes" (getenv "HOME"))
  "Base directory for Org files."
  :type 'directory
  :group 'xq)

(use-package org
  :straight (:type built-in)
  :init
  (setq org-directory xq/org-directory
        org-agenda-files (list xq/org-directory)
        org-hide-emphasis-markers t
        org-log-done 'time)
  :config
  (require 'org-tempo)
  (unless (file-directory-p xq/org-directory)
    (make-directory xq/org-directory t)))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))

(use-package org-appear
  :after (org evil)
  :init
  (setq org-appear-trigger 'manual
        org-appear-autolinks t
        org-appear-autoemphasis t
        org-appear-autosubmarkers t)
  :hook
  (org-mode . (lambda ()
                (org-appear-mode 1)
                (add-hook 'evil-insert-state-entry-hook
                          #'org-appear-manual-start
                          nil
                          t)
                (add-hook 'evil-insert-state-exit-hook
                          #'org-appear-manual-stop
                          nil
                          t))))

(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t
        org-roam-completion-everywhere t
        org-roam-dailies-capture-templates
        '(("t" "todo" entry
           "* TODO %?\n  %U\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n\n"))))
  :custom
  (org-roam-directory (file-truename (expand-file-name "roam" xq/org-directory)))
  (org-roam-dailies-directory (file-truename (expand-file-name "daily" xq/org-directory)))
  :config
  (unless (file-directory-p org-roam-directory)
    (make-directory org-roam-directory t))
  (org-roam-db-autosync-mode 1))

(provide 'xq-org)
;;; xq-org.el ends here
