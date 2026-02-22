;;; xq-org.el --- Org mode setup for notes -*- lexical-binding: t; -*-

(defconst xq/org-directory (expand-file-name "notes" (getenv "HOME"))
  "Base directory for Org files.")

(defconst xq/org-daily-directory (expand-file-name "daily" xq/org-directory)
  "Directory for daily Org notes.")

(defun xq/org--ensure-daily-file (file)
  "Ensure FILE exists and has a title header."
  (unless (file-directory-p (file-name-directory file))
    (make-directory (file-name-directory file) t))
  (unless (file-exists-p file)
    (with-temp-file file
      (insert "#+title: " (format-time-string "%Y-%m-%d") "\n\n"))))

(defun xq/org-open-today-note ()
  "Open today's daily Org note."
  (interactive)
  (let ((file (expand-file-name (format-time-string "%Y-%m-%d.org")
                                xq/org-daily-directory)))
    (xq/org--ensure-daily-file file)
    (find-file file)))

(defun xq/org-open-inbox ()
  "Open the default Org inbox file."
  (interactive)
  (unless (file-directory-p xq/org-directory)
    (make-directory xq/org-directory t))
  (find-file (expand-file-name "inbox.org" xq/org-directory)))

(use-package org
  :straight (:type built-in)
  :init
  (setq org-directory xq/org-directory
        org-default-notes-file (expand-file-name "inbox.org" xq/org-directory)
        org-agenda-files (list xq/org-directory)
        org-hide-emphasis-markers t
        org-log-done 'time)
  :config
  (unless (file-directory-p xq/org-directory)
    (make-directory xq/org-directory t))
  (setq org-capture-templates
        `(("t" "Todo" entry
           (file+headline ,org-default-notes-file "Tasks")
           "* TODO %?\n  %U\n")
          ("n" "Note" entry
           (file+headline ,org-default-notes-file "Notes")
           "* %?\n  %U\n"))))

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

(provide 'xq-org)
;;; xq-org.el ends here
