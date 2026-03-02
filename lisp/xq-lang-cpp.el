;;; xq-lang-cpp.el --- C++ language defaults -*- lexical-binding: t; -*-

(with-eval-after-load 'project
  (add-to-list 'project-vc-extra-root-markers "compile_commands.json")
  (add-to-list 'project-vc-extra-root-markers "CMakeLists.txt"))

(defun xq/lang-cpp-setup ()
  "Enable C++ defaults and lsp-mode in current buffer."
  (setq-local c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)
  (when (fboundp 'lsp-deferred)
    (lsp-deferred)))

(provide 'xq-lang-cpp)
;;; xq-lang-cpp.el ends here
