 (use-package lsp-mode
   :ensure (:wait t)
   :init
    (setq read-process-output-max (* 1024 1024)) ;; 1mb
        :hook
        (lsp-mode . lsp-enable-which-key-integration)
        :commands lsp)
(use-package lsp-ui
  :ensure t)

(provide 'elespizzler)
