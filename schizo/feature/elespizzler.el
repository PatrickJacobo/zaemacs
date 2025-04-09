;;; elespizzler.el --- pizzler the rizzler -*- lexical-binding: t; -*-
(use-package lsp-mode
  :ensure (:wait t)
  :init
   (setq read-process-output-max (* 1024 1024)) ;; 1mb
  :hook
   (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  :config
  (lsp-headerline-breadcrumb-mode -1))
(use-package lsp-ui
  :ensure t)

(provide 'elespizzler)
