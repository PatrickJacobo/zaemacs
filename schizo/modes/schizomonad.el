;; -*- coding: utf-8; lexical-binding: t -*-

;; Configure

;; (defun enzuru-configure-haskell-mode ()
;;   (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;   (add-hook 'haskell-mode-hook 'turn-on-haskeull-indent)
;;   (put 'downcase-region 'disabled nil))

;; Packages

(use-package haskell-mode
 :ensure t
 :defer t
 :init (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
 (add-hook 'haskell-mode-hook #'lsp)
 :bind (:map haskell-mode-map
        ("C-c h" . hoogle)
        ("C-c s" . haskell-mode-stylish-buffer))
 :config (message "Loaded haskell-mode")
 (setq haskell-mode-stylish-haskell-path "ormolu"))



(use-package lsp-haskell
 :ensure t
 :after lsp
 :config (message "Loaded lsp-haskell"))

 
;;(use-package intero
;;  :ensure t
;;  :hook ((haskell-mode . intero-mode)))

(provide 'schizomonad)
