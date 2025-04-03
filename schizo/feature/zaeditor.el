;; Stolen from Icy-Thought
(use-package editorconfig
  :ensure t
  :hook (prog-mode . editorconfig-mode))

(use-package smartparens
  :ensure t  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package direnv
  :hook (emacs-startup . direnv-mode)
  :config (add-to-list 'warning-suppress-types' '(direnv))
  :custom (direnv-always-show-summary nil))

(use-feature executable
  :hook (after-save . executable-make-buffer-file-executable-if-script-p))

(use-feature autorevert
  :diminish (auto-revert-mode)
  :hook (emacs-startup . global-auto-revert-mode)
  :custom
  (auto-revert-interval 1)
  (auto-revert-notify t)
  (auto-revert-verbose t))

(use-feature ediff
  :hook
  ((ediff-prepare-buffer . outline-show-all)
   (ediff-quit . winner-undo))
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))
(defvar-local +electric-indent-words '()
  "The list of electric words. Typing these will trigger reindentation of the
current line.")
(use-feature electric
  :config
  (electric-indent-mode))

(use-package quickrun
  :ensure t)

(use-package eros
  :ensure t
  :config
  (eros-mode 1))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
 

(provide 'zaeditor)

