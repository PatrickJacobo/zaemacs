;; -*- coding: utf-8; lexical-binding: t -*-

;; Functions
;; This is truly one of the most evil, villainous things

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-anzu
  :after evil
  :ensure t)

(use-package evil-args
  :after evil
  :ensure t)

(use-package evil-lion
  :after evil
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-goggles
  :after evil
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-nerd-commenter
              :after evil
              :ensure t
              :config
              (evilnc-default-hotkeys))

(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-numbers
  :ensure t
  :after evil
  :config
    (evil-define-key '(normal visual) 'global (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (evil-define-key '(normal visual) 'global (kbd "C-c -") 'evil-numbers/dec-at-pt)
    (evil-define-key '(normal visual) 'global (kbd "C-c C-+") 'evil-numbers/inc-at-pt-incremental)
    (evil-define-key '(normal visual) 'global (kbd "C-c C--") 'evil-numbers/dec-at-pt-incremental))

(provide 'malificient-keys)
