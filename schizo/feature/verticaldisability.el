;;; verticaldisability.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Patrick Lee <leepatrick338@gmail.com>
;; Copyright © 2025, Patrick Lee, all rights reserved.
;; Created:  2025
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Vert&co
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))
(use-feature savehist
  :init (savehist-mode))
(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode)
  :config
  (setf (alist-get 'elpaca-info marginalia-command-categories) 'elpaca))

(use-package consult
  :ensure t
  :after (general)
  :general
  (+general-global-search
    "i" 'consult-imenu
    "d" 'consult-ripgrep
    "D" 'consult-dir
    "b" 'consult-buffer
    "p" 'consult-yank-pop
    "t" 'consult-theme))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-C-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package corfu
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))
;; (add-hook 'completion-at-point-functions #'cape-history)
;; ...



(use-package orderless
  :ensure t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))
;; Consult users will also want the embark-consult package.
;; (use-package embark-consult
;;   :ensure t ; only need to install it, embark loads it after consult if found
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

(use-package yasnippet
  :ensure (:wait t)
  :config
  (yas-global-mode 1)
  (setq yas-snippets-dirs
        '("~/.config/emacs/snippets")))
;; (use-package doom-snippets
;;  :after yasnippet
;;  :ensure (:host github
;;                  :repo "doomemacs/snippets"
;;                  :files ("*.el" "*")))
(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)
(use-package yasnippet-capf
  :ensure t
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))
(use-package auto-yasnippet
  :init
  (setq aya-persist-snippets-dir "~/.config/emacs/snippets")
  :ensure t)


;; (provide 'autoinsert)
;; (use-feature autoinsert
;;   :after yasnippet
;;   :init

;;  (auto-insert-mode 1)
;;  (defun ha/autoinsert-yas-expand()
;;     "Replace text in yasnippet template."
;;     (yas-expand-snippet (buffer-string) (point-min) (point-max)))
;;  (setq auto-insert-query nil)
;;  (setq auto-insert-directory (locate-user-emacs-file "templates"))
;;  (add-hook 'find-file-hook 'auto-insert)
;;  (auto-insert-mode 1)
;;  ;; :config
;;  (define-auto-insert "\\.el?$" ["default-elisp.el" ha/autoinsert-yas-expand])
;;  (define-auto-insert "\\.sh?$" ["default-sh.sh" ha/autoinsert-yas-expand])
;;  (define-auto-insert "\\.php?$" ["default-php.php" ha/autoinsert-yas-expand])
;;  (define-auto-insert "\\.org?$" ["default-org.org" ha/autoinsert-yas-expand]))

;; (general-define-key
;;  :prefix "SPC"
;;  :states 'normal
;;   "s" '(:ignore t "search")
;;   "si" 'consult-imenu
;;   "sd" 'consult-dir
;;   "sb" 'consult-buffer
;;   "sp" 'consult-yank-pop
;;   "st" 'consult-theme)

(provide 'verticaldisability)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; verticaldisability.el ends here
