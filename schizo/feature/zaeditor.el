;;; zaeditor.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Patrick Lee <leepatrick338@gmail.com>
;; Copyright © 2025, Patrick Lee, all rights reserved.
;; Created:  2025
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  Some editor stuff
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

(use-feature saveplace
  :hook
  (after-init . save-place-mode))

(use-feature savehist
  :hook (after-init . savehist-mode)
  :custom
  (savehist-additional-variables '(abbrev-minor-mode-table-alist)))

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

(use-package fancy-compilation
  :ensure t
  :config
  (fancy-compilation-mode)
  :custom
  (fancy-compilation-override-colors nil)
  (fancy-compilation-scroll-output 'first-error))



(use-package expand-region
  :bind ("C-;" . er/expand-region))


(provide 'zaeditor)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zaeditor.el ends here
