;;; zashell.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Patrick Lee <leepatrick338@gmail.com>
;; Copyright © 2025, Patrick Lee, all rights reserved.
;; Created:  6 April 2025
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  Stuff for shell and ssh
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


(setq eshell-rc-script "~/.config/emacs/eshell/profile"
      eshell-aliases-file "~/.config/emacs/eshell/aliases"
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package eat
  :after eshell
  :ensure t
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  (eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback (* 50 1000)))


(use-package hydra)
(use-package ssh-deploy
  :ensure (:host github :repo "emacs-straight/ssh-deploy")
  :after hydra
  :hook ((after-save . ssh-deploy-after-save)
         (find-file . ssh-deploy-find-file))
  :config
  (ssh-deploy-line-mode)
  (ssh-deploy-hydra "C-c C-z"))
(eval-after-load 'tramp '(setenv "SHELL" "/bin/sh"))

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :ensure t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-did-you-mean
  :ensure t
  :config
  (eshell-did-you-mean-setup))

(setq eshell-prompt-regexp "^.* λ "
      eshell-prompt-function
      (lambda nil
        (concat
         (propertize (replace-regexp-in-string "/home/[a-z]+" "~" ( eshell/pwd )) 'face `(:foreground "orange"))
         (propertize "\nλ " 'face `(:foreground "lavender")))))




(provide 'zashell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zashell.el ends here
