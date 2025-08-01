;;; schizomonad.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Patrick Lee <leepatrick338@gmail.com>
;; Copyright © 2025, Patrick Lee, all rights reserved.
;; Created:  2025
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Haskell cus' yeah
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

(use-package haskell-mode
  :ensure t
  :defer t
  :init (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)
  (add-hook 'haskell-mode-hook #'lsp)
  :bind (:map haskell-mode-map
              ("C-c h" . hoogle)
              ("C-c s" . haskell-mode-stylish-buffer))
  :config (message "Loaded haskell-mode"))
;; (setq haskell-mode-stylish-haskell-path ""))



(use-package lsp-haskell
  :ensure t
  :after lsp
  :config (message "Loaded lsp-haskell"))

(use-package dhall-mode
  :ensure t)

;;(use-package intero
;;  :ensure t
;;  :hook ((haskell-mode . intero-mode)))

(provide 'schizomonad)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; schizomonad.el ends here
