;;; organization.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Patrick Lee <leepatrick338@gmail.com>
;; Copyright © 2025, Patrick Lee, all rights reserved.
;; Created:  2025
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Org mode is kind of nice
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
(use-feature org
  :defer t
  :hook variable-pitch
  :config
  (setq visual-fill-column-mode 1
        visual-line-mode 1)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (shell . t)
     (haskell . t)
     (emacs-lisp . nil))))
;; (bash . t))))

(use-package visual-fill-column
  :ensure (:wait t)
  :init
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t))
(use-package org-present
  :init
  (defun my/org-present-start ()
    ;; center the presentation and wrap lines
    (visual-fill-column-mode 1)
    (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch
                                                (header-line (:height 4.0) variable-pitch)
                                                (org-document-title (:height 1.75) org-document-title)
                                                (org-code (:height 1.55) org-code)
                                                (org-verbatim (:height 1.55) org-verbatim)
                                                (org-block (:height 1.25) org-block)))
                (org-block-begin-line (:height 0.7) org-block)
                (visual-line-mode 0))

    (defun my/org-present-end ()
      ;; Stop centering the document
      (visual-fill-column-mode 0)
      (visual-line-mode 0))
    (add-hook 'org-present-mode-hook 'my/org-present-start)
    (add-hook 'org-present-mode-quit-hook 'my/org-present-end)))

(use-package literate-calc-mode
  :ensure t)
(use-package org-auto-tangle
  :ensure t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))
(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode)
  (markdown-mode . toc-org-mode))

(use-package org-superstar
  :ensure t
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-download
  :init
  (add-hook 'dired-mode-hook 'org-download-enable)
  (add-hook 'org-mode-hook 'org-download-enable))

(use-package org-sticky-header
  :hook org-mode)


(provide 'organizational)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; organizational.el ends here
