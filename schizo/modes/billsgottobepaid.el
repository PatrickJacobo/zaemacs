;;; billsgottobepaid.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Patrick Lee <leepatrick338@gmail.com>
;; Copyright © 2025, Patrick Lee, all rights reserved.
;; Created:  2025
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Php cus bills
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
(use-package php-mode
  :ensure t)
(use-package psysh
  :ensure t)
(use-package php-refactor-mode
  :ensure t)
(use-package composer
  :ensure t)
(use-package phpunit)
;; (use-package lsp-mode
;;   :ensure nil
;;   :config
;;   ;; register remote intelephense
;;   (lsp-register-client
;;    (make-lsp-client :new-connection 
;;                     (lsp-tramp-connection lsp-intelephense-server-command)
;;                     :activation-fn (lsp-activate-on "php")
;;                     :priority -1
;;                     :notification-handlers 
;;                     (ht ("indexingStarted" #'ignore)
;;                         ("indexingEnded" #'ignore))
;;                     :initialization-options 
;;                     (lambda ()
;;                       (list :storagePath lsp-intelephense-storage-path
;;                             :globalStoragePath 
;;                             lsp-intelephense-global-storage-path
;;                             :licenceKey lsp-intelephense-licence-key
;;                             :clearCache lsp-intelephense-clear-cache))
;;                     :multi-root lsp-intelephense-multi-root
;;                     :completion-in-comments? t
;;                     :remote? t
;;                     :server-id 'iph-remote
;;                     :synchronize-sections '("intelephense"))))


(provide 'billsgottobepaid)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; billsgottobepaid.el ends here
