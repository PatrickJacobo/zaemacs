;;; CUSTOMSHIT --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Patrick Lee <leepatrick338@gmail.com>
;; Copyright © 2025, Patrick Lee, all rights reserved.
;; Created:  6 April 2025
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  ;;; CUSTOMSHIT --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Patrick Lee <leepatrick338@gmail.com>
;; Copyright © 2025, Patrick Lee, all rights reserved.
;; Created:  6 April 2025
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
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
;;; Why do you do this emacs???? What's wrong with you????


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mediawiki-site-alist
   '(("MEDIAWIKI_1.39" "https://dev.mw.test" "admin" "Chachas!123" ""
      "Página Principal")
     ("Wikipedia" "https://en.wikipedia.org/w/" "username" "password" nil
      "Main Page")))
 '(package-selected-packages
   '(auctex avy cape consult-flycheck diff-hl eat embark-consult evil-anzu
            evil-args evil-goggles evil-matchit evil-nerd-commenter evil-numbers
            evil-surround exwm haskell-mode indent-bars marginalia markdown-mode
            orderless pdf-tools plz rainbow-delimiters rainbow-mode request
            smartparens undo-fu vertico wgrep ws-butler yasnippet-snippets))
 '(safe-local-variable-values
   '((ssh-deploy-root-remote . "/ssh:prod:/data/git/mediawiki_1.39/")
     (ssh-deploy-root-local . "~/git/mediawiki_1.39/")
     (ssh-deploy-remote-sql-database . "mwvintage")
     (ssh-deploy-root-remote . "/ssh:prod:/data/git/mediawiki_vintage/")
     (ssh-deploy-root-local . "~/git/mediawiki_vintage/")
     (ssh-deploy-remote-sql-user . "database_password")
     (ssh-deploy-remote-sql-password . "wikiuser")
     (ssh-deploy-remote-sql-database . "mwmineductest")
     (ssh-deploy-root-remote . "/ssh:prod:/data/git/mediawiki_test/")
     (ssh-deploy-root-local . "~/git/mediawiki_test/")
     (pyvenv-workon expand-file-name ".venv"
                    (locate-dominating-file default-directory ".dir-locals.el"))
     (ssh-deploy-root-remote . "/ssh:prod:/data/git/patrick/")
     (ssh-deploy-root-local . "~/git/patrick/")
     (ssh-deploy-remote-sql-user . "root")
     (ssh-deploy-remote-sql-password . "root")
     (ssh-deploy-remote-sql-database . "propiedaddb")
     (ssh-deploy-on-explicit-save . 1) (ssh-deploy-async . 1)
     (ssh-deploy-root-remote . "/ssh:prod:/data/git/ocr_test/")
     (ssh-deploy-root-local . "~/git/ocr_test/")))
 '(warning-suppress-types '((ox-latex) (emacs) '(direnv))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
