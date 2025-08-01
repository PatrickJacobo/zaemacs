;;; aesthetics.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Patrick Lee <leepatrick338@gmail.com>
;; Copyright © 2025, Patrick Lee, all rights reserved.
;; Created:  2025
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Aesthetic stuff
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

;; (use-package emojify
;;   :init
;;   (global-emojify-mode)
;;   :config
;;   (setq emojify-styles '(ascii github unicode)) ;;   (emojify-set-emoji-styles emojify-styles))
(use-package catppuccin-theme
  :ensure t
  :config
  (load-theme 'catppuccin :no-confirm))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '( ;; For reminders to change or add something at a later date.
          ("TODO" warning bold)
          ;; For code (or code paths) that are broken, unimplemented, or slow,
          ;; and may become bigger problems later.
          ("FIXME" error bold)
          ;; For code that needs to be revisited later, either to upstream it,
          ;; improve it, or address non-critical issues.
          ("REVIEW" font-lock-keyword-face bold)
          ;; For code smells where questionable practices are used
          ;; intentionally, and/or is likely to break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For sections of code that just gotta go, and will be gone soon.
          ;; Specifically, this means the code is deprecated, not necessarily
          ;; the feature it enables.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; Extra keywords commonly found in the wild, whose meaning may vary
          ;; from project to project.
          ("NOTE" success bold)
          ("BUG" error bold)
          ("XXX" font-lock-constant-face bold)))
  (global-hl-todo-mode))
;; Emacs? Why?????
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; `blink-cursor-mode'
;; "Besides, your editor came to a thoroughly objective conclusion many
;; years ago that blinking cursors are an annoying distraction and that
;; any developer implementing such behavior should be sentenced to ten
;; years of COBOL coding under a strobe light."
(blink-cursor-mode -1)

(setq inhibit-startup-screen t) ;; I wanted VIM
(setq inhibit-startup-message t)

;; (global-display-line-numbers-mode 'relative)
(setq initial-scratch-message "\
;; DO NOT PANIC   -*- lexical-binding: t; -*-
")
(add-hook 'prog-mode 'column-number-mode)
(set-face-attribute 'default nil :family "Iosevka" :height 110)
;; (set-face-attribute 'variable-pitch nil :family "Iosevka"ll)
(column-number-mode)
(display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(use-feature simple
  ;; :general
  ;; (+general-global-toggle
  ;;   "f" 'auto-fill-mode)
  :custom
  (eval-expression-debug-on-error nil)
  (fill-column 80 "Wrap at 80 columns."))


;; (use-package doom-themes
;;     :ensure t
;;     :config
;;     ;; Global settings (defaults)
;;     (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;     (load-theme 'doom-tokyo-night t)

;;     ;; Enable flashing mode-line on errors
;;     (doom-themes-visual-bell-config)
;;     ;add-hook 'prog; Enable custom neotree theme (nerd-icons must be installed!)
;;     (doom-themes-neotree-config)
;;     ;; or for treemacs users
;;     (setq doom-themes-treemacs-theme "doom-tokyo-night") ; use "doom-colors" for less minimal icon theme
;;     (doom-themes-treemacs-config)
;;     ;; Corrects (and improves) org-mode's native fontification.
;;     (doom-themes-org-config))

(setq display-line-numbers 'relative)
(setq scroll-margin 8)
(pixel-scroll-precision-mode)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(use-package nyan-mode
  :config
  (nyan-mode 1))
(use-package beacon
  :config
  (beacon-mode 1))
(use-package solaire-mode
  :ensure t
  :config (solaire-global-mode +1))

(use-package rainbow-mode
  :ensure t
  :hook
  ((org-mode prog-mode) . rainbow-mode))

(use-package fireplace
  :ensure t)

;; This is the correct way to make a cursor
(setq evil-normal-state-cursor '(box "white"))
(setq evil-insert-state-cursor '(box "white"))
(setq evil-visual-state-cursor '(box "white"))
(xterm-mouse-mode 1)
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package indent-bars
  :ensure t
  :config
  (setq indent-bars-starting-column 0
        indent-bars-width-frac 0.15
        indent-bars-color-by-depth nil
        indent-bars-treesit-support 1)
  :hook
  (prog-mode . indent-bars-mode))

(use-package unicode-fonts
  :ensure t
  :config
  (unicode-fonts-setup))
;; áéñ§  ñ ń ćĆçą
;; (use-package ligature
;;   :ensure t
;;   :config
;;   (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!=")
;;                               "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
;;                               "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
;;                               "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
;;                               "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
;;                               "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
;;                               "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
;;                               "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
;;                               "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
;;                               "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
;;                               "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
;;                               ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
;;                               "<:<" ";;;")
;;   (global-ligature-mode t))
(use-package ligature
  :ensure t
  :config
  ;; Enable all Iosevka Aileligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))
(use-package vi-tilde-fringe
  :ensure t
  :config
  (global-vi-tilde-fringe-mode))
(use-package nav-flash
  :ensure t
  :defer t
  :config
  (nav-flash-show))
(use-package highlight-numbers
  :ensure (:host github :repo "Fanael/highlight-numbers")
  :hook prog-mode)



(use-package popwin)

(use-package helpful
  :ensure (:host github :repo "slotThe/helpful")
  :after popwin
  :bind (("C-c C-d"                 . #'helpful-at-point)
         ([remap describe-function] . #'helpful-callable)
         ([remap describe-variable] . #'helpful-variable)
         ([remap describe-key     ] . #'helpful-key))
  :custom
  (helpful-hide-docstring-in-source t)
  (helpful-switch-buffer-function   ; re-use the same buffer when going deeper
   (lambda (buffer-or-name)
     (if (eq major-mode 'helpful-mode)
         (popwin:switch-to-buffer buffer-or-name)
       (popwin:pop-to-buffer buffer-or-name)))))

(use-package hide-mode-line
  :ensure t)

(use-feature prog-mode
  :ensure nil
  :config
  (global-font-lock-mode)
  (setq-default   ; Always indent with spaces, never use tabs; #FuckTabs
   indent-tabs-mode  nil
   default-tab-width 2)
  (setq standard-indent 2))

(use-feature compile ;; http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html
  :hook (compilation-filter . ansi-color-compilation-filter)
  :bind (:map compilation-mode-map
              ("z" . delete-window))
  :custom
  (compilation-scroll-output 'first-error) ; Scroll to first error
  (compilation-auto-jump-to-first-error t)
  ;; Just do it.
  (compilation-always-kill t)
  (compilation-ask-about-save nil))

(provide 'aesthetics)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; aesthetics.el ends here
