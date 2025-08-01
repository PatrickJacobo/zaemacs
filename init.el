;;; init.el --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Patrick Lee <leepatrick338@gmail.com>
;; Copyright © 2025, Patrick Lee, all rights reserved.
;; Created: 25 April 2025
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  ;;; init --- Summary -*- lexical-binding: t; -*-
;;
;; Author: Patrick Lee <leepatrick338@gmail.com>
;; Copyright © 2025, Patrick Lee, all rights reserved.
;; Created: 25 April 2025
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; The config, remove requires if not needed
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
;; Elpaca bootstrap
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth))) ;
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Macro for using included packages
(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :ensure nil
     ,@args))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))
(if debug-on-error
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))
(add-to-list 'load-path "~/.config/emacs/schizo/modes")
(add-to-list 'load-path "~/.config/emacs/schizo/feature")

;; DESKTOP STUFF uncomment to enable exwm
;; (add-to-list 'load-path "~/.config/emacs/schizo/desktop")
;; (require 'mysmallexwm)
;; DESKTOP STUFF END


(setq user-full-name "Patrick Lee")
(setq user-mail-addres "leepatrick338@gmail.com")
(setq gc-cons-threshold #x40000000)
(setq read-process-output-max (* 1024 1024 4))

(recentf-mode 1)
(setq recentf-max-menu-items 25
      recentf-max-saved-items 25)

;; (electric-pair-mode 1)
(global-set-key [escape] 'keyboard-escape-quit)

;; (add-to-list 'load-path "~/.config/emacs/schizo/aesthetics")
;; (add-to-list 'load-path "~/.config/emacs/schizo/modes")

(use-package general
  :ensure (:wait t)
  :demand t
  :config
  (general-override-mode)
  (general-auto-unbind-keys)

  (general-define-key
   :keymaps 'override
   :states '(insert motion hybrid motion visual operator emacs)
   :prefix-map '+prefix-map
   :prefix-command '+prefix-map
   :prefix "SPC"
   :global-prefix "M-SPC")
  (general-create-definer global-definer
    :wk-full-keys nil
    :keymaps '+prefix-map)
  (global-definer
    "SPC" 'execute-extended-command
    "/" 'occur
    "!" 'shell-command
    ":" 'eval-expression
    "h" (general-simulate-key "C-h" :which-key "help")
    "w" (general-simulate-key "C-w" :which-key "windows")
    "z" '((lambda (local) (interactive "p")
            (unless repeat-mode (repeat-mode))
            (let ((local current-prefix-arg)
                  (current-prefix-arg nil))
              (call-interactively (if (local #'text-scale-adjust #'global-text-scale-adjust)))))
          :which-key "zoom"))
  (general-create-definer global-leader
    :keymaps 'override
    :states '(insert normal hybrid motion visual operator)
    :prefix "SPC m"
    :non-normal-prefix "S-SPC m"
    "" '( :ignore t
          :which-key
          (lambda (arg)
            (cons (cadr (split-string (car arg) " "))
                  (replace-regexp-in-string "-mode$" "" (symbol-name major-mode))))))
  ;; Stolen from progfolio, elpaca's author cause this macro looking good
  (defmacro +general-global-menu! (name prefix-key &rest body)
    "Create a definer named +general-global-NAME wrapping global-definer.
Create prefix map: +general-global-NAME-map. Prefix bindings in BODY with PREFIX-KEY."
    (declare (indent 2))
    (let* ((n (concat "+general-global-" name))
           (prefix-map (intern (concat n "-map"))))
      `(progn
         (general-create-definer ,(intern n)
           :wrapping global-definer
           :prefix-map (quote ,prefix-map)
           :prefix ,prefix-key
           :wk-full-keys nil
           "" '(:ignore t :which-key ,name))
         (,(intern n) ,@body))))
  ;; elpaca stuff
  (+general-global-menu! "elpaca" "a"
    "p" '(:ignore t "elpaca")
    "pb" 'elpaca-browse
    "pr" '((lambda () (interactive)
             (let ((current-prefix-arg (not current-prefix-arg))
                   (this-command 'elpaca-rebuild))
               (call-interactively #'elpaca-rebuild)))
           :which-key "rebuild")
    "pm" 'elpaca-manager
    "pl" 'elpaca-log
    "pi" 'elpaca-info
    "pI" '((lambda () (interactive) (info "Elpaca"))
           :which-key "elpaca-info")
    "ps" 'elpaca-status
    "pt" 'elpaca-try
    "pv" 'elpaca-visit)
  ;; shell stuff
  ;; should move it out
  (+general-global-menu! "Terminals" "v"
    "t" 'vterm
    "y" 'eshell)
  (+general-global-menu! "Dired" "e"
    "e" 'find-file
    "m" 'mkdir)
  (+general-global-menu! "vc-control" "g")
  (+general-global-menu! "search" "s")

  ;; buffer stuff
  (+general-global-menu! "buffer" "b"
    "d" 'kill-current-buffer
    "d" 'ibuffer
    "o" '((lambda () (interactive) (switch-to-buffer nill))
          :which-key "other-buffer")
    "p" 'previous-buffer
    "i" 'ibuffer
    "r" 'rename-buffer
    "R" 'revert-buffer
    "M" '((lambda () (interactive) (switch-to-buffer "*Messages*"))
          :which-key "messages-buffer")
    "n" 'next-buffer
    "s" 'scratch-buffer
    "TAB" '((lambda () (interactive) (switch-to-buffer nil))
            :which-key "other-buffer")))



;; Aesthetics
;; Theme
;; org

;;magit (peak)
(setq vc-follow-symlinks t)
(use-package transient  :defer t)
(use-package magit
  :defer t
  :after (general)
  :general
  (+general-global-vc-control
    "b"  'magit-branch
    "B"  'magit-blame
    "c"  'magit-clone
    "f"  '(:ignore t :which-key "file")
    "ff" 'magit-find-file
    "fh" 'magit-log-buffer-file
    "i"  'magit-init
    "L"  'magit-list-repositories
    "m"  'magit-dispatch
    "S"  'magit-stage-file
    "g"  'magit-status
    "U"  'magit-unstage-file))
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))
;; :config)
;; (+global-general-vc-control-magit
;;  "g" 'magit-status-here))
;;  ;; "c" 'magit-commit))

(use-package emacs-everywhere
  :ensure t)
(use-feature org)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package  flycheck-indicator
  :after flycheck
  :ensure t
  :hook flycheck-mode)

;; ;;ibuffer
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/org/" "~/uni/" "~/.config" "~/git"))
  :config
  ;; I typically use this keymap prefix on macOS
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; On Linux, however, I usually go with another one
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package ibuffer-vc
  :ensure t
  :hook (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root))
(use-package ibuffer-projectile
  :ensure t
  ;; Group ibuffer's list by project root
  :hook (ibuffer . ibuffer-projectile-set-filter-groups))
(use-package pdf-tools
  :ensure t)
(use-package saveplace-pdf-view
  :ensure t
  :after pdf-view)
;; emacs yeah cause emacs emacses
(use-feature emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil))





;; completion cause I want it complete

(use-package wgrep
  :ensure t)
(use-package nerd-icons-completion
  :ensure t)
(use-package consult-flycheck
  :ensure t)
(use-package consult-yasnippet
  :ensure t)

;; term cause baby I ain't on bad terms
(use-package tldr
  :ensure t)



;; lisp mode stuff
;; TODO:
(add-hook 'emacs-lisp-mode-hook (lambda () (indent-tabs-mode -1)))
(use-package ledger-mode)

(use-feature dired
  :commands dired-jump
  :init
  ;; some goodies stolen from doom emacs
  (setq dired-dwim-target t
        dired-auto-revert-buffer #'dired-buffer-stale-p
        dired-create-destination-dirs 'ask
        dired-recursive-copies 'always
        dired-recursive-deletes 'top))
(use-package nerd-icons-dired
  :ensure t
  :after dired
  :hook
  (dired-mode . nerd-icons-dired-mode))
(use-package diredfl
  :ensure t
  :after dired
  :config
  (diredfl-global-mode))
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(auto-save-visited-mode 1)


(require 'verticaldisability)
(require 'solicitation)
(require 'not-my-litter)
(require 'malificient-keys)
(require 'mybigtree)
(require 'schizo-which-key)
(require 'organizational)
(require 'aesthetics)
(require 'thecorrectformat)
(require 'philosophy)
(require 'markingdown)
(require 'elespizzler)
(require 'zaeditor)
(require 'zalisper)
(require 'zashell)
(require 'schizomonad)
(require 'schizotoml)
(require 'billsgottobepaid)
(require 'thedatadontlie)
(require 'thepython)
(require 'rusteeze)
(require 'statisticalnotebook)
(require 'hopf-latex)

(setq password-cache-expiry nil)

(cl-defmacro lsp-org-babel-enable (lang)
  "Support LANG in org source code block."
  (setq centaur-lsp 'lsp-mode)
  (cl-check-type lang string)
  (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
         (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
    `(progn
       (defun ,intern-pre (info)
         (let ((file-name (->> info caddr (alist-get :file))))
           (unless file-name
             (setq file-name (make-temp-file "babel-lsp-")))
           (setq buffer-file-name file-name)
           (lsp-deferred)))
       (put ',intern-pre 'function-documentation
            (format "Enable lsp-mode in the buffer of org source block (%s)."
                    (upcase ,lang)))
       (if (fboundp ',edit-pre)
           (advice-add ',edit-pre :after ',intern-pre)
         (progn
           (defun ,edit-pre (info)
             (,intern-pre info))
           (put ',edit-pre 'function-documentation
                (format "Prepare local buffer environment for org source block (%s)."
                        (upcase ,lang))))))))
(defvar org-babel-lang-list
  '( "python" "ipython" "rust"))
(dolist (lang org-babel-lang-list)
  (eval `(lsp-org-babel-enable ,lang)))

(use-feature autoinsert
  :after yasnippet
  :init

  (auto-insert-mode 1)
  (defun ha/autoinsert-yas-expand()
    "Replace text in yasnippet template."
    (yas-expand-snippet (buffer-string) (point-min) (point-max)))
  (setq auto-insert-query nil)
  (setq auto-insert-directory (locate-user-emacs-file "templates"))
  (add-hook 'find-file-hook 'auto-insert)
  (auto-insert-mode 1)
  ;; :config
  (define-auto-insert "\\.el?$" ["default-elisp.el" ha/autoinsert-yas-expand])
  (define-auto-insert "\\.sh?$" ["default-sh.sh" ha/autoinsert-yas-expand])
  (define-auto-insert "\\.php?$" ["default-php.php" ha/autoinsert-yas-expand])
  (define-auto-insert "\\.org?$" ["default-org.org" ha/autoinsert-yas-expand]))

;; this is temporary,shall be moved to a file some day
;; (enable-remote-dir-locals t))

;; I do not want this, it is getting git ignored, still it will get autogenerated
(setq custom-file "~/.config/emacs/customshit.el")
(load custom-file)


(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
