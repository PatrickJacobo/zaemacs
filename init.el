;; Elpaca bootstrap
(defvar elpaca-installer-version 0.10)
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
(require 'malificient-keys)
(require 'mybigtree)
(require 'schizo-which-key)
(require 'philosophy)
(require 'elespizzler)
;; (add-to-list 'load-path "~/.config/emacs/schizo/aesthetics")
;; (add-to-list 'load-path "~/.config/emacs/schizo/modes")
;; end of init.el

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
  :global-prefix "S-SPC")
 (general-create-definer global-definer
     :wk-full-keys nil
     :keymaps '+prefix-map)
 (global-definer
  "SPC" 'execute-extended-command
  "/" 'occur
  "!" 'shell-command
  ":" 'eval-expression
  "h" (general-simulate-key "C-h" :which-key "help")
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
 (+general-global-menu! "Terminals" "v"
       "t" 'vterm
       "y" 'eshell)
 (+general-global-menu! "Dired" "e"
   "e" 'find-file
   "m" 'mkdir)
 (+general-global-menu! "Magit" "g"
   "g" 'magit-status-here)
   
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
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(global-display-line-numbers-mode)
(add-hook 'prog-mode 'column-number-mode)
(set-face-attribute 'default nil :family "Iosevka" :height 110)
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
(column-number-mode)
;; Theme
(use-package doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (load-theme 'doom-tokyo-night t)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;add-hook 'prog; Enable custom neotree theme (nerd-icons must be installed!)
    (doom-themes-neotree-config)
    ;; or for treemacs users
    (setq doom-themes-treemacs-theme "doom-tokyo-night") ; use "doom-colors" for less minimal icon theme
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

(setq display-line-numbers 'relative)
(display-line-numbers-mode)
(use-package doom-modeline
 :ensure t
 :init (doom-modeline-mode 1))

;; org
(use-package org-auto-tangle
 :ensure t
 :hook (org-mode . org-auto-tangle-mode))
(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode)
  (markdown-mode . toc-org-mode))
(use-package solaire-mode
 :ensure t
 :config (solaire-global-mode +1))

;;magit (peak)
(use-package transient :defer t)
(use-package magit
 :defer t)

(use-package emacs-everywhere
 :ensure t)
(use-feature org)

(use-package parinfer-rust-mode
 :ensure t
 :hook emacs-lisp-mode
 :init
 (setq parinfer-rust-auto-download t))
(use-package flycheck
 :ensure t
 :config
 (add-hook 'after-init-hook #'global-flycheck-mode))
(use-package  flycheck-indicator
 :after flycheck
 :ensure t
 :hook flycheck-mode)
(use-package yasnippet
 :ensure t
 :config
 (yas-global-mode 1)
 (setq yas-snippets-dirs
     '("~/.config/emacs/snippets")))
(use-package doom-snippets
 :after yasnippet
 :ensure (:host github
                 :repo "doomemacs/snippets"
                 :files ("*.el" "*")))
;;ibuffer
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))
(use-package ibuffer-vc
  :ensure t
  :hook (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root))
(use-package ibuffer-projectile
  :ensure t
  ;; Group ibuffer's list by project root
  :hook (ibuffer . ibuffer-projectile-set-filter-groups)
  :config
  (setq ibuffer-projectile-prefix
        (if (modulep! +icons)
            (concat (nerd-icons-octicon
                     "nf-oct-file_directory"
                     :face ibuffer-filter-group-name-face
                     :v-adjust -0.05)
                    " ")
          "Project: ")))
(use-package pdf-tools
  :ensure t)
(use-package saveplace-pdf-view
  :ensure t
  :after pdf-view)
;; Haskell stuff
(use-package haskell-mode
  :ensure t
  :hook
  (haskell-mode . subword-mode)
  (haskell-mode .interactive-haskell-mode)
  (haskell-mode . turn-on-haskell-indentation)
  (haskell-mode .haskell-auto-insert-module-template))
(use-package dhall-mode
  :ensure t)
;; emacs yeah cause emacs emacses 
(use-feature emacs
  :custom
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil))

;; completion cause I want it complete
(use-package vertico
  :ensure t
  :config
  (vertico-mode))
(use-package corfu
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode))
(use-package consult
  :ensure t)
(use-package embark
  :ensure t)

(use-package embark-consult
  :ensure t)

(use-package marginalia
  :ensure t)


(use-package orderless
  :ensure t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package wgrep
  :ensure t)
(use-package nerd-icons-completion
  :ensure t)
(use-package consult-flycheck
  :ensure t)
(use-package consult-yasnippet
  :ensure t)

;; term cause baby I ain't on bad terms
(use-package vterm
  :ensure t)
(use-package eat
  :ensure t
  :hook eshell-load)
;; essential stuff
(auto-save-visited-mode)
(setq display-line-numbers-type 'relative)


;; :NOTE| Folding of code-blocks

(use-package treesit-fold
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :hook (emacs-startup . global-treesit-fold-mode))

  


;; :NOTE| Structural editing & navigation

;; (use-package combobulate
;;   :ensure (:host github :repo "mickeynp/combobulate")
;;   :commands (combobulate)
;;   :hook (tree-sitter-after-on . combobulate-mode))

;; Web stuff
;; (use-package psysh
;;   :ensuret t)

;; (use-package async
;;   :ensure t)
;; (use-package
;;   :ensure (:host github :repo "arnested/php-extras")
;;   :after async)

;; (use-package php-mode
;;   :ensure t)
;; (use-package php-refactor-mode
;;   :ensure t)
;; (use-package phpunit
;;   :ensure t)
;; (use-package composer
;;   :ensure t)
;; (package! psysh :pin "ae15a36301a49e5ae03118ff815a6a511603ae13")
;; (package! php-extras
;;   :recipe (:host github :repo "arnested/php-extras")
;;   :pin "d410c5af663c30c01d461ac476d1cbfbacb49367")
;; (package! php-mode :pin "0f756a8c0782ebdc00557addc68763305c91ca51")
;; (package! php-refactor-mode :pin "7a794b0618df2882b1bd586fdd698dba0bc5130d")
;; (package! phpunit :pin "650a50898de1fa4eeb47360b12aeb1126b2448c2")
;; (package! composer :pin "6c7e19256ff964546cea682edd21446c465a663c")

;; (when (modulep! +hack)
;;   (package! hack-mode
;;     :recipe (:host github :repo "hhvm/hack-mode")
;;     :pin "343e45f2a616c726f20ba099f3f98a1a01cec405"))

;; ;; For building php-extras
;; (package! async :pin "b99658e831bc7e7d20ed4bb0a85bdb5c7dd74142")

;; lisp mode stuff
(add-hook 'emacs-lisp-mode-hook (lambda () (indent-tabs-mode -1))) 
;; end of init.el
