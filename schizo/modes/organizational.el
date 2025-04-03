(use-feature org
  :defer t
  :config
  (setq visual-fill-column-mode 1
    visual-line-mode 1))
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

(provide 'organizational)
