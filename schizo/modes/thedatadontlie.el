(use-package csv-mode
  :ensure t)
(use-feature nxml-mode
  :mode "\\.p\\(?:list\\|om\\)\\'" ; plist, pom
  :mode "\\.xs\\(?:d\\|lt\\)\\'"   ; xslt, xsd
  :mode "\\.rss\\'"
  :config
  (setq nxml-slash-auto-complete-flag t
        nxml-auto-insert-xml-declaration-flag t))
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'"
  :config
  (add-hook 'yaml-mode-hook  (lambda () (setq tab-width yaml-indent-offset))))

(provide 'thedatadontlie)
