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
