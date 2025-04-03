

;; stolen from masteringemacs cause they do be cool
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(use-feature treesit
  :config
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (dhall "https://github.com/jbellerb/tree-sitter-dhall")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
     (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (xml "https://github.com/tree-sitter-grammars/tree-sitter-xml" "master" "xml/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml"))))



(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :commands (combobulate)
  :hook (tree-sitter-after-on . combobulate-mode))

(use-package treesit-fold
  :ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
  :hook (emacs-startup . global-treesit-fold-mode))

(provide 'mybigtree)
