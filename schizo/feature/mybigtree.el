

;; stolen from masteringemacs cause they do be cool
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(use-feature treesit
  :config
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
(provide 'mybigtree)
