(setq-default abbrev-mode 1)

(use-package yasnippet
  :defer 2
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :defer)

(use-package ivy-yasnippet
  :bind ("C-c y" . ivy-yasnippet))

(use-package company
  :bind (:map prog-mode-map
         ("C-i" . company-indent-or-complete-common)
         ("C-M-i" . counsel-company))
  :hook (emacs-lisp-mode . company-mode))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode))

(use-package lsp-mode
  :hook ((c-mode c++-mode d-mode elm-mode go-mode js-mode kotlin-mode python-mode
          typescript-mode vala-mode web-mode)
         . lsp)
  :init
  (setq lsp-keymap-prefix "H-l"
        lsp-rust-analyzer-proc-macro-enable t)
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-show-code-actions t)
  :bind (("C-c A" . lsp-execute-code-action)
         ("C-c d" . lsp-ui-doc-show)
         ("C-c I" . lsp-ui-imenu)))

(use-package flycheck
  :defer)

(use-package js
  :straight nil
  :bind (:map js-mode-map
         ([remap js-find-symbol] . xref-find-definitions))
  :init
  (setq js-indent-level 2))

(use-package web-mode
  :mode "\\.\\([jt]sx\\)\\'")

(use-package typescript-mode
  :defer)

(use-package json-mode :ensure t)
