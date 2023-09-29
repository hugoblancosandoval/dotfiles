(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Keep this file as clean as possible and move custom variables to its own file.
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Disable welcome screen
(setq inhibit-startup-screen t)

;; Ask for emacs to confirm exit
(setq confirm-kill-emacs 'y-or-n-p)

;; Always follow version controlled links
(setq vc-follow-symlinks t)

;; Anything that writes to the buffer while the region is active will overwrite it, including paste,
(delete-selection-mode 1)

;; disable ctrl-x ctrl-z that send emacs to the background
(global-unset-key (kbd "C-z"))

;; yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Mapping control+cursor to change window pane size
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; Making ctr-b ctrl-k the same as ctrl-b k
(global-set-key (kbd "s-k") 'kill-buffer)

;; Making S-b the same as ctrl-x
(global-set-key (kbd "s-b") 'switch-to-buffer)

;; Remove all other windows
(global-set-key (kbd "s-1") 'delete-other-windows)

;; Disable ctrl-x, ctrl-b
(global-unset-key [(control x)(control b)])

;; Disable ctrl-x ctrl-z that minimizes emacs
(global-unset-key [(control x)(control z)])

;; Disable ctrl-x m that opens the email composition
(global-unset-key (kbd "C-x m"))

;; Ctrl-h map to delete-backward
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Disable ctrl-x m that opens the email composition
(global-unset-key (kbd "C-x m"))

;; Super-r to revert buffer
(global-set-key (kbd "s-r") 'revert-buffer)

;; disable the lockfiles
(setq create-lockfile nil)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; Setting font
(when (member "JetBrains Mono" (font-family-list))
  (set-frame-font "JetBrains Mono 16" nil t))

;; Set font-size
(set-face-attribute 'default nil :height 160)
;; Disable sound bell
(setq visible-bell t)

;; Disable super-x m that minimizes emacs
(global-unset-key (kbd "s-m"))

;; Disable super-h that hides emacs
(global-unset-key (kbd "s-h"))

;; Disable super-n that creates a new frame
(global-unset-key (kbd "s-n"))

;; Browse the kill ring in a popup menu
(global-set-key (kbd "C-c C-y") '(lambda ()
                                 (interactive)
                                 (popup-menu 'yank-menu)))

;; Kill the minibuffer on blur
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; set column-mode on
(setq column-number-mode t)

;; active wind-move!!
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(use-package rainbow-delimiters
  :ensure t
  :config '(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-symbol :ensure t)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Built-in project package
(require 'project)
(global-set-key (kbd "C-x p f") #'project-find-file)

;; General settings
(delete-selection-mode t)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(global-display-line-numbers-mode)


(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)
  
;; Loads all your shell configs, makes it easier to load other binaries that emacs might use
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Displays the key bindings following your currently entered incomplete command (a prefix) in a popup.
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Pretty self explanatory
(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))

;; json-mode
(use-package json-mode
  :ensure t)

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package typescript-mode
  :ensure t
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))

  (setq typescript-indent-level 2))

;; Ensure Eglot is launched on Typescript buffers
(use-package eglot :ensure t)
(add-hook 'typescript-mode-hook 'eglot-ensure)
(setq eglot-events-buffer-size 0)

;; company
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)
(use-package company
  :ensure t
  :config (global-company-mode t))
(add-hook 'after-init-hook 'global-company-mode)

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; never loose what you wrote on the scratch
(use-package everlasting-scratch :ensure t)

;; markdown mode
(use-package markdown-mode :ensure t :config (setq initial-major-mode 'markdown-mode))

;; theme
;; Other themes I liked: exotica, subatomic, noctilux
(setq custom-safe-themes t) ;; Treat all custom themes as safe
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(if (display-graphic-p) 
    (use-package weyland-yutani-theme
      :ensure t 
      :config (load-theme 'weyland-yutani t))
    (use-package tron-legacy-theme
      :ensure t
      :config (load-theme 'tron-legacy t)))

(use-package yaml-mode :ensure t)
(use-package yafolding :ensure t)
(use-package hackernews :ensure t)
(use-package dockerfile-mode :ensure t)

(setq web-mode-enable-auto-indentation nil)

(use-package projectile 
  :ensure t
  :config (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))
(projectile-mode 1)

;; Treemacs + treeacs-projectile
(use-package treemacs
  :ensure t
  :bind (:map global-map
	      ("<f12>" . treemacs)))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))

;; ORG-MODE and related configurations
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)"))))

;; Set org-mode export backend, I added the md option
(setq org-export-backends '(ascii html icalendar latex md odt))

;; Add backends for org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(setq org-src-preserve-indentation t) ;; This avoids reindentation on SRC blocks
(setq org-hide-emphasis-markers t) ;; Hide the formatting markers

;; TODO: How do I manage my TODOs in org-roam????
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/usr/notes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
  :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

;; Org-babel
(require 'ob-js)
(add-to-list 'org-babel-load-languages '(js . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))
(put 'upcase-region 'disabled nil)
