;; initialize quelpa
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Bootstrapping straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)

;; General settings
(setq custom-file (concat user-emacs-directory "/custom.el")) ;; Keep init.el as clean as possible and move custom variables to its own file.
(setq custom-safe-themes t) ;; Don't if themes are safe
(setq inhibit-startup-screen t) ;; Disable welcome screen
(setq confirm-kill-emacs 'y-or-n-p) ;; Ask for emacs to confirm exit
(setq vc-follow-symlinks t) ;; Always follow version controlled links
(setq visible-bell t) ;; Disable sound bell
(setq create-lockfile nil) ;; disable the lockfiles
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil) ;; ALWAYS use spaces for tabs

(delete-selection-mode t) ;; Anything that writes to the buffer while the region is active will overwrite it, including paste,
(fset 'yes-or-no-p 'y-or-n-p) ;; yes/no prompts to y/n
(set-language-environment "UTF-8") ;; UTF-8 as default encoding

;; Setting font
(when (member "JetBrains Mono" (font-family-list))
  (set-frame-font "JetBrains Mono 16" nil t))
(set-face-attribute 'default nil :height 160) ;; Set font-size

;; Ctrl-h map to delete-backward
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

(global-set-key (kbd "s-1") 'delete-other-windows) ;; Remove all other windows
(global-set-key (kbd "s-b") 'switch-to-buffer) ;; Making S-b the same as ctrl-x
(global-set-key (kbd "s-k") 'kill-buffer) ;; Making ctr-b ctrl-k the same as ctrl-b k
(global-set-key (kbd "s-r") 'revert-buffer);; Super-r to revert buffer

(global-unset-key [(control x)(control b)]) ;; Disable ctrl-x, ctrl-b
(global-unset-key [(control x)(control z)]) ;; Disable ctrl-x ctrl-z that minimizes emacs
(global-unset-key (kbd "C-x m")) ;; Disable ctrl-x m that opens the email composition
(global-unset-key (kbd "C-z")) ;; disable ctrl-x ctrl-z that send emacs to the background
(global-unset-key (kbd "s-m")) ;; Disable super-x m that minimizes emacs
(global-unset-key (kbd "s-h")) ;; Disable super-h that hides emacs
(global-unset-key (kbd "s-n")) ;; Disable super-n that creates a new frame

(global-set-key (kbd "C-c C-y") '(lambda () ;; Browse the kill ring in a popup menu
                                 (interactive)
                                 (popup-menu 'yank-menu)))

;; Kill the minibuffer on blur
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; active wind-move!!
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(use-package highlight-symbol :ensure t)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; TODO: Enable project?
;; TODO: Enable IDO?
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

;; Built-in project package
;; (require 'project)
;; (global-set-key (kbd "C-x p f") #'project-find-file)

;; company
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)

(use-package company
  :ensure t
  :config (global-company-mode t))
(add-hook 'after-init-hook 'global-company-mode)

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package typescript-mode
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
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; https://github.com/orzechowskid/tsi.el/
;; great tree-sitter-based indentation for typescript/tsx, css, json
(use-package tsi
  :after tree-sitter
  :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(use-package eglot :ensure t) ;; To get "intellisense" from tree-sitter. You also need to have prettier and typescript-language-server for tsx


;; Install use-package
(straight-use-package 'use-package)

(straight-use-package
  '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

;; Default layout (optional)
(require 'nano-layout)

;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))

;; Theme
(require 'nano-faces)
(require 'nano-theme)
(require 'nano-modeline)
(require 'nano-bindings)

(use-package inkpot-theme :straight t)
(load-theme 'inkpot)

;; Mouse active in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; No scroll bars
(if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

;; No toolbar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; No menu bar
(if (display-graphic-p)
    (menu-bar-mode t) ;; When nil, focus problem on OSX
  (menu-bar-mode -1))

;; Mac specific
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
        mac-use-title-bar nil))
