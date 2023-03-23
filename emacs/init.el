;; No startup  screen
(setq inhibit-startup-screen t)

;; No startup message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Use RET to open org-mode links, including those in quick-help.org
(setq org-return-follows-link t)

;; Anything that writes to the buffer while the region is active will overwrite it, including paste,
(delete-selection-mode 1)

;; disable ctrl-x ctrl-z that send emacs to the background
(global-unset-key (kbd "C-z"))

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

;; disable the lockfiles
(setq create-lockfile nil)

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

;; active wind-move!!
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Kill the minibuffer on blur
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

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

;; Make sure clipboard works properly in tty mode on OSX
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
(when (and (not (display-graphic-p))
           (eq system-type 'darwin))
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)
;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

(delete-selection-mode t) ;; Substitute what you selected with yanked
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Default shell in term
(unless
    (or (eq system-type 'windows-nt)
        (not (file-exists-p "/bin/zsh")))
  (setq-default shell-file-name "/bin/zsh")
  (setq explicit-shell-file-name "/bin/zsh"))

;; Kill term buffer when exiting
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; IDO
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Built-in project package
(require 'project)
(global-set-key (kbd "C-x p f") #'project-find-file)

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
(require 'nano-theme-light)
(require 'nano-theme-dark)

(nano-theme-set-dark)
(nano-refresh-theme)

;; Start configuring the packages
(use-package persistent-scratch 
  :straight t
  :config '(persistent-scratch-setup-default))

(persistent-scratch-setup-default)

(use-package rainbow-delimiters
  :straight t
  :config '(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package highlight-symbol :straight t)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

;; Loads all your shell configs, makes it easier to load other binaries that emacs might use
(use-package exec-path-from-shell
  :straight t
  :config (exec-path-from-shell-initialize))

;; Displays the key bindings following your currently entered incomplete command (a prefix) in a popup.
(use-package which-key
  :straight t
  :config (which-key-mode))

;; Pretty self explanatory
(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))

;; json-mode
(use-package json-mode :straight t)

;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
  :straight t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode)

;; company
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)
(use-package company :straight t
  :config (global-company-mode t))

;; magit
(use-package magit :straight t
  :bind (("C-x g" . magit-status)))

;; never loose what you wrote on the scratch
(use-package everlasting-scratch :straight t)
(add-hook 'after-init-hook 'everlasting-scratch-mode)

;; markdown mode
(use-package markdown-mode :straight t :config (setq initial-major-mode 'markdown-mode))

;; elpy
(use-package elpy
  :straight t
  :config (elpy-enable))

;; Enable Flycheckn for pythin
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(use-package py-autopep8
  :straight t
  :config #'(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(setq lsp-log-io nil) ;; Don't log everything = speed
(setq lsp-keymap-prefix "C-c l")
(setq lsp-restart 'auto-restart)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-code-actions t)

(use-package lsp-mode
  :straight t
  :hook ((web-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))

(use-package prettier-js :straight t)
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
                              '("\\.tsx?\\'" . prettier-js-mode))))

;; Clojure mode and CIDER
(use-package clojure-mode :straight t)
(use-package cider :straight t)

(use-package yaml-mode :straight t)
;; orgmode agenda
(global-set-key (kbd "C-c a") 'org-agenda) ; set key for agenda

;; File to save todo items
(setq org-agenda-files (quote ("/Users/hugo/.emacs.d/todo.org")))

;; Set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;; Set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#8c1eff" :weight bold))
                           (?B . (:foreground "#f222ff"))
                           (?C . (:foreground "#ff2975"))))

;; Open agenda in current window
(setq org-agenda-window-setup (quote current-window))

;; Capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "/Users/hugo/.emacs.d/todo.org" "Tasks")
         "* TODO [#A] %?")))

