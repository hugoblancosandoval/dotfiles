(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Disable welcome screen
(setq inhibit-startup-screen t)

;; Ask for emacs to confirm exit
(setq confirm-kill-emacs 'y-or-n-p)

;; Anything that writes to the buffer while the region is active will overwrite it, including paste,
(delete-selection-mode 1)

;; disable ctrl-x ctrl-z that send emacs to the background
(global-unset-key (kbd "C-z"))

;; enable line number for all!
;;(global-linum-mode t)

;; yes/no prompts to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Ctrl-h map to delete-backward
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "RET") 'newline-and-indent)

;; Mapping control+cursor to change window pane size
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; Making ctr-b ctrl-k the same as ctrl-b k
(global-set-key (kbd "S-k") 'kill-buffer)

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

;; Mapping control+cursor to change window pane size
(global-set-key (kbd "<C-up>") 'shrink-window)
(global-set-key (kbd "<C-down>") 'enlarge-window)
(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;; Making ctr-b ctrl-k the same as ctrl-b k
(global-set-key (kbd "S-k") 'kill-buffer)

;; Disable ctrl-x, ctrl-b
(global-unset-key [(control x)(control b)])

;; Disable ctrl-x ctrl-z that minimizes emacs
(global-unset-key [(control x)(control z)])

;; Disable ctrl-x m that opens the email composition
(global-unset-key (kbd "C-x m"))

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

;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode)

;; company
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)
(use-package company
  :ensure t
  :config (global-company-mode t))

;; magit
(use-package magit
  :ensure t
  :bind (
	 ("C-x g" . magit-status)))

;; never loose what you wrote on the scratch
(use-package everlasting-scratch :ensure t)

;; markdown mode
(use-package markdown-mode :ensure t :config (setq initial-major-mode 'markdown-mode))

;; theme
;; Other themes I liked: exotica, subatomic, noctilux
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(if (display-graphic-p) 
    (use-package cherry-blossom-theme
      :ensure t 
      :config (load-theme 'cherry-blossom))
    (use-package tron-legacy-theme
      :ensure t
      :config (load-theme 'tron-legacy t)))

;; elpy
(use-package elpy
  :ensure t
  :config (elpy-enable))

;; lsp-mode
(setq lsp-log-io nil) ;; Don't log everything = speed
(setq lsp-keymap-prefix "C-c l")
(setq lsp-restart 'auto-restart)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-code-actions t)

(use-package lsp-mode
  :ensure t
  :hook (
	 (web-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands lsp-deferred)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))

(use-package prettier-js
  :ensure t)
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
                              '("\\.tsx?\\'" . prettier-js-mode))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9ee253fcdb48535bf16df2700582b0a11fe99390b018755b941140f2fcdff219" default))
 '(package-selected-packages
   '(highlight-symbol rainbow-delimiters everlasting-scratch elpy prettier-js lsp-ui lsp-mode tron-legacy-theme magit company web-mode json-mode expand-region which-key exec-path-from-shell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
