;; Setting up theme and function to toggle
(use-package doom-modeline :ensure t :init (doom-modeline-mode 1))
(use-package nerd-icons)

(use-package base16-theme
  :defer)

(use-package faff-theme
  :defer)

(setq my-dark-theme 'base16-nord
      my-light-theme 'base16-mexico-light)

(defun my-select-theme (theme)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (cond
               ((eq theme 'dark) my-dark-theme)
               ((eq theme 'light) my-light-theme)
               (t theme))))

(defun my-select-theme-if-none-selected (frame)
  (if (and (eq 'x (window-system frame))
           (null (seq-filter (lambda (theme)
                               (not (string-prefix-p "smart-mode-line-"
                                                     (symbol-name theme))))
                             custom-enabled-themes)))
      (my-select-theme 'dark)))

(my-select-theme-if-none-selected nil)
(add-to-list 'after-make-frame-functions #'my-select-theme-if-none-selected)

(defun my-toggle-theme ()
  "Toggle between dark and light themes."
  (interactive)
  (my-select-theme (if (custom-theme-enabled-p my-dark-theme)
                       my-light-theme
                     my-dark-theme)))
