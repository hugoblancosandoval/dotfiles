;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
(use-package project
  :ensure nil
  :demand t
  :bind (("M-s M-s" . project-find-file)
         :map project-prefix-map
         ("m" . project-magit)
         ("d" . project-dired))
  :init
  (setopt project-switch-commands
          '((project-find-file "Find file" ?f)
            (project-dired "Dired" ?d)
            (project-vc-dir "VC-Dir" ?v)
            (project-eshell "Eshell" ?e)
            (project-shell "Shell" ?s)))
  :config
  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;; 1. project.el (project-roots)
  (setq consult-project-function
        (lambda (may-prompt)
          (when-let* ((project (project-current))
                      (project-root (car (project-roots (project-current))))
                      (is-not-home
                       (not (string= "/home/gavinok/" (car (project-roots
                                                            (project-current)))))))
            project-root)))

  ;; Added in emacs 29
  (setopt project-vc-extra-root-markers
          '("pyproject.toml" "requirements.txt" "spago.dhall" "CMakeList.txt"
            "package.clj" "package.json" "Project.toml" ".project" "Cargo.toml"
            "mix.exs" "qlfile" ".git")))

(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.5) ; Open after .5s instead of 1s
  :config
  (which-key-mode))

(use-package vterm)
(use-package vterm-toggle)
(use-package highlight-symbol :ensure t)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(use-package exec-path-from-shell :ensure t)
(exec-path-from-shell-initialize)

;; Saves the history in minibuffer, part of emacs
(use-package savehist :init (savehist-mode))

(use-package vertico
  :ensure t
  :init (vertico-mode)
  :custom (vertico-cycle t))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/usr/notes")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>"))))
  :config (org-roam-setup)
  :bind (
         ("C-c n t" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"   . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
)

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	       ("C--" . er/contract-region)))
