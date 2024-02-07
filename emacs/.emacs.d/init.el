(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
	"straight/repos/straight.el/bootstrap.el"
	(or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  (tool-bar-mode -1)             ; Hide the outdated icons
  (scroll-bar-mode -1)           ; Hide the always-visible scrollbar
  (setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
  (setq use-file-dialog nil)      ; Ask for textual confirmation instead of GUI
  
  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message ""))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (set-face-attribute 'default nil
		      :font "Fira Code"
		      :height 160)
  (defun ab/enable-line-numbers ()
    "Enable relative line numbers"
    (interactive)
    (display-line-numbers-mode)
    (setq display-line-numbers 'relative))
  (add-hook 'prog-mode-hook #'ab/enable-line-numbers)

  (setq vc-follow-symlinks t) ;; Always follow version controlled links

  (with-eval-after-load 'package
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

  ;; If you want to turn off the welcome screen, uncomment this
  (setq inhibit-splash-screen t)

  (setq initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
  (setq display-time-default-load-average nil) ; this information is useless for most

  ;; Automatically reread from disk if the underlying file changes
  (setq auto-revert-interval 1)
  (setq auto-revert-check-vc-info t)
  (global-auto-revert-mode)

  ;; Move through windows with Ctrl-<arrow keys>
  (windmove-default-keybindings 'control) ; You can use other modifiers here
  (split-window-horizontally)

  ;; Fix archaic defaults
  (setq sentence-end-double-space nil)

  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))

  (setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
  (setq completion-cycle-threshold 1)                  ; TAB cycles candidates
  (setq completions-detailed t)                        ; Show annotations
  (setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
  (setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates

  (setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
  (setq completions-max-height 20)                     ; This is arbitrary
  (setq completions-detailed t)
  (setq completions-format 'one-column)
  (setq completions-group t)
  (setq completion-auto-select 'second-tab)            ; Much more eager
					;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

  ;; Mode line information
  (setq line-number-mode t)                        ; Show current line in modeline
  (setq column-number-mode t)                      ; Show column as well

  (setq x-underline-at-descent-line nil)           ; Prettier underlines
  (setq switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

  (setq-default show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
  (setq-default indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

  ;; Enable horizontal scrolling
  (setq mouse-wheel-tilt-scroll t)
  (setq mouse-wheel-flip-direction t)

  ;; We won't set these, but they're good to know about
  ;;
  ;; (setq-default indent-tabs-mode nil)
  ;; (setq-default tab-width 4)

  ;; Misc. UI tweaks
  (blink-cursor-mode -1)                                ; Steady cursor
  (pixel-scroll-precision-mode)                         ; Smooth scrolling

  ;; Use common keystrokes by default
  ;;(cua-mode) ;; Allows Ctrl-c/v/x for copy/paste/cut

  ;; Display line numbers in programming mode
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)           ; Set a minimum width

  ;; Nice line wrapping when working with text
  (add-hook 'text-mode-hook 'visual-line-mode)

  ;; Modes to highlight the current line with
  (let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
    (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

  ;; Show the tab-bar as soon as tab-bar functions are invoked
  (setq tab-bar-show 0)

  ;; Add the time to the tab-bar, if visible
  (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
  (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
  (setq display-time-format "%a %F %T")
  (setq display-time-interval 1)
  (display-time-mode)
  (put 'upcase-region 'disabled nil)
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
  (setq create-lockfiles nil) ;; Fuck it, no more **annoying** lockfiles

  (delete-selection-mode t) ;; Anything that writes to the buffer while the region is active will overwrite it, including paste,
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
                                     (popup-menu 'yank-menu))))


(load-file (expand-file-name "packages.el" user-emacs-directory))

