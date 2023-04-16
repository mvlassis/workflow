;; init.el

;; Use "custom.el" to save internal configuration done by Emacs
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file)

;; Important configuration
;; Copy when right-clicking an active region
(defadvice mouse-save-then-kill (around mouse2-copy-region activate)
  (when (region-active-p)
    (copy-region-as-kill (region-beginning) (region-end)))
  ad-do-it)
;; Make the mouse a bit better on the terminal
(add-hook 'tty-setup-hook (lambda () (xterm-mouse-mode)))
;; Make the alias a bit shorter
(defalias 'yes-or-no-p 'y-or-n-p)
;; Raise the frame when emacsclient is called
(add-hook 'server-switch-hook #'raise-frame)
;; Make resizing smoother
(setq frame-resize-pixelwise t)
(setq x-select-enable-clipboard t)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Customization
;; Custom font and font size
(set-face-attribute 'default nil :font "Fira Mono" :height 120)

(setq inhibit-startup-message t) ; Disable the startup screen when opening Emacs
(setq column-number-mode t) ; Display the current column of the cursor
(setq kill-whole-line t) ; Also delete the '\n' characters when deleting lines
(setq Buffer-menu-name-width 60) ; Increase the buffer menu name width
(setq backup-directory-alist `(("." . "~/.saves")))
(setq c-default-style "linux"
      c-basic-offset 4)
(setq-default tab-width 4)
;; Recognize .cu files as .c files
(add-to-list 'auto-mode-alist '("\\.cu$" . c-mode))
;; Regard those files as shell scripts
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("zshrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("config\\'" . sh-mode))
;; Stop an annoying message in python
(setq python-indent-guess-indent-offset-verbose nil)
; Don't prompt me when opening symlinks under version control
(setq vc-follow-symlinks t) 
(add-to-list 'default-frame-alist '(width  . 90))

; Org Mode
(add-hook 'org-mode-hook 'toggle-truncate-lines)
(setq org-display-custom-times t)
(setq org-time-stamp-custom-formats 
      '("<%d %b %A>" . "<%d %b %H:%M>"))

;; Minor modes
(global-display-line-numbers-mode 1) ; Enable line numbers
;; Disable the default minor modes for beginners
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
;; (recentf-mode 1)
;; (save-place-mode 1)


;; Global shortcuts
;; Simple functions that replace the delete-word and backward-delete-word
;; but simply do not copy to the clipboard
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-unset-key (kbd "C-S-v"))
(global-set-key (kbd "C-M-,") 'beginning-of-buffer) ; Move to the beginning of the buffer
(global-set-key (kbd "C-M-.") 'end-of-buffer) ; Move to the end of the buffer
(global-set-key (kbd "<C-return>") (lambda ()
		  (interactive)
                  (end-of-line)
                  (newline-and-indent)))
(global-set-key (kbd "M-d") 'delete-word)
(global-set-key (kbd "M-DEL") 'backward-delete-word)
(global-set-key (kbd "<C-backspace>") 'backward-delete-word)
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))

;; Shortcuts for Greek
(global-set-key (kbd "C-α") 'beginning-of-line)
(global-set-key (kbd "C-ε") 'end-of-line)
(global-set-key (kbd "M-φ") 'forward-word)
(global-set-key (kbd "M-β") 'backward-word)
(global-set-key (kbd "M-δ") 'delete-word)
(global-set-key (kbd "C-φ") 'forward-char)
(global-set-key (kbd "C-β") 'backward-char)
(global-set-key (kbd "C-ν") 'next-line)
(global-set-key (kbd "C-π") 'previous-line)
(global-set-key (kbd "C-κ") 'kill-line)
(global-set-key (kbd "M-ν") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-π") (lambda () (interactive) (previous-line 5)))


;; Use 'package for package management
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package quelpa)
;; (quelpa '(eat :fetcher git
;;               :url "https://codeberg.org/akib/emacs-eat.git"
;;               :files ("*.el" "dir"
;;                       "*.info" "*.texi"
;;                       "*.ti" ("e" "e/*")))):

;; Packages to load and configurations
(use-package arduino-mode)
(use-package ini-mode ;; mode for .ini files
  :config
  (add-to-list 'auto-mode-alist '("\\.ini\\'" . ini-mode)))
(use-package vterm
  :bind (:map vterm-mode-map ("C-y" . vterm-yank))
  :config
  (setq vterm-max-scrollback 10000)
  :hook(vterm-mode . (lambda () (display-line-numbers-mode 0))))     
(use-package ein) ; Jupyter notebook in Emacs

;; Themes
(use-package zenburn-theme)
(use-package dracula-theme)
(use-package kaolin-themes)
(use-package monokai-theme)
(use-package material-theme)
(use-package gruvbox-theme)
;; Theme to load
(load-theme 'gruvbox-dark-hard )

(use-package doom-modeline
  :config
  (setq doom-modeline-height 25)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-name t)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-battery t)
  (setq doom-modeline-buffer-modification-icon nil)
  :ensure t
  :init (doom-modeline-mode 1))

(use-package all-the-icons)

(use-package markdown-mode)
(use-package math-preview)
(use-package lsp-mode
  :config
  (setq lsp-signature-render-documentation nil))
(use-package lsp-ui)
;; (use-package lsp-java
;;   :config
;;   (add-hook 'java-mode-hook 'lsp))
(use-package lsp-pyright
  :config
  (setq lsp-pyright-use-library-code-for-types nil)
  :ensure t
  :hook (python-mode . (lambda ()
			 (setq tab-width 4)
			 (setq python-indent-offset 4)
			 (require 'lsp-pyright)
			 (lsp))))
(use-package auto-package-update)
(use-package which-key
  :config
  (which-key-mode)
  )
(use-package anzu ;; Simple mode to show total matches when searching
  :config
  (global-anzu-mode 1)
)
(use-package flycheck)
(use-package flyspell
  :config
  (setq ispell-dictionary "en_US,el_GR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,el_GR")
)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(use-package company
  :config
  (global-company-mode)
)
(use-package yaml-mode
  :config
     (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
	 )
(use-package xclip)

;; (use-package keycast
;;   :config
;;   (keycast-mode)
;; )
