;; init.el

;; Use "custom.el" to save internal configuration done by Emacs
;; (setq custom-file (concat user-emacs-directory "custom.el"))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

;; Increase threshold of garbage collectiosn to improve startup time
(setq gc-cons-threshold (* 50 1000 1000))

;; Theme to load
(if (locate-file "kaolin-themes.el" custom-theme-load-path)
    (load-theme 'kaolin-bubblegum t)
  (if (locate-file "modus-theme.el" custom-theme-load-path)
      (load-theme 'modus-vivendi t)
    (load-theme 'tango-dark t)))

(when (find-font (font-spec :name "Fira Mono"))
  (set-face-attribute 'default nil :font "Fira Mono:style=Regular" :height 130))

;; Important configuration
;; Copy when right-clicking an active region
(defadvice mouse-save-then-kill (around mouse2-copy-region activate)
  (when (region-active-p)
    (copy-region-as-kill (region-beginning) (region-end)))
  ad-do-it)

;; Make the mouse work a bit better on the terminal
(add-hook 'tty-setup-hook (lambda () (xterm-mouse-mode)))

;; ;; Turn off the touchpad when Emacs is in 
;; (defun turn-off-mouse (&optional frame)
;;   (interactive)
;;   (shell-command "xinput --disable \"MSFT0001:00 06CB:CE2D Touchpad\""))

;; (defun turn-on-mouse (&optional frame)
;;   (interactive)
;;   (shell-command "xinput --enable \"MSFT0001:00 06CB:CE2D Touchpad\""))

;; (add-hook 'focus-in-hook #'turn-off-mouse)
;; (add-hook 'focus-out-hook #'turn-on-mouse)
;; (add-hook 'delete-frame-functions #'turn-on-mouse)

(define-coding-system-alias 'UTF-8 'utf-8)
;; Make the alias a bit shorter
(defalias 'yes-or-no-p 'y-or-n-p)
;; Raise the frame when emacsclient is called
(add-hook 'server-switch-hook #'raise-frame)
;; Make resizing smoother
(setq frame-resize-pixelwise t)
(setq x-select-enable-clipboard t)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable all comments on the scratchpad
(setq initial-scratch-message "")

;; Add recentf mode and a shortcut to open recent files
(defun my-recentf-cleanup-silently ()
  (let ((inhibit-message t))
    (recentf-cleanup)))

;; Replace the default recentf cleanup with the silent version
(setq recentf-auto-cleanup 'my-recentf-cleanup-silently)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key  (kbd "C-x C-r") 'recentf-open-files)
(savehist-mode 1)

;; Customization
;; Set custom font and font size if it exists in the system
(defun set-font-if-exists (frame)
  (select-frame frame)
  (when (find-font (font-spec :name "Fira Mono"))
    (set-face-attribute 'default nil :font "Fira Mono:style=Regular" :height 130)))

(add-hook 'after-make-frame-functions 'set-font-if-exists)

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
(require 'org-mouse)
(setq org-display-custom-times t)
(setq org-time-stamp-custom-formats 
      '("<%d %b %A>" . "<%d %b %H:%M>"))

;; Minor modes
(global-display-line-numbers-mode t) ; Enable line numbers
; Dont shift the line numbers mode when moving after line 100 (or 1000)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-grow-only t)

; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Disable the default minor modes for beginners
;; (menu-bar-mode -1)
;; (tool-bar-mode -1)
;; (scroll-bar-mode -1)
;; Alternative ways to disable the above modes (but faster startup)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(tooltip-mode -1)
(electric-pair-mode)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)



;; Global shortcuts
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(defun shift-text (distance)
  (if (use-region-p)
      (let ((mark (mark)))
        (save-excursion
          (indent-rigidly (region-beginning)
                          (region-end)
                          distance)
          (push-mark mark t t)
          (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))

;; Simple functions that replace the delete-word and backward-delete-word
;; but simply do not copy to the clipboard

(defun delete-line (&optional arg)
  (interactive "P")
  ;; taken from kill-line
  (delete-region (point)
                 ;; It is better to move point to the other end of the kill
                 ;; before killing.  That way, in a read-only buffer, point
                 ;; moves across the text that is copied to the kill ring.
                 ;; The choice has no effect on undo now that undo records
                 ;; the value of point from before the command was run.
                 (progn
                   (if arg
                       (forward-visible-line (prefix-numeric-value arg))
                     (if (eobp)
                         (signal 'end-of-buffer nil))
                     (let ((end
                            (save-excursion
                              (end-of-visible-line) (point))))
                       (if (or (save-excursion
                                 ;; If trailing whitespace is visible,
                                 ;; don't treat it as nothing.
                                 (unless show-trailing-whitespace
                                   (skip-chars-forward " \t" end))
                                 (= (point) end))
                               (and kill-whole-line (bolp)))
                           (forward-visible-line 1)
                         (goto-char end))))
                   (point))))

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
(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-<") 'beginning-of-buffer) ; Move to the beginning of the buffer
(global-set-key (kbd "M->") 'end-of-buffer) ; Move to the end of the buffer
(global-set-key (kbd "<C-return>") (lambda ()
		  (interactive)
                  (end-of-line)
                  (newline-and-indent)))
(global-set-key (kbd "M-<f5>") (lambda ()
		  (interactive)
                  (end-of-line)
                  (newline-and-indent)))
(global-set-key (kbd "M-d") 'delete-word)
(global-set-key (kbd "M-DEL") 'backward-delete-word)
(global-set-key (kbd "C-k") 'delete-line)
(global-set-key (kbd "M-k") 'kill-line)
(global-set-key (kbd "<C-backspace>") 'backward-delete-word)
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))



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

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Packages to load and configurations
(use-package arduino-mode
  :mode ("\\.ino\\'" . arduino-mode)
)

(use-package ini-mode ;; mode for .ini files
  :mode ("\\.ini\\'" . ini-mode))

(use-package vterm
  :defer t
  :bind (:map vterm-mode-map ("C-y" . vterm-yank))
  :config
  (setq vterm-max-scrollback 10000)
  :hook(vterm-mode . (lambda () (display-line-numbers-mode 0))))

;; (use-package ein) ; Jupyter notebook in Emacs

;; (use-package evil
;;   :config
;;   (setq evil-default-state 'emacs)
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-C-u-scroll t)
;;   (evil-mode 1)
;; )

;; Themes
(use-package zenburn-theme
  :defer t
)
(use-package dracula-theme)
(use-package kaolin-themes
  :defer t
)
(use-package monokai-theme
  :defer t
)
(use-package material-theme
  :defer t
)
(use-package gruvbox-theme
  :defer t
)

(use-package doom-themes
  :defer t
)

(use-package doom-modeline
  :config
  (setq doom-modeline-height 30)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-env-version t)
  :init (doom-modeline-mode 1))

(use-package all-the-icons)

(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)
  :init
  (vertico-mode))

(use-package consult
  :after vertico
  :bind (
		 ("C-x b" . consult-buffer)
		 ("M-s l" . consult-line))
  :config
  (consult-customize consult-theme :preview-key '(:debounce 0.5 any)))
  

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package bind-key)
(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
)
(use-package rust-mode
  :defer t
)
(use-package math-preview
  :defer t
)
(use-package flycheck
  :defer t
)

;; LSP client
(use-package eglot
  :config
  (setq eglot-report-progress nil)
  (defun my-inhibit-eglot-messages (orig-fun &rest args)
    "Suppress all messages generated by `eglot-message`."
    (let ((inhibit-message t))  ;; Inhibit all messages within this scope
      (apply orig-fun args)))   ;; Call the original eglot-message function

  (advice-add 'eglot--message :around #'my-inhibit-eglot-messages)
  :hook
  (python-mode . eglot-ensure))

(use-package pyvenv-auto
  :hook ((python-mode . pyvenv-auto-run)))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  :config
  (auto-package-update-maybe))
(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5)
  )

;; Simple mode to show total matches when searching
(use-package anzu 
  :config
  (global-anzu-mode 1)
)

(use-package flyspell
  :defer t
  :config
  (setq ispell-quietly 1)
  (setq ispell-program-name "/usr/bin/hunspell")
  (setq ispell-dictionary "en_US,el_GR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,el_GR")
)
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(yaml-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(use-package company
  :config
  (global-company-mode)
  (setq company-minimum-prefix-length 5
		company-idle-delay 1) ;; Delay to display suggestions
  )

(use-package yaml
  :defer t
)
(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
)

(use-package xclip
  :config
  (message "Value of XDG_SESSION_TYPE: %s" (getenv "XDG_SESSION_TYPE"))
  (if (string-equal (getenv "XDG_SESSION_TYPE") "wayland")
      ;; Configuration for Wayland
	  ;; See https://discourse.doomemacs.org/t/how-to-copy-and-paste-in-wayland/4566/8
      (progn
		(message "Setting xclip-program to wl-copy...")
        (setq xclip-program "wl-copy")
        (setq xclip-method 'wl-copy)))
  (setq xclip-select-enable-clipboard t)
  (setq xclip-mode 1))

;; (use-package xclip
;;   :config
;;   (let ((exit-code (call-process "wl-paste" nil nil nil)))
;;     (if (= exit-code 0)
;;         (progn
;;           (message "wl-paste detected, using wl-copy...")
;;           (setq xclip-program "wl-copy")
;;           (setq xclip-method 'wl-copy))
;;       (message "wl-paste not detected, skipping wl-copy configuration.")))
;;   (setq xclip-select-enable-clipboard t)
;;   (setq xclip-mode 1))

;; (use-package xclip
;;   :defer t
;;   :init
;;   (add-hook 'server-visit-hook
;;             (lambda ()
;;               (message "Value of XDG_SESSION_TYPE: %s" (getenv "XDG_SESSION_TYPE"))
;; 			  (if (string-equal (getenv "XDG_SESSION_TYPE") "wayland")
;;                   (progn
;;                     (message "wl-paste detected, using wl-copy...")
;;                     (setq xclip-program "wl-copy")
;;                     (setq xclip-method 'wl-copy))
;;                   (message "wl-paste not detected, skipping wl-copy configuration."))
;;               (setq xclip-select-enable-clipboard t)
;;               (xclip-mode 1))))

;; Shows colors of hex codes
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Mode to make all shortcuts work on greek layout
(use-package reverse-im
  :custom
  (reverse-im-input-methods '("greek"))
  :config
  (reverse-im-mode t))

;; (use-package esup)
(use-package restart-emacs
  :defer t
)

(use-package dap-mode
  :defer t
)

(use-package rustic
  :defer t
  :hook (rustic-mode . (lambda ()
                         (require 'lsp-rust)
                         (setq lsp-rust-analyzer-completion-add-call-parenthesis nil)))
)

(use-package rainbow-delimiters
  :config
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-engine 'luatex)
  (setq TeX-view-program-selection
		'((output-pdf "Okular")))
  :hook (tex-mode . LaTeX-mode))

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(use-package terraform-mode)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  ("<f1>" . treemacs)
)

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("788121c96b7a9b99a6f35e53b7c154991f4880bb0046a80330bb904c1a85e275" "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66" "30d174000ea9cbddecd6cc695943afb7dba66b302a14f9db5dd65074e70cc744" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "c7fd1708e08544d1df2cba59b85bd25263180b19b287489d4f17b9118487e718" "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09" "90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "78e6be576f4a526d212d5f9a8798e5706990216e9be10174e3f3b015b8662e27" default))
 '(package-selected-packages
   '(pyvenv-auto pyvenv exec-path-from-shell terraform-mode gnu-elpa-keyring-update consult zenburn-theme yaml-mode xclip which-key vterm vertico use-package sudo-edit snap-indent rustic reverse-im restart-emacs rainbow-mode rainbow-delimiters quelpa monokai-theme math-preview material-theme marginalia lsp-ui lsp-pyright keycast kaolin-themes ini-mode helm gruvbox-theme flycheck evil esup elpy ein eat dracula-theme doom-themes doom-modeline dap-mode cyberpunk-theme clipetty benchmark-init auto-package-update auctex arduino-mode anzu all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

