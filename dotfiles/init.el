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
(define-coding-system-alias 'UTF-8 'utf-8)
;; Make the alias a bit shorter
(defalias 'yes-or-no-p 'y-or-n-p)
;; Raise the frame when emacsclient is called
(add-hook 'server-switch-hook #'raise-frame)
;; Make resizing smoother
(setq frame-resize-pixelwise t)
(setq x-select-enable-clipboard t)
(add-to-list 'exec-path "/usr/bin/vendor_perl/") ; Add this to the emacs path for biber
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Add recentf mode and a shortcut to open recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key  (kbd "C-x C-r") 'recentf-open-files)

;; Customization
;; Set custom font and font size if it exists in the system
(when (member "Fira Mono:style=Regular" (font-family-list))
  (set-face-attribute 'default nil :font "Fira Mono:style=Regular" :height 130))

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
(global-display-line-numbers-mode 1) ; Enable line numbers
; Dont shift the line numbers mode when moving after line 100 (or 1000)
(setq display-line-numbers-width-start t)
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
(yas-reload-all) 
(add-hook 'prog-mode-hook #'yas-minor-mode) ; For function templates
;; (recentf-mode 1)
;; (save-place-mode 1)


;; Global shortcuts
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
(global-set-key (kbd "M-k") 'previous-buffer)
(global-set-key (kbd "M-l") 'next-buffer)
(global-set-key (kbd "C-M-,") 'beginning-of-buffer) ; Move to the beginning of the buffer
(global-set-key (kbd "C-M-.") 'end-of-buffer) ; Move to the end of the buffer
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
(load-theme 'gruvbox-dark-hard)

(use-package doom-modeline
  :config
  (setq doom-modeline-icon nil)
  ;; (setq doom-modeline-height 26)  
  (setq doom-modeline-buffer-name t)
  ;; (setq doom-modeline-highlight-modified-buffer-name t)
  (setq doom-modeline-time-icon t)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-battery nil)
  (setq doom-modeline-buffer-modification-icon nil)
  :ensure t
  :init (doom-modeline-mode 1))

(use-package all-the-icons)
(use-package helm-core)
(use-package helm
  :bind
  ((:map helm-map 
                ([tab] . helm-execute-if-single-persistent-action)
                ))
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x r b") 'helm-bookmarks)
  )

(defun double-flash-mode-line ()
 "Flash the modeline"
 (let ((flash-sec (/ 1.0 20)))
   (invert-face 'mode-line)
   (run-with-timer flash-sec nil #'invert-face 'mode-line)
   (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
   (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))

(defun helm-execute-if-single-persistent-action (&optional attr split-onewindow)
 "Execute persistent action if the candidate list is less than 2"
 (interactive)
 (with-helm-alive-p
   (if (> (helm-get-candidate-number) 2)
	   ()
       ;; (double-flash-mode-line)
     (helm-execute-persistent-action)
     )))

(use-package markdown-mode)
(use-package rust-mode)
(use-package math-preview)
(use-package flycheck)
(use-package lsp-mode
  :config
  (define-key lsp-mode-map (kbd "M-l") lsp-command-map)
  (setq lsp-signature-render-documentation nil))
(use-package lsp-ui)
;; (use-package lsp-java
;;   :config
;;   (add-hook 'java-mode-hook 'lsp))
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
						 (setq tab-width 4)
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

(use-package auto-package-update)
(use-package which-key
  :config
  (which-key-mode)
)
;; Simple mode to show total matches when searching
(use-package anzu 
  :config
  (global-anzu-mode 1)
)

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
  (setq company-minimum-prefix-length 5
		company-idle-delay 1) ;; Delay to display suggestions

)
(use-package yaml-mode
  :config
     (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
	 )
(use-package xclip
  :config
  (xclip-mode 1) ; Enables easy copy/pasting in the terminal
)

;; Shows colors of hex codes
(use-package rainbow-mode
  :config
  (define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
	(lambda () (rainbow-mode 1)))
  (my-global-rainbow-mode 1))
  ;; (add-hook 'find-file-hook (lambda () rainbow-mode 1)))

;; Mode to make all shortcuts work on greek layout
(use-package reverse-im
  :ensure t
  :custom
  (reverse-im-input-methods '("greek"))
  :config
  (reverse-im-mode t))

(use-package esup)
(use-package restart-emacs)

;; (use-package dap-mode)

;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

(use-package rustic
  :ensure t
  :config
  (require 'lsp-rust)
  (setq lsp-rust-analyzer-completion-add-call-parenthesis nil))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package tex-mode
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq-default TeX-PDF-mode t)
  (setq-default TeX-engine 'luatex)
  (setq TeX-view-program-selection
		'((output-pdf "Okular")))
  (setq bibtex-dialect 'biblatex)
  )

