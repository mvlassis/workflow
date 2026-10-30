;;; init-base.el --- Core defaults, appearance and editing -*- lexical-binding: t; -*-

;;;; ========== General defaults ==========

(define-coding-system-alias 'UTF-8 'utf-8)
(setq use-short-answers t)                ; y/n instead of yes/no
(setq inhibit-startup-message t)          ; No startup screen
(setq initial-scratch-message "")         ; Empty scratch buffer
(setq frame-resize-pixelwise t)           ; Smoother resizing
(setq vc-follow-symlinks t)               ; Don't prompt for symlinks under VC
(setq backup-directory-alist `(("." . "~/.saves")))

;; Raise the frame when emacsclient is called
(add-hook 'server-switch-hook #'raise-frame)

;; Reload files changed on disk
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Recent files, with a silent cleanup
(defun my-recentf-cleanup-silently ()
  (let ((inhibit-message t))
    (recentf-cleanup)))
(setq recentf-auto-cleanup 'my-recentf-cleanup-silently)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(savehist-mode 1)

;; Add pipx binary directory to Emacs path
(let ((pipx-bin (expand-file-name "~/.local/bin")))
  (add-to-list 'exec-path pipx-bin)
  (setenv "PATH" (concat pipx-bin ":" (getenv "PATH"))))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  :config
  (auto-package-update-maybe))

;; (use-package esup)
(use-package restart-emacs
  :defer t)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;;;; ========== Appearance ==========

;;; Themes
(use-package zenburn-theme :defer t)
(use-package dracula-theme)
(use-package kaolin-themes :defer t)
(use-package monokai-theme :defer t)
(use-package material-theme :defer t)
(use-package gruvbox-theme :defer t)
(use-package doom-themes :defer t)

(cond ((memq 'kaolin-bubblegum (custom-available-themes))
       (load-theme 'kaolin-bubblegum t))
      ((memq 'modus-vivendi (custom-available-themes))
       (load-theme 'modus-vivendi t))
      (t (load-theme 'tango-dark t)))

;;; Font
;; Set custom font and font size if it exists in the system
(defun set-font-if-exists (&optional frame)
  (with-selected-frame (or frame (selected-frame))
    (when (find-font (font-spec :name "Fira Mono"))
      (set-face-attribute 'default nil :font "Fira Mono:style=Regular" :height 130))))

(set-font-if-exists)                                         ; initial frame
(add-hook 'after-make-frame-functions #'set-font-if-exists) ; emacsclient frames

;;; General UI
(tooltip-mode -1)
(setq column-number-mode t)        ; Display the current column of the cursor
(setq Buffer-menu-name-width 60)   ; Increase the buffer menu name width

;;; Line numbers
(global-display-line-numbers-mode t)
;; Don't shift the line numbers when moving past line 100 (or 1000)
(setq display-line-numbers-width-start t)
(setq display-line-numbers-grow-only t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;;; Modeline and helpers
(use-package doom-modeline
  :config
  (setq doom-modeline-height 30)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-env-version t)
  :init (doom-modeline-mode 1))

(use-package all-the-icons)

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

;; Show total matches when searching
(use-package anzu
  :config
  (global-anzu-mode 1))

;;;; ========== Editing behavior and keybindings ==========

(setq kill-whole-line t)            ; Also delete the '\n' when deleting lines
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(electric-pair-mode)

;;; Mouse
;; Copy when right-clicking an active region
(defun my-mouse-copy-region (orig-fun &rest args)
  (when (region-active-p)
    (copy-region-as-kill (region-beginning) (region-end)))
  (apply orig-fun args))
(advice-add 'mouse-save-then-kill :around #'my-mouse-copy-region)

;; Make the mouse work a bit better on the terminal
(add-hook 'tty-setup-hook (lambda () (xterm-mouse-mode)))

;; ;; Turn off the touchpad when Emacs is in focus
;; (defun turn-off-mouse (&optional frame)
;;   (interactive)
;;   (shell-command "xinput --disable \"MSFT0001:00 06CB:CE2D Touchpad\""))
;; (defun turn-on-mouse (&optional frame)
;;   (interactive)
;;   (shell-command "xinput --enable \"MSFT0001:00 06CB:CE2D Touchpad\""))
;; (add-hook 'focus-in-hook #'turn-off-mouse)
;; (add-hook 'focus-out-hook #'turn-on-mouse)
;; (add-hook 'delete-frame-functions #'turn-on-mouse)

;;; Commands
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

;; Replacements for kill-line, kill-word and backward-kill-word
;; that do not copy to the clipboard
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

(defun my-open-line-below ()
  "Insert a new indented line below the current one."
  (interactive)
  (end-of-line)
  (newline-and-indent))

;;; Global keybindings
(global-unset-key (kbd "C-S-v"))
(global-unset-key (kbd "M-l"))
(global-set-key (kbd "M-<") 'beginning-of-buffer)
(global-set-key (kbd "M->") 'end-of-buffer)
(global-set-key (kbd "<C-return>") #'my-open-line-below)
(global-set-key (kbd "M-<f5>") #'my-open-line-below)
(global-set-key (kbd "M-d") 'delete-word)
(global-set-key (kbd "M-DEL") 'backward-delete-word)
(global-set-key (kbd "C-k") 'delete-line)
(global-set-key (kbd "M-k") 'kill-line)
(global-set-key (kbd "<C-backspace>") 'backward-delete-word)
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
(define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)

(use-package bind-key)

;;; Clipboard
(setq select-enable-clipboard t)

(use-package xclip
  :config
  (when (string-equal (getenv "XDG_SESSION_TYPE") "wayland")
    ;; See https://discourse.doomemacs.org/t/how-to-copy-and-paste-in-wayland/4566/8
    (setq xclip-program "wl-copy")
    (setq xclip-method 'wl-copy))
  (setq xclip-select-enable-clipboard t)
  (xclip-mode 1))

;; Clipboard over SSH/tmux via OSC 52
(use-package clipetty
  :hook (after-init . global-clipetty-mode))

;;; Keyboard layouts and modal editing
;; Make all shortcuts work on the Greek layout
(use-package reverse-im
  :custom
  (reverse-im-input-methods '("greek"))
  :config
  (reverse-im-mode t))

(use-package evil)
;; (use-package evil
;;   :init
;;   (setq evil-want-integration t)
;;   (setq evil-want-C-u-scroll t)
;;   (evil-mode 1)
;;   :config
;;   (setq evil-default-state 'emacs))

(provide 'init-base)
;;; init-base.el ends here
