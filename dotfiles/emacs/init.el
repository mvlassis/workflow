;;; init.el -*- lexical-binding: t; -*-

;; Modules live in lisp/ next to the real (symlink-resolved) init.el
(add-to-list 'load-path
             (expand-file-name "lisp" (file-name-directory
                                       (file-truename (or load-file-name buffer-file-name)))))

;; Keep Custom's auto-generated settings out of the tracked config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; Package management
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

(require 'init-base)     ; defaults, appearance, editing, keybindings
(require 'init-dev)      ; completion, eglot, programming languages
(require 'init-writing)  ; org, spell checking, markdown, LaTeX
