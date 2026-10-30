;;; early-init.el -*- lexical-binding: t; -*-
;; Loaded before the package system and the first frame are initialized.

;; Increase threshold of garbage collections to improve startup time...
(setq gc-cons-threshold (* 50 1000 1000))
;; ...and restore a sane value once startup is done
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1000 1000))))

;; Disable the menu bar, tool bar and scroll bars before the first frame
;; is drawn (faster than calling the modes, and no flicker)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(add-to-list 'default-frame-alist '(width . 90))
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
