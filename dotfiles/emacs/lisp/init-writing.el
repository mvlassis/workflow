;;; init-writing.el --- Prose: org, spelling, markdown, LaTeX -*- lexical-binding: t; -*-

;;; Org
(add-hook 'org-mode-hook 'toggle-truncate-lines)
(require 'org-mouse)
(setq org-display-custom-times t)
(setq org-time-stamp-custom-formats
      '("<%d %b %A>" . "<%d %b %H:%M>"))

;;; Spell checking (English + Greek via hunspell)
(use-package flyspell
  :ensure nil
  :defer t
  :config
  (setq ispell-quietly 1)
  (setq ispell-program-name "/usr/bin/hunspell")
  (setq ispell-dictionary "en_US,el_GR")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,el_GR"))

;; On for text, off for HTML (tags get flagged), code and YAML
(add-hook 'text-mode-hook
          (lambda ()
            (unless (derived-mode-p 'mhtml-mode)
              (flyspell-mode 1))))
(dolist (hook '(mhtml-mode-hook prog-mode-hook yaml-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;;; Markdown
(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;;; LaTeX
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

(use-package math-preview :defer t)

(provide 'init-writing)
;;; init-writing.el ends here
