
;; This file is incomplete, but better than nothing

(setq inhibit-startup-screen t)

(electric-pair-mode 1)
(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq show-paren-delay 0)

(global-prettify-symbols-mode)

(iswitchb-mode)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)))

(add-hook 'js2-mode-hook
          (lambda ()
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)
            (push '("=>" . ?⇒) prettify-symbols-alist)
            (push '("!=" . ?≠) prettify-symbols-alist)))

(setq iswitchb-buffer-ignore '("^ " "*Completions*" "*Apropos*" "*Backtrace*" "*Help*" "*Buffer List*"))

(global-set-key (kbd "C-o") (lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "C-l") 'switch-to-buffer)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "C-z") 'hippie-expand)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)

