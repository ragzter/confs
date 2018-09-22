
;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(defun install-packages ()
  (interactive)
  (package-install "multiple-cursors")
  (package-install "centered-cursor-mode")
  (package-install "haskell-mode")
  (package-install "highlight-indent-guides"))

(setq inhibit-startup-screen t)
(setq tab-width 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq show-paren-delay 0)

(setq-default indent-tabs-mode nil)

(defun smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(electric-pair-mode 1)
(show-paren-mode 1)
(global-prettify-symbols-mode)
(iswitchb-mode)
(ido-mode)

(if (boundp 'centered-cursor-mode) (centered-cursor-mode))

(if (boundp 'highlight-indent-guides-mode)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

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
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-z") 'hippie-expand)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)

(defun eval-haskell-code ()
  (interactive)
  (fset 'eval-haskell-code
        "\C-xo:load TTT\C-m\C-xo"))

(global-set-key (kbd "C-c C-n") 'eval-haskell-code)

(global-set-key (kbd "C-c C-l") 'comment-region)
(global-set-key (kbd "C-c C-/") 'uncomment-region)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(set-default-font "Monospace-9")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(package-selected-packages
   (quote
    ("multiple-cursors" "saoeuhraloehculrahoelrc" multiple-cursors centered-cursor-mode haskell-mode highlight-indent-guides))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-hl-line-mode)

(set-face-background hl-line-face "#202020")
(set-face-attribute hl-line-face nil :underline nil)
(set-face-foreground 'highlight nil)
