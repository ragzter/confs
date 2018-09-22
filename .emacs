
;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(defun install-packages ()
  (interactive)
  (if (not (package-installed-p 'multiple-cursors))
      (package-install 'multiple-cursors))
  (if (not (package-installed-p 'centered-cursor-mode))
      (package-install 'centered-cursor-mode))
  (if (not (package-installed-p 'haskell-mode))
      (package-install 'haskell-mode))
  (if (not (package-installed-p 'rjsx-mode))
      (package-install 'rjsx-mode))
  (if (not (package-installed-p 'highlight-indent-guides))
      (package-install 'highlight-indent-guides)))

(install-packages)

(setq inhibit-startup-screen t)
(setq tab-width 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq show-paren-delay 0)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(if (package-installed-p 'rjsx-mode)
    (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

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

(if (package-installed-p 'centered-cursor-mode)
    (add-hook 'prog-mode-hook 'centered-cursor-mode))

(if (package-installed-p 'highlight-indent-guides-mode)
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(add-hook 'prog-mode-hook
          (lambda ()
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '("&&" . ?∧) prettify-symbols-alist)
            (push '("||" . ?∨) prettify-symbols-alist)
            (push '("<-" . ?←) prettify-symbols-alist)
            (push '("->" . ?→) prettify-symbols-alist)
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
 '(js2-ignored-warnings (quote ("")))
 '(js2-strict-missing-semi-warning nil)
 '(package-selected-packages
   (quote
    (multiple-cursors centered-cursor-mode haskell-mode highlight-indent-guides))))

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
