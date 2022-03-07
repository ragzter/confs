
;; Basics

(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t)
(setq vc-follow-symlinks t)
(setq make-backup-files nil)
(setq custom-file "~/.emacs-custom.el")
(setq show-paren-delay 0)

(menu-bar-mode -1)
(electric-pair-mode 1)
(show-paren-mode 1)

(global-set-key (kbd "C-o") (lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "C-l") 'switch-to-buffer)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-,") 'execute-extended-command) ; Can I get this to work in tty?

(load-theme 'wombat)

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; Packages

(use-package magit
  :bind (("C-x g" . 'magit-status)
	 ("C-c ?" . 'magit-log-buffer-file)
	 ("C-c f" . 'magit-find-file)))

(use-package selectrum
  :init
  (setq selectrum-max-window-height 10)
  (selectrum-mode +1))

(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode +1))

;; (find-file "~/.emacs")
;; (find-file "~/code/confs/old/.emacs")
