(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t)
(setq vc-follow-symlinks t)

(menu-bar-mode -1)

(global-set-key (kbd "C-o") (lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "C-l") 'switch-to-buffer)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Can I get this to work in tty?
;; (global-set-key (kbd "C-,") 'execute-extended-command)

;; Packages

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; 
;; (require 'use-package)
;; 
;; (setq use-package-always-ensure t)
