
;; Packages

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(setq package-check-signature nil)

(defun install-if-not-installed (package)
  (if (not (package-installed-p package))
      (package-install package)))

(defun install-packages ()
  (interactive)
  (mapc
   'install-if-not-installed
   '(centered-cursor-mode
     projectile
     haskell-mode
     company
     magit
     prettier-js
     tide
     web-mode)))

(defalias 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1)
(display-time-mode 1)

(setq org-startup-folded nil)

(install-packages)

(set-fringe-mode 0)

(setq inhibit-startup-screen t)
(setq tab-width 2)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(setq show-paren-delay 0)
(setq c-basic-offset 2)

(add-hook 'php-mode-hook #'(lambda() (setq c-basic-offset 2)))

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

(setq-default indent-tabs-mode nil)

(defun smart-beginning-of-line ()
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))

(global-set-key (kbd "C-a") 'smart-beginning-of-line)
(global-set-key (kbd "C-S-k") 'kill-whole-line)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(electric-pair-mode 1)
(show-paren-mode 1)
(ido-mode)

(defvar tide-electric-pairs '((?' . ?')) "Electric pairs for org-mode.")

(defun ts-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs tide-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'tide-mode-hook 'ts-add-electric-pairs)

(defun count-lines-buffer ()
  (save-excursion
    (let ((opoint (point)) beg end
	  total before after)
      (forward-page)
      (beginning-of-line)
      (or (looking-at page-delimiter)
	  (end-of-line))
      (setq end (point))
      (backward-page)
      (setq beg (point))
      (setq total (count-lines beg end))
      total)))

(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(if (package-installed-p 'centered-cursor-mode)
    (add-hook 'prog-mode-hook
              (lambda ()
                (interactive)
                (if (< (count-lines-buffer) 1000)
                    (centered-cursor-mode)))))

(add-hook 'org-mode 'centered-cursor-mode)

(global-set-key (kbd "C-o") (lambda () (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "C-l") 'switch-to-buffer)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-z") 'company-complete)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)

(global-set-key (kbd "C-c C-l") 'comment-region)
(global-set-key (kbd "C-c C-/") 'uncomment-region)

(defun font-exists-p (font)
  (if (null (x-list-fonts font)) nil t))

(if (window-system)
    (if (font-exists-p "Fantasque Sans Mono-12")
        (set-frame-font "Fantasque Sans Mono-12")
      (set-frame-font "Monospace-9")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (deeper-blue)))
 '(default-input-method "swedish-postfix")
 '(global-color-identifiers-mode t)
 '(js2-ignored-warnings (quote ("")))
 '(js2-strict-missing-semi-warning nil)
 '(package-selected-packages
   (quote
    (marginalia selectrum-prescient consult selectrum smartparens move-text beacon direx rainbow-delimiters avy which-key google-this forge ripgrep company-prescient prescient use-package company projectile helm rust-mode prettier-js web-mode tide company-lsp flycheck lsp-ui lsp-typescript typescript-mode php-mode rjsx-mode idris-mode multiple-cursors centered-cursor-mode haskell-mode highlight-indent-guides))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (global-hl-line-mode)

;; (set-face-background hl-line-face "#406060")
;;(set-face-attribute hl-line-face nil :underline nil)
;; (set-face-foreground 'highlight nil)
;; (set-face-foreground 'highlight t)

(set-cursor-color "orange")
(setq-default cursor-type 'hbar)

(define-skeleton console-skeleton
  "Console skeleton"
  nil
  "console.log(" _ ")"
  )

(global-set-key (kbd "C-c C-p") 'console-skeleton)

(global-display-line-numbers-mode)

(setq typescript-indent-level 2)

(add-to-list 'load-path "/home/ragnar/.nvm/versions/node/v10.9.0/bin/")
(add-to-list 'load-path "/home/ragnar/.yarn/bin/")

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (flycheck-add-next-checker 'javascript-eslint '(t . tsx-tide) 'append)
  (flycheck-select-checker 'javascript-eslint)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq-local company-backends
              '((company-tide company-files)))
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "js" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "ts" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
(add-hook 'web-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-l") 'comment-region)))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  )

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")
        ("jsx" . "\\.tsx?\\'")))

(setq web-mode-enable-auto-quoting nil)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-script-padding 2)
(setq web-mode-attr-indent-offset 2)
(setq web-mode-enable-css-colorization t)

(defun tide-format-before-save ())

(global-set-key (kbd "M-r") 'tide-references)

(require 'prettier-js)

(setq-default fill-column 80)
(add-hook 'org-mode-hook 'auto-fill-mode)

(add-hook 'tide-mode-hook 'prettier-js-mode)

(setq sh-basic-offset 2)

;; Projectile

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; this seems broken right now:
;; (setq projectile-max-file-buffer-count 10)

(setq company-idle-delay 0.2)
(setq company-async-timeout 5)

(setq web-mode-auto-close-style 2)

(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

(global-set-key (kbd "C-c ?") 'magit-log-buffer-file)
(global-set-key (kbd "C-c f") 'magit-find-file)
(global-set-key (kbd "C-c r") 'tide-rename-symbol)

(global-set-key (kbd "<f3>") (lambda () (interactive) (shell-command "free -h")))

(setq css-indent-offset 2)

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(setq haskell-process-args-ghci '("-ferror-spans" "-Wall"))

(setq-default display-line-numbers-width 3)

(setq desktop-save-mode 1)

; (desktop-read)

(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
;; (global-set-key (kbd "C-=") 'hs-show-all)
;; (global-set-key (kbd "C--") 'hs-toggle-hiding)

(global-set-key (kbd "C-c C-e") 'sql-send-paragraph)

(add-hook 'tide-mode-hook 'hs-minor-mode)

(require 'use-package)

(use-package ripgrep
  :bind (("C-c /" . projectile-ripgrep))
  :ensure)

(setq tab-width 2)
(setq-default tab-width 2)

(use-package forge
  :bind (("C-c i" . forge-list-assigned-issues)
         ("C-c h" . forge-list-requested-reviews)
         ("C-c l" . forge-list-authored-pullreqs))
  :after magit
  :ensure)

(setq forge-topic-list-limit '(10 . 5))

(setq magit-section-initial-visibility-alist
      (list
       '(stashes . show)
       '(unpushed . show)
       '(unpulled . show)
       '(pullreqs . show)
       '(issues . show)
       ))

(setq vc-follow-symlinks t)

(setq magit-loaded nil)
;; (global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-x g")
                (lambda ()
                  (interactive)
                  (magit-status)
                  (if (not magit-loaded)
                      (progn
                        (forge-pull)
                        (magit-toggle-margin)
                        (magit-toggle-margin-details)
                        (magit-refresh)
                        (setq magit-loaded t)))))

(use-package google-this
  :custom
;;  (google-this-wrap-in-quotes t)
  (google-this-browse-url-function
   (lambda (url)
     (browse-url url)))
  :bind (("C-c g" . google-this))
  :ensure)

;; (run-with-idle-timer 180 nil (lambda () (forge-pull)))

(use-package which-key
  :bind (("<f6>" . which-key-show-top-level)
         ("<f7>" . which-key-show-full-keymap))
  :ensure)

(global-set-key (kbd "M-/") 'quick-calc)

(setq which-key-paging-prefixes '("C-x"))

(which-key-mode)

;; which-key-mode
;; which-key-show-top-level
;; which-key-show-full-keymap
;; Good keys: C-c n, C-c C-n, C-x C-n, C-,

(use-package avy
  :bind (("C-'" . avy-goto-line)
         ("M-'" . avy-goto-end-of-line)
         ("C-." . avy-goto-char-timer)
         ("C-c K" . avy-kill-whole-line)
         ("C-c k" . avy-kill-region)
         ("C-c s" . avy-kill-ring-save-region)
         ("C-c m" . avy-move-region)
         ("C-c c" . avy-copy-region))
  :ensure)

(use-package rainbow-delimiters
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'text-mode-hook 'rainbow-delimiters-mode))
  :ensure)

(use-package direx
  :bind (("C-c C-c" . direx-project:jump-to-project-root))
  :ensure)

(use-package beacon
  :init
  (beacon-mode)
  :ensure)

(use-package move-text
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down))
  :ensure)

(defun newline-next-line ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(global-set-key (kbd "C-j") 'newline-next-line)

;; (use-package smartparens
;;   :bind (("C-M-a" . sp-beginning-of-sexp)
;;          ("C-M-e" . sp-end-of-sexp)
;;          ("C-M-f" . sp-forward-sexp)
;;          ("C-M-b" . sp-backward-sexp)
;;          ("C-M-k" . sp-kill-sexp)
;;          ("C-k"   . sp-kill-hybrid-sexp)
;;          ("M-k"   . sp-backward-kill-sexp)
;;          ("C-M-w" . sp-copy-sexp))
;; ;;  :custom
;; ;;  (smartparens-strict-mode t)
;;   :init
;;   (smartparens-global-strict-mode)
;;   :ensure)

(defun diff-file ()
  (interactive)
  (diff-buffer-with-file (buffer-name))
  (next-window-any-frame))

(defun revert ()
  (interactive)
  (revert-buffer nil t t))

;; (use-package org
;;   :bind ("C-c C-d" . diff-file))

;;  (sp-local-pair 'org-mode "*" nil)
;; (add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook #'turn-off-smartparens-mode)

(setq org-hide-leading-stars t)

(global-set-key (kbd "C-c C-d") 'diff-file)
(global-set-key (kbd "C-c C-r") 'revert)

;; `org-mode' key overrides
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-'") 'avy-goto-line))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-d") 'diff-file))
(with-eval-after-load 'org
  (define-key web-mode-map (kbd "C-c C-d") 'diff-file))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") 'revert))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-h") 'backward-delete-char))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-h") 'backward-kill-word))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c k") 'avy-kill-region))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c K") 'avy-kill-whole-line))
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c m") 'avy-move-region))

;; `web-mode' key overrides
(with-eval-after-load
    'web-mode
  (define-key web-mode-map (kbd "C-c C-r") 'revert))

(add-hook 'emacs-lisp-mode 'company-mode)

(global-company-mode)

;; Local electric pair delimiters in `org-mode'

(defvar org-electric-pairs '((?~ . ?~)) "Electric pairs for org-mode.")

(defun org-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs org-electric-pairs))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(add-hook 'org-mode-hook 'org-add-electric-pairs)

(use-package selectrum
  :init
  (selectrum-mode +1)
  :ensure)

(use-package selectrum-prescient
  :init
  (selectrum-prescient-mode +1)
  :ensure)

(use-package prescient
  :init
  (prescient-persist-mode +1)
  :ensure)

(use-package company-prescient
  :init
  (company-prescient-mode +1)
  :ensure)

(use-package consult
  :init
  :bind (("C-l" . consult-buffer)
         ("C-c e" . consult-goto-line)
         ("C-c /" . consult-git-grep)
         ("C-c o" . consult-grep)
         ("M-y" . consult-yank-pop))
  :ensure)

(use-package marginalia
  :init
  (marginalia-mode +1)
  (setq-default marginalia-annotators '(marginalia-annotators-heavy))
  :ensure)

(setq projectile-completion-system 'default)

;; C-- C-x o is super useful for recursive editing

(global-set-key (kbd "C-M-p") 'previous-multiframe-window)
(global-set-key (kbd "C-M-n") 'other-window)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-/") 'company-show-location)
(define-key company-active-map (kbd "C--") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)

(setq org-hide-emphasis-markers t)

(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Load extras

(if (file-exists-p "~/.emacs_extras.el")
    (load-file "~/.emacs_extras.el"))
