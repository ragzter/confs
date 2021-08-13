
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
; (ido-mode)

(defvar tide-electric-pairs '((?' . ?') (?< . ?>) (?` . ?`)) "Electric pairs for tide-mode.")

(defun ts-add-electric-pairs ()
  (interactive)
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

;; (if (window-system)
;;     (if (font-exists-p "Terminus-9")
;;         (set-frame-font "Terminus-9")
;;       (set-frame-font "Monospace-9")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes '(wombat))
 '(default-input-method "swedish-postfix")
 '(global-color-identifiers-mode t)
 '(js2-ignored-warnings '(""))
 '(js2-strict-missing-semi-warning nil)
 '(package-selected-packages
   '(json-mode expand-region goto-last-change dimmer marginalia selectrum-prescient consult selectrum smartparens move-text beacon direx rainbow-delimiters avy which-key google-this forge ripgrep company-prescient prescient use-package company projectile helm rust-mode prettier-js web-mode tide company-lsp flycheck lsp-ui lsp-typescript php-mode rjsx-mode idris-mode multiple-cursors centered-cursor-mode haskell-mode highlight-indent-guides)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:inherit font-lock-variable-name-face :background "#220024d635d6"))))
 '(company-preview-common ((t (:inherit font-lock-variable-name-face :background "#220024d635d6"))))
 '(company-scrollbar-bg ((t (:background "#220024d635d6"))))
 '(company-scrollbar-fg ((t (:background "#2be92f924587"))))
 '(company-tooltip ((t (:inherit default :background "#1c0e1e652c6c"))))
 '(company-tooltip-annotation ((t (:inherit font-lock-keyword-face))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(fringe ((t (:background "#1e0a208a2f8f"))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "light goldenrod"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "dark turquoise"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "medium slate blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "goldenrod1"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "dodger blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "MediumOrchid1"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "medium spring green"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "royal blue"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "MediumOrchid1")))))

;; (global-hl-line-mode)

;; (set-face-background hl-line-face "#406060")
;;(set-face-attribute hl-line-face nil :underline nil)
;; (set-face-foreground 'highlight nil)
;; (set-face-foreground 'highlight t)

(set-cursor-color "orange")
;; (setq-default cursor-type 'hbar)
(setq-default cursor-type 'box)

(global-display-line-numbers-mode)

(setq typescript-indent-level 2)

(add-to-list 'load-path "/home/ragnar/.nvm/versions/node/v10.9.0/bin/")
(add-to-list 'load-path "/home/ragnar/.yarn/bin/")

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)

  ;; Use same checker for "ts" file extension as with "tsx".
  (flycheck-define-generic-checker 'tsx-tide
    "A TSX syntax checker using tsserver."
    :start #'tide-flycheck-start
    :verify #'tide-flycheck-verify
    :modes '(web-mode)
    :predicate (lambda ()
                 (and
                  (or (tide-file-extension-p "ts") (tide-file-extension-p "tsx"))
                  (tide-flycheck-predicate))))

  (flycheck-add-next-checker 'javascript-eslint '(t . tsx-tide) 'append)
  (flycheck-select-checker 'javascript-eslint)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-check-syntax-automatically '(save)) ;; idle-change
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (setq-local company-backends
              '((company-tide company-files)))
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq tide-server-max-response-length 2147483647)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
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
        ("jsx" . "\\.ts[x]?\\'")))

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

(setq company-idle-delay nil) ; 0.2
(setq company-async-timeout 10)
(setq company-tooltip-limit 20)
(setq company-tooltip-minimum-width 15)
(setq company-require-match t)
(setq company-selection-wrap-around t)

(setq tide-completion-ignore-case t)
(setq tide-completion-detailed nil)
(setq tide-completion-show-source t)
(setq tide-always-show-documentation t)
(setq tide-hl-identifier-idle-time 0)

(setq flycheck-display-errors-delay 1)

(setq web-mode-auto-close-style 2)

(global-set-key (kbd "C-0") 'flycheck-mode)

(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

(global-set-key (kbd "C-c ?") 'magit-log-buffer-file)
(global-set-key (kbd "C-c f") 'magit-find-file)
(global-set-key (kbd "C-c r") 'tide-rename-symbol)
(global-set-key (kbd "C-c R") 'tide-rename-file)

(global-set-key (kbd "<f3>") (lambda () (interactive) (shell-command "free -h")))

(setq css-indent-offset 2)

;; (defun my/use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/use-eslint-from-node-modules ()
  (let* ((eslint "~/.yarn/bin/eslint_d"))
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
       '(unmerged . show)
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
;; Good keys: C-c n, C-c C-n, C-x C-n

(use-package avy
  :init
  (setq avy-timeout-seconds 0.25)
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?, ?. ?c ?r ?l))
  :bind (("C-'" . avy-goto-line)
         ("M-'" . avy-goto-end-of-line)
         ("C-." . avy-goto-word-1)
         ("C-c K" . avy-kill-whole-line)
         ("C-c k" . avy-kill-region)
         ("C-c s" . avy-kill-ring-save-region)
         ("C-c S" . avy-kill-ring-save-whole-line)
         ("C-c m" . avy-move-region)
         ("C-c M" . avy-move-line)
         ("C-c c" . avy-copy-region)
         ("C-c C" . avy-copy-line)
         ("C-3" . avy-isearch)
         ("C-2" . avy-resume))
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
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-,") 'execute-extended-command))

;; `web-mode' key overrides
(with-eval-after-load
    'web-mode
  (define-key web-mode-map (kbd "C-c C-r") 'revert))

;; `php-mode' key overrides
;; (with-eval-after-load 'org (define-key php-mode-map (kbd "C-.") 'avy-goto-word-1))

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
  (setq selectrum-quick-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?, ?. ?c ?r ?l))
  (setq selectrum-max-window-height 10)
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
         ("C-S-l" . consult-buffer-other-window)
         ("C-c e" . consult-goto-line)
;;         ("C-c /" . consult-git-grep)
         ("C-c o" . consult-git-grep)
         ("M-y" . consult-yank-pop))
  :ensure)

(use-package marginalia
  :init
  (marginalia-mode +1)
  (setq-default marginalia-annotators '(marginalia-annotators-heavy))
  :ensure)

(setq projectile-completion-system 'default)

;; C-- C-x o is super useful for recursive editing

;; (global-set-key (kbd "C-2") 'previous-multiframe-window)
;; (global-set-key (kbd "C-3") 'other-window)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-/") 'company-show-location)
(define-key company-active-map (kbd "C--") 'company-show-doc-buffer)
(define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "C-m") 'company-complete-selection)

(setq org-hide-emphasis-markers t)

(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face :background ,(color-lighten-name bg 10)))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))
   `(company-preview ((t (:inherit font-lock-variable-name-face :background ,(color-lighten-name bg 5)))))
   `(company-preview-common ((t (:inherit font-lock-variable-name-face :background ,(color-lighten-name bg 5)))))
   `(company-tooltip-annotation ((t (:inherit font-lock-keyword-face))))
   ))

(set-fringe-mode 10)

;; (set-fringe-mode 0)

;; (set-face-attribute 'fringe nil
;;                     :foreground (face-foreground 'default)
;;                     :background (face-background 'default))

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(fringe ((t (:background ,(color-lighten-name bg 3)))))))

(global-set-key (kbd "C-,") 'execute-extended-command)

(add-hook 'org-mode-hook
          (lambda () (set-face-attribute 'org-code nil :foreground "SeaGreen2" :italic t :bold nil)))
                                        ; (set-face-attribute 'org-code nil :foreground "SeaGreen2")

(use-package dimmer
  :bind (("C-9" . dimmer-mode))
  :init
  ;; (dimmer-mode +1)
  (setq dimmer-fraction 0.5)
  :ensure)

(use-package goto-last-change
  :bind (("C-/" . goto-last-change))
  :ensure)

(global-set-key (kbd "C--") 'other-window)

(defun reload-file ()
  "For the unfortunate times when modes get broken."
  (interactive)
  (let ((current-file (buffer-file-name))
        (current-point (point)))
    (kill-buffer)
    (find-file current-file)
    (goto-char current-point)))

(global-set-key (kbd "C-c C-'") 'reload-file)

(global-set-key (kbd "<C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-down>") 'shrink-window)
(global-set-key (kbd "<C-up>") 'enlarge-window)

(setq org-link-frame-setup '((file . find-file)))

(global-set-key (kbd "C-=") (lambda () (interactive) (kill-buffer (current-buffer))))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this-word)
         ("C-<" . mc/mark-previous-like-this-word)
         ("C-?" . mc/mark-pop))
  :ensure)

(delete-selection-mode t)

(setq gc-cons-threshold 80000000)
(setq read-process-output-max (* 1024 1024))

(use-package expand-region
  :bind (; ("C-4" . er/contract-region)
         ("C-4" . er/expand-region)
         ("C-5" . er/mark-method-call))
  :ensure)

(use-package json-mode
  :ensure)

(setq prettier-js-show-errors nil)

;; Skeletons!

(define-skeleton console-skeleton
  "Console skeleton"
  nil
  > "console.log(" _ ")"
  )
(global-set-key (kbd "C-c C-p") 'console-skeleton)

(define-skeleton const-skeleton
  "Const skeleton"
  nil
  > "const " _ " = "
  )

(define-skeleton return-skeleton
  "Return skeleton"
  nil
  > "return " _
  )

(define-skeleton function-skeleton
  "Function skeleton"
  nil
  "(" _ ") => {}"
  )

(define-skeleton async-function-skeleton
  "Async function skeleton"
  nil
  "async (" _ ") => {}"
  )

(define-skeleton styled-div-skeleton
  "Styled div skeleton"
  nil
  "styled.div`" \n
  "  " _ \n
  "`"
  )

(define-skeleton if-skeleton
  "If skeleton"
  nil
  > "if ("
  _
  ") {" \n
  > "\n"
  "}"
  >
  )

(define-skeleton else-skeleton
  "Else skeleton"
  nil
  " else {" \n
  > _ \n
  "}"
  >
  )

(define-skeleton space-skeleton
  "Space skeleton 8)"
  nil
  "\n"
  > _
  "\n"
  )
(global-set-key (kbd "C-c C-.") 'space-skeleton)

(define-skeleton space-block-start-skeleton
  "Space skeleton 8) at start of block"
  nil
  > _
  "\n"
  )

(defun goto-line-and-insert-space-skeleton ()
  (interactive)
  (avy-goto-line)
  (save-excursion
    (previous-line)
    (setq start-of-block-p (string= (substring (reverse (s-chomp (thing-at-point 'line t))) 0 1) "{")))
  (if start-of-block-p
      (space-block-start-skeleton)
    (space-skeleton)))

(global-set-key (kbd "C-c C-3") (lambda () (interactive) (avy-goto-line) (space-block-start-skeleton)))
(global-set-key (kbd "C-c C-,") 'goto-line-and-insert-space-skeleton)

(define-abbrev web-mode-abbrev-table "co" "" 'const-skeleton)
(define-abbrev web-mode-abbrev-table "om" "" 'if-skeleton)
(define-abbrev web-mode-abbrev-table "el" "" 'else-skeleton)
(define-abbrev web-mode-abbrev-table "f" "" 'function-skeleton)
(define-abbrev web-mode-abbrev-table "af" "" 'async-function-skeleton)
(define-abbrev web-mode-abbrev-table "ret" "return")
(define-abbrev web-mode-abbrev-table "pos" "position:")
(define-abbrev web-mode-abbrev-table "rel" "relative;")
(define-abbrev web-mode-abbrev-table "abs" "absolute;")
(define-abbrev web-mode-abbrev-table "ma" "margin:")
(define-abbrev web-mode-abbrev-table "mb" "margin-bottom:")
(define-abbrev web-mode-abbrev-table "mt" "margin-top:")
(define-abbrev web-mode-abbrev-table "ml" "margin-left:")
(define-abbrev web-mode-abbrev-table "mr" "margin-right:")
(define-abbrev web-mode-abbrev-table "pa" "padding:")
(define-abbrev web-mode-abbrev-table "pb" "padding-bottom:")
(define-abbrev web-mode-abbrev-table "pt" "padding-top:")
(define-abbrev web-mode-abbrev-table "pl" "padding-left:")
(define-abbrev web-mode-abbrev-table "pr" "padding-right:")
(define-abbrev web-mode-abbrev-table "fs" "font-size:")
(define-abbrev web-mode-abbrev-table "ff" "font-family:")
(define-abbrev web-mode-abbrev-table "df" "display: flex;")
(define-abbrev web-mode-abbrev-table "bc" "background-color:")
(define-abbrev web-mode-abbrev-table "lh" "line-height:")
(define-abbrev web-mode-abbrev-table "sd" "" 'styled-div-skeleton)
(define-abbrev web-mode-abbrev-table "ed" "export default ")

(define-skeleton styled-components-inline
  "Inline function for styled-components"
  nil
  "${(p) => " _ "};"
  )

(define-abbrev web-mode-abbrev-table "si" "" 'styled-components-inline)

(define-skeleton translation
  "Translation"
  nil
  "t('" _ "')"
  )

(define-abbrev web-mode-abbrev-table "si" "" 'styled-components-inline)
(define-abbrev web-mode-abbrev-table "tr" "" 'translation)

(define-skeleton state-skeleton
  "Write a state declaration"
  "Name: "
  "const [" str ", set" (s-upper-camel-case str) "] = useState(" _ ")")

(define-abbrev web-mode-abbrev-table "st" "" 'state-skeleton)

(define-skeleton effect-skeleton
  "Write an effect"
  nil
  "useEffect (() => {\n" > _ "\n" "}, [])" >)

(define-abbrev web-mode-abbrev-table "ue" "" 'effect-skeleton)

(define-skeleton select-skeleton
  ""
  nil
  "`\nselect\n  " _ "\nfrom\n  \nwhere\n  \n`")
(define-skeleton update-skeleton
  ""
  nil
  "`\nupdate\n  " _ "\nset\n  \nwhere\n  \n`")
(define-skeleton delete-skeleton
  ""
  nil
  "`\ndelete from\n  " _ "\nwhere\n  \n`")

(define-abbrev web-mode-abbrev-table "sel" "" 'select-skeleton)
(define-abbrev web-mode-abbrev-table "upd" "" 'update-skeleton)
(define-abbrev web-mode-abbrev-table "del" "" 'delete-skeleton)

(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.tsx\\'" . "React skeleton")
     '(nil
       "import React from 'react'\n\n"
       "const " (reverse (substring (reverse (buffer-name)) 4)) " = () => {\n"
       > "return <>" _ "</>"
       "\n}\n\n"
       "export default " (reverse (substring (reverse (buffer-name)) 4)))))
;; (eval-after-load 'autoinsert
;;   '(define-auto-insert
;;      '("\\.ts\\'" . "TypeScript skeleton")
;;      '(nil
;;        "export default () => {\n"
;;        > _
;;        "\n}\n\n")))
(auto-insert-mode)
(setq auto-insert-query nil)

;; df

(define-key web-mode-map (kbd "C-c C-f") 'tide-fix)

(add-hook 'web-mode-hook 'abbrev-mode)

(use-package s
  :ensure)

;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /home/ragnar/tss.log"))
(setq tide-sync-request-timeout 120)

;; (defun my-copy-word-and-yank ()
;;   (interactive)
;;   (let ((buffer (current-buffer)))
;;     (save-excursion
;;       (call-interactively 'avy-goto-word-1)
;;       (er/mark-word)
;;       (call-interactively 'kill-ring-save)
;;       (deactivate-mark)
;;       (if (not (eq buffer (current-buffer)))
;;           (call-interactively 'other-window))))
;;   (yank))
;;
;; (defun my-copy-region-and-yank ()
;;   (interactive)
;;   (let ((buffer (current-buffer)))
;;     (save-excursion
;;       (call-interactively 'avy-goto-word-1)
;;       (call-interactively 'set-mark-command)
;;       (call-interactively 'avy-goto-word-1)
;;       (call-interactively 'kill-ring-save)
;;       (deactivate-mark)
;;       (if (not (eq buffer (current-buffer)))
;;           (call-interactively 'other-window)))
;;     (yank)))
;;
;; (global-set-key (kbd "C-c C-o") 'my-copy-word-and-yank)
;; (global-set-key (kbd "C-c C-s") 'my-copy-region-and-yank)
;; (define-key web-mode-map (kbd "C-c C-s") 'my-copy-region-and-yank)

(setq consult-project-root-function 'projectile-project-root)

(defun copy-type-signature ()
  (interactive)
  (unless (member last-command '(next-error previous-error))
    (if (tide-method-call-p)
        (tide-command:signatureHelp #'tide-eldoc-maybe-show)
      (when (looking-at "\\s_\\|\\sw")
        (tide-command:quickinfo
         (tide-on-response-success-callback response (:ignore-empty t)
           (let* ((raw (substring-no-properties (seq--into-string (tide-doc-text (plist-get response :body))))))
                  ;; (s (s-replace " | undefined" "" (car (last (split-string raw ": "))))))
             (kill-new raw))))))))

(defun copy-type-signature-partial ()
  (interactive)
  (unless (member last-command '(next-error previous-error))
    (if (tide-method-call-p)
        (tide-command:signatureHelp #'tide-eldoc-maybe-show)
      (when (looking-at "\\s_\\|\\sw")
        (tide-command:quickinfo
         (tide-on-response-success-callback response (:ignore-empty t)
           (let* ((raw (substring-no-properties (seq--into-string (tide-doc-text (plist-get response :body)))))
                  (s (s-replace " | undefined" "" (car (last (split-string raw ": "))))))
             (kill-new s))))))))

(global-set-key (kbd "C-c C-s") 'copy-type-signature)
(define-key web-mode-map (kbd "C-c C-s") 'copy-type-signature)
(global-set-key (kbd "C-S-s") 'copy-type-signature-partial)
(define-key web-mode-map (kbd "C-S-s") 'copy-type-signature-partial)

(setq save-abbrevs nil)

;; Load extras

(if (file-exists-p "~/.emacs_extras.el")
    (load-file "~/.emacs_extras.el"))
