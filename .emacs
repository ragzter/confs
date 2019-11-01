
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
     highlight-indent-guides
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
; (setq left-fringe-width 10)
; (setq right-fringe-width 10)

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

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(electric-pair-mode 1)
(show-paren-mode 1)
(global-prettify-symbols-mode)
(iswitchb-mode)
(ido-mode)

(defun ts-add-electric-pairs ()
  (setq-local electric-pair-pairs (append electric-pair-pairs '((?\' . ?\'))))
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

(if (package-installed-p 'highlight-indent-guides)
    (add-hook 'prog-mode-hook
              (lambda ()
                (interactive)
                (if (< (count-lines-buffer) 1000)
                    (highlight-indent-guides-mode)))))

(add-hook 'prog-mode-hook
          (lambda ()
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '("&&" . ?⋀) prettify-symbols-alist)
            (push '("||" . ?⋁) prettify-symbols-alist)
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
(global-set-key (kbd "C-z") 'company-complete)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-c C-l") 'comment-region)
(global-set-key (kbd "C-c C-/") 'uncomment-region)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun font-exists-p (font)
  (if (null (x-list-fonts font)) nil t))

(if (font-exists-p "Fantasque Sans Mono-12")
    (set-default-font "Fantasque Sans Mono-12")
  (set-default-font "Monospace-9"))

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
    (company projectile helm rust-mode prettier-js web-mode tide company-lsp flycheck lsp-ui lsp-typescript typescript-mode php-mode rjsx-mode magit idris-mode multiple-cursors centered-cursor-mode haskell-mode highlight-indent-guides))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-hl-line-mode)

(set-face-background hl-line-face "#101010")
(set-face-attribute hl-line-face nil :underline nil)
(set-face-foreground 'highlight nil)

(set-cursor-color "orange")
(setq-default cursor-type 'hbar)

(defun find-overlays-specifying (prop pos)
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun highlight-or-dehighlight-line ()
  (interactive)
  (if (find-overlays-specifying
       'line-highlight-overlay-marker
       (line-beginning-position))
      (remove-overlays (line-beginning-position) (+ 1 (line-end-position)))
    (let ((overlay-highlight (make-overlay
                              (line-beginning-position)
                              (+ 1 (line-end-position)))))
        (overlay-put overlay-highlight 'face '(:background "#006030"))
        (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))

(global-set-key (kbd "C-c C-c") 'highlight-or-dehighlight-line)

(define-skeleton if-skeleton
  "If skeleton"
  nil
  > "if ("
  _
  ") {\n"
  > "\n"
  "}"
  >
  )

(define-skeleton else-skeleton
  "Else skeleton"
  nil
  " else {\n"
  > _ "\n"
  "}"
  >
  )

(define-skeleton map-skeleton
  "Map skeleton"
  nil
  ".map(e => {\n"
  > _ "\n"
  "})"
  >
  )

(define-skeleton map-index-skeleton
  "Map with index skeleton"
  nil
  ".map((e, i) => {\n"
  > _ "\n"
  "})"
  >
  )

(define-skeleton console-skeleton
  "Console skeleton"
  nil
  "console.log(" _ ")"
  )

(global-set-key (kbd "C-c C-i") 'if-skeleton)
(global-set-key (kbd "C-c C-e") 'else-skeleton)
(global-set-key (kbd "C-c C-m") 'map-skeleton)
(global-set-key (kbd "C-c C-S-m") 'map-index-skeleton)
(global-set-key (kbd "C-c C-p") 'console-skeleton)

(eval-after-load 'autoinsert
  '(define-auto-insert
     '("\\.js\\'" . "React skeleton")
     '(nil
       "\nimport React from 'react'\n\n"
       "export default props => {\n"
       > _
       "\n}\n")))

(auto-insert-mode)

(if (file-exists-p "~/.emacs_extras.el")
    (load-file "~/.emacs_extras.el"))

(global-display-line-numbers-mode)

(setq typescript-indent-level 2)

(add-to-list 'load-path "/home/ragnar/.nvm/versions/node/v10.9.0/bin/")
(add-to-list 'load-path "/home/ragnar/.yarn/bin/")

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
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
;; enable typescript-tslint checker
;; (flycheck-add-mode 'typescript-tslint 'web-mode)

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'typescript-tslint 'web-mode)
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

(require 'prettier-js)

(setq-default fill-column 80)
(add-hook 'org-mode-hook 'auto-fill-mode)

(add-hook 'tide-mode-hook 'prettier-js-mode)

(setq sh-basic-offset 2)

;; Helm

; (require 'helm-config)

;; Projectile

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq company-idle-delay 0.2)
(setq company-async-timeout 5)

(setq web-mode-auto-close-style 2)
