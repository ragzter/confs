
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
  (if (not (package-installed-p 'idris-mode))
      (package-install 'idris-mode))
  (if (not (package-installed-p 'highlight-indent-guides))
      (package-install 'highlight-indent-guides))
  (if (not (package-installed-p 'magit))
      (package-install 'magit)))

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
(global-set-key (kbd "C-z") 'hippie-expand)
(global-set-key (kbd "M-p") 'scroll-down-line)
(global-set-key (kbd "M-n") 'scroll-up-line)
(global-set-key (kbd "C-x g") 'magit-status)

(defun eval-haskell-code ()
  (interactive)
  (fset 'eval-haskell-code
        "\C-xo:load TTT\C-m\C-xo"))

(global-set-key (kbd "C-c C-r") 'eval-haskell-code)

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
    (rjsx-mode magit idris-mode multiple-cursors centered-cursor-mode haskell-mode highlight-indent-guides))))

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
