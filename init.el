;;; init --- My Emacs Configuration

;;; Commentary:
;; Must use Emacs 24

;;; Code:
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)


;;; Initial stuff:
(require 'eieio)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'req-package)

(defvar use-rtags t)
(defvar use-irony t)
(defvar use-ycmd nil)

;;; Packages:
(req-package smart-tab
  :config (global-smart-tab-mode t))

(req-package yasnippet
  :config
  (progn
    ;; yasnippet
    (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
    ;;(yas/initialize)

    ;; This is where your snippets will lie.
    (setq yas/root-directory '("~/.emacs.d/snippets"))
    (mapc 'yas/load-directory yas/root-directory)
  ))

(req-package helm-config
  :require helm
  :bind (("C-c h" . helm-mini)
         ("C-c g" . helm-get-grep)
         ("M-i" . helm-imenu))
  :config
  (progn
    ;; Invoke `helm-git-grep' from isearch.
    (define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
    ;; Invoke `helm-git-grep' from other helm.
    (define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm)))

(req-package helm
  :config (helm-mode t))

(req-package magit
  :bind ("C-x g" . magit-status))

(req-package evernote-mode
  :bind (("C-c e c" . evernote-create-note)
         ("C-c e o" . evernote-open-note)
         ("C-c e s" . evernote-search-notes)
         ("C-c e S" . evernote-do-saved-search)
         ("C-c e w" . evernote-write-note)
         ("C-c e p" . evernote-post-region)
         ("C-c e b" . evernote-browser))
  :config
  (progn
    (setq evernote-username "hackerfoo") ; optional: you can use this username as default.
    (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
  ))

(req-package auto-complete-config
  :require auto-complete
  :config
  (progn
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4.20110207/dict")
    
    (setq-default ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
    (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
    (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
    (add-hook 'css-mode-hook 'ac-css-mode-setup)
    (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    ;(global-auto-complete-mode t)
    (add-to-list 'ac-modes 'objc-mode)))

(req-package auto-complete)

(req-package projectile
  :config (projectile-global-mode))
(req-package helm-projectile
  :require (helm projectile)
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)))

(req-package sr-speedbar
  :bind (("<f12>" . sr-speedbar-toggle)))

(if (executable-find "w3m")
  (req-package xcode-document-viewer))

(req-package ensime)
(req-package gruber-darker-theme)
(when (memq window-system '(mac ns))
  (req-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)))

(req-package purpose
  :config (purpose-mode t))

(req-package golden-ratio
  :config (golden-ratio-mode t))

(req-package smart-mode-line
  :config
  (progn
    (setq sml/theme 'respectful)
    (sml/setup)))

(req-package expand-region
  :bind (("C-=" . er/expand-region)))

(if use-rtags
  (req-package rtags
    :config
    (progn
    (defun use-rtags (&optional useFileManager)
      (and (rtags-executable-find "rc")
           (cond ((and (not (eq major-mode 'c++-mode))
                       (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
                 (useFileManager (rtags-has-filemanager))
                 (t (rtags-is-indexed)))))

    (defun tags-find-symbol-at-point (&optional prefix)
      (interactive "P")
      (if (and (not (rtags-find-symbol-at-point prefix)) rtags-last-request-not-indexed)
          (gtags-find-tag)))
    (defun tags-find-references-at-point (&optional prefix)
      (interactive "P")
      (if (and (not (rtags-find-references-at-point prefix)) rtags-last-request-not-indexed)
          (gtags-find-rtag)))
    (defun tags-find-symbol ()
      (interactive)
      (call-interactively (if (use-rtags) 'rtags-find-symbol 'gtags-find-symbol)))
    (defun tags-find-references ()
      (interactive)
      (call-interactively (if (use-rtags) 'rtags-find-references 'gtags-find-rtag)))
    (defun tags-find-file ()
      (interactive)
      (call-interactively (if (use-rtags t) 'rtags-find-file 'gtags-find-file)))
    (defun tags-imenu ()
      (interactive)
      (call-interactively (if (use-rtags t) 'rtags-imenu 'helm-imenu)))

    (define-key c-mode-base-map (kbd "M-.") (function tags-find-symbol-at-point))
    (define-key c-mode-base-map (kbd "M-,") (function tags-find-references-at-point))
    (define-key c-mode-base-map (kbd "M-;") (function tags-find-file))
    (define-key c-mode-base-map (kbd "C-.") (function tags-find-symbol))
    (define-key c-mode-base-map (kbd "C-,") (function tags-find-references))
    (define-key c-mode-base-map (kbd "C-<") (function rtags-find-virtuals-at-point))
    (define-key c-mode-base-map (kbd "M-i") (function tags-imenu))

    (define-key global-map (kbd "M-.") (function tags-find-symbol-at-point))
    (define-key global-map (kbd "M-,") (function tags-find-references-at-point))
    (define-key global-map (kbd "M-;") (function tags-find-file))
    (define-key global-map (kbd "C-.") (function tags-find-symbol))
    (define-key global-map (kbd "C-,") (function tags-find-references))
    (define-key global-map (kbd "C-<") (function rtags-find-virtuals-at-point))
    (define-key global-map (kbd "M-i") (function tags-imenu)))))

(if use-ycmd
  (progn
    (req-package ycmd
      :config
      (progn
        (set-variable 'ycmd-server-command '("python" "/opt/ycmd/ycmd"))
        (set-variable 'ycmd-idle-change-delay 0.5)
        (set-variable 'ycmd-parse-conditions '(save new-line idle-change mode-enabled))
        (add-hook 'c-mode-hook 'ycmd-mode)
        (add-hook 'c++-mode-hook 'ycmd-mode)))

    (req-package flycheck-ycmd
      :require (flycheck ycmd)
      :config
      (flycheck-ycmd-setup))

    (req-package company-ycmd
      :require (company ycmd)
      :config
      (company-ycmd-setup))))

(if use-irony
  (progn
    (req-package irony
      :config
      (progn
        (add-hook 'c++-mode-hook 'irony-mode)
        (add-hook 'c-mode-hook 'irony-mode)
        (add-hook 'objc-mode-hook 'irony-mode)

        ;; replace the `completion-at-point' and `complete-symbol' bindings in
        ;; irony-mode's buffers by irony-mode's function
        (defun my-irony-mode-hook ()
          (define-key irony-mode-map [remap completion-at-point]
            'irony-completion-at-point-async)
          (define-key irony-mode-map [remap complete-symbol]
            'irony-completion-at-point-async))
        (add-hook 'irony-mode-hook 'my-irony-mode-hook)
        (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)))

    (req-package company-irony
      :require (company irony)
      :config
      (progn
        (add-to-list 'company-backends 'company-irony)
        (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

    (req-package flycheck-irony
      :require (flycheck irony)
      :config
       (add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

    (req-package irony-eldoc
      :require (irony eldoc)
      :config
      (add-hook 'irony-mode-hook 'irony-eldoc))
))

(req-package flycheck
  :config
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (defun flycheck-gcc-include-local-dir ()
      "Add the current dir to the gcc checker include list"
      (if (derived-mode-p 'c-mode 'c++-mode)
        (add-to-list 'flycheck-gcc-include-path (file-name-directory (buffer-file-name)))))
    (add-hook 'flycheck-before-syntax-check-hook 'flycheck-gcc-include-local-dir)
))

(req-package popup)

(req-package flycheck-pos-tip
  :require (flycheck popup)
  :config
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(req-package flycheck-color-mode-line
  :require flycheck
  :config
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(if (not use-rtags)
  (req-package ggtags
    :config
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode 1))))))

(req-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0)
  (custom-set-faces
   '(company-preview ((t (:underline t))))
   '(company-preview-common ((t (:inherit company-preview :foreground "deep sky blue"))))
   '(company-scrollbar-bg ((t (:inherit company-tooltip :background "deep sky blue"))))
   '(company-scrollbar-fg ((t (:background "white"))))
   '(company-tooltip ((t (:background "gray30" :foreground "white"))))
   '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "deep sky blue"))))
   '(company-tooltip-common ((t (:inherit company-tooltip :weight bold))))
   '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold))))
   '(company-tooltip-selection ((t (:inherit company-tooltip :background "steel blue"))))))

(req-package readline-complete
  :require company
  :config
  (progn
    (push 'company-readline company-backends)))
;    (add-hook 'rlc-no-readline-hook (lambda () (company-mode -1)))))

;; (req-package company-quickhelp
;;   :require (company pos-tip)
;;   :config
;;   (company-quickhelp-mode 1))

(req-package-finish)

;;; UI options:
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;; Misc key bindings
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key [pause] 'toggle-window-dedicated)
(global-set-key [f5] 'recompile)
(global-set-key [f6] 'rgrep)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-x p") 'objc-headline)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(global-set-key (kbd "C-c v") 'revert-buffer-no-confirm)

;;; Misc settings
(show-paren-mode t)

(setq dired-listing-switches "-lgG")

(add-hook 'c-mode-common-hook
  '(lambda ()
     (setq glasses-face "bold")
     (setq glasses-separator nil)
     (hs-minor-mode t)))

;; handle output from iarbuild
(add-hook 'compilation-mode-hook
  '(lambda ()
     (setq split-width-threshold nil)
     (setq compilation-window-height 12)
     (setq compilation-scroll-output 'first-error)
     (add-to-list 'compilation-error-regexp-alist 'iarbuild)
     (add-to-list 'compilation-error-regexp-alist-alist
       '(iarbuild "^\\(.*\\)(\\([0-9]+\\))" 1 2))))

(add-hook 'hs-minor-mode-hook
  '(lambda ()
     (hs-hide-initial-comment-block)
     (local-set-key (kbd "C-<tab>") 'hs-toggle-hiding)))

(setq hippie-expand-try-functions-list
  '(try-expand-dabbrev
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-complete-file-name-partially
    try-complete-file-name
    try-expand-all-abbrevs
    try-expand-list
    try-expand-line
    try-complete-lisp-symbol-partially
    try-complete-lisp-symbol))

(autoload 'imaxima "imaxima" "Image support for Maxima." t)

(if (executable-find "w3m")
  (setq browse-url-browser-function 'w3m))

(setq-default indent-tabs-mode nil)
(setq c-default-style "linux")
(setq c-basic-offset 2)
(winner-mode t)
(windmove-default-keybindings)

(setq org-src-fontify-natively t)

;; save automatically
; (setq auto-save-visited-file-name t
;       auto-save-interval 1
;       auto-save-timeout 1)
;
; (defun full-auto-save ()
;   (interactive)
;   (save-excursion
;       (dolist (buf (buffer-list))
;         (set-buffer buf)
;         (if (and (buffer-file-name) (buffer-modified-p))
;                 (basic-save-buffer)))))
; (add-hook 'auto-save-hook 'full-auto-save)

;;; Toggle window dedication:
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
          (not (window-dedicated-p window))))
      "Window '%s' is dedicated"
      "Window '%s' is normal")
   (current-buffer)))

;;; TeX:
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
(set-default 'preview-scale-function 2.0)

;;; Imenu:
(require 'imenu)
(setq imenu-auto-rescan t)

;; Local Variables:
;; imenu-generic-expression: (("Section" "^\\s<+\\s-+\\(.+\\):$" 1) ("Package" "^(req-package \\(\\S-+\\)$" 1))
;; End:

(provide 'init)
;;; init.el ends here

