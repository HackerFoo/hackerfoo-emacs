(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#000000")
 '(background-mode dark)
 '(custom-enabled-themes (quote (gruber-darker)))
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "4190edf57c89d2ec0470c1aaf66f4d6ec3691b41074efcd6f4cd806242f8f935" "ce8998464858cd579515f35dd9c582f03e14175d898f67ace69f6a6c5624ed68" "9b6130d4d7896da7ae73652ec541a648fac70353ad3808bde69a91d5db0fedd9" "857e2b73466f6fc0c996b69e5c9ba9f239847434dd846bfa25ae98534ed628d4" default)))
 '(elfeed-feeds
   (quote
    ("https://www.reddit.com/r/emacs/.rss" "http://www.reddit.com/r/programming/.rss")))
 '(elfeed-search-title-max-width 200)
 '(haskell-mode-hook (quote (turn-on-haskell-indent)))
 '(matrix-homeserver-base-url "https://sandbox.hackerfoo.com:8448")
 '(org-startup-truncated nil)
 '(package-selected-packages
   (quote
    (scad-mode scad-preview z3-mode htmlize ox-reveal exwm xwidgete elfeed helm-ag matrix-client window-purpose sr-speedbar smart-tab smart-mode-line shell-switcher rtags req-package persp-projectile org-projectile org-gcal magit irony-eldoc helm-projectile helm-dash gruber-darker-theme golden-ratio framemove flycheck-pos-tip flycheck-irony flycheck-color-mode-line expand-region ensime diff-hl company-irony auto-complete auto-compile)))
 '(password-cache-expiry nil)
 '(rtags-enable-unsaved-reparsing t)
 '(rtags-periodic-reparse-timeout 1.0)
 '(rtags-reparse-timeout 30)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(z3-solver-cmd "/usr/bin/z3"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#e4e4ef" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Inconsolata"))))
 '(company-preview ((t (:underline t))))
 '(company-preview-common ((t (:inherit company-preview :foreground "deep sky blue"))))
 '(company-scrollbar-bg ((t (:inherit company-tooltip :background "deep sky blue"))))
 '(company-scrollbar-fg ((t (:background "white"))))
 '(company-tooltip ((t (:background "gray30" :foreground "white"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "deep sky blue"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold))))
 '(company-tooltip-selection ((t (:inherit company-tooltip :background "steel blue"))))
 '(helm-source-header ((((class color) (min-colors 89)) (:foreground "#626262" :background "#729fcf" :bold t))))
 '(magit-item-highlight ((t nil)) t)
 '(org-hide ((t (:foreground "gray15"))))
 '(persp-selected-face ((t (:foreground "deep sky blue" :weight bold))))
 '(proof-locked-face ((t (:background "gray16")))))
