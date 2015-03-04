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
    ("a655f17225ad0a7190c79602593563191b7640ddebbb8c8fbd80c9d82faff1c6" "4190edf57c89d2ec0470c1aaf66f4d6ec3691b41074efcd6f4cd806242f8f935" "ce8998464858cd579515f35dd9c582f03e14175d898f67ace69f6a6c5624ed68" "9b6130d4d7896da7ae73652ec541a648fac70353ad3808bde69a91d5db0fedd9" "857e2b73466f6fc0c996b69e5c9ba9f239847434dd846bfa25ae98534ed628d4" default)))
 '(haskell-mode-hook (quote (turn-on-haskell-indent)))
 '(org-startup-truncated nil)
 '(safe-local-variable-values
   (quote
    ((gud-gdb-command-name . "arm-none-eabi-gdb -i=mi")
     (gud-gdb-command-name . "arm-none-eabi-gdb -i=mi build/last.elf -cd .. -x gdb/commands.gdb")
     (gud-gdb-command-name . "arm-none-eabi-gdb -i=mi build/sensor.elf -cd .. -x gdb/commands.gdb"))))
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#e4e4ef" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Menlo"))))
 '(helm-source-header ((((class color) (min-colors 89)) (:foreground "#626262" :background "#729fcf" :bold t))))
 '(magit-item-highlight ((t nil)) t)
 '(org-hide ((t (:foreground "gray15")))))
