;; init.el for this setup. Must use Emacs 24
(require 'org)
(org-babel-load-file
 (expand-file-name "emacs-init.org" user-emacs-directory))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
