;; init.el for this setup. Must use Emacs 24


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(require 'org)
(org-babel-load-file
 (expand-file-name "emacs-init.org" user-emacs-directory))
