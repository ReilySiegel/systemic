(define-module (systemic home emacs shell)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (rde home services emacs)
  #:use-module (systemic home emacs-utils)
  #:export (service))

(define service
  (simple-service
   'emacs-eshell-service
   home-emacs-service-type
   (emacs-configuration-extension
    ("eshell"
     (keymap-global-set "C-c s" #'eshell)
     (with-eval-after-load 'eshell
       (add-to-list 'eshell-modules-list 'eshell-smart)
       (add-to-list 'eshell-modules-list 'eshell-elecslash)
       (setopt eshell-history-size 4096
               eshell-hist-ignoredups t
               remote-file-name-inhibit-cache nil
               eshell-scroll-to-bottom-on-input t
               vc-ignore-dir-regexp
               (format "%s\\|%s" vc-ignore-dir-regexp tramp-file-name-regexp))))
    ("tramp"
     (with-eval-after-load 'tramp
       (setopt tramp-show-ad-hoc-proxies t
               tramp-ssh-controlmaster-options
               (concat
                "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                "-o ControlMaster=auto -o ControlPersist=yes"))))
    (emacs-eat
     (setopt eshell-visual-commands nil
             ;; Better remote compatibility.
             eat-term-name "xterm-256color")
     (add-hook 'eshell-load-hook #'eat-eshell-mode))
    (emacs-buffer-env
     (setopt buffer-env-script-name "guix.scm")
     (add-hook 'hack-local-variables-hook #'buffer-env-update)
     (add-hook 'comint-mode-hook #'buffer-env-update))
    (emacs-inheritenv
     (with-eval-after-load 'comint
       (inheritenv-add-advice #'make-comint))))))
