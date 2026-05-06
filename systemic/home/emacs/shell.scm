(define-module (systemic home emacs shell)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (rde home services emacs)
  #:use-module (systemic home emacs-utils)
  #:export (service))

(define services
  (list
   (simple-service
    'eat-terminal-integration
    home-bash-service-type
    (home-bash-extension
      (bashrc  (list
                #~(string-append
                   "source "
                   #$emacs-eat
                   "/share/emacs/site-lisp/eat-"
                   (package-version emacs-eat)
                   "/integration/bash")))))
   (simple-service
    'emacs-eshell-service
    home-emacs-service-type
    (emacs-configuration-extension
     ("eshell"
      (with-eval-after-load 'eshell
        (add-to-list 'eshell-modules-list 'eshell-smart)
        (add-to-list 'eshell-modules-list 'eshell-elecslash)
        (setopt eshell-history-size 4096
                eshell-hist-ignoredups t
                remote-file-name-inhibit-cache nil
                eshell-scroll-to-bottom-on-input t)))
     ("tramp"
      (with-eval-after-load 'tramp
        (setopt tramp-show-ad-hoc-proxies t
                tramp-ssh-controlmaster-options
                (concat
                 "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                 "-o ControlMaster=auto -o ControlPersist=yes"))))
     (emacs-eat
      (keymap-global-set "C-c s" #'eat)
      (setopt eshell-visual-commands nil
              ;; Better remote compatibility.
              eat-term-name "xterm-256color")
      (add-hook 'eshell-load-hook #'eat-eshell-mode)
      (add-hook 'eat-mode-hook (lambda () (setq-local scroll-conservatively 101))))
     (emacs-buffer-env
      (setopt buffer-env-script-name "guix.scm")
      (add-hook 'hack-local-variables-hook #'buffer-env-update)
      (add-hook 'comint-mode-hook #'buffer-env-update))
     (emacs-inheritenv
      (with-eval-after-load 'comint
        (inheritenv-add-advice #'make-comint)))))))
