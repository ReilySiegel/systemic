(define-module (systemic home shell)
  #:use-module (gnu home services shells)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (rde home services emacs)
  #:use-module (systemic home emacs-utils)
  #:export (systemic-shell-service-type))

(define eat-bash-integration
  (plain-file
   "source-eat-integration"
   "
[ \"$EAT_SHELL_INTEGRATION_DIR\" ] && source \"$EAT_SHELL_INTEGRATION_DIR/bash\"
[ -n \"$__eat_integration_enabled\" ] && TERM=xterm-256color"))

(define (bash-extension _)
  (home-bash-extension
    (bashrc (list eat-bash-integration))))

(define (emacs-extension _)
  (emacs-configuration-extension
   ("tramp"
    (with-eval-after-load 'tramp
      (setopt tramp-show-ad-hoc-proxies t
              tramp-ssh-controlmaster-options
              (concat
               "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
               "-o ControlMaster=auto -o ControlPersist=yes"))))
   (emacs-eat
    (keymap-global-set "C-c s" #'eat)
    (setopt eat-enable-shell-prompt-annotation nil
            eat-kill-buffer-on-exit t)
    (add-hook 'eat-mode-hook
              (lambda () (setq-local scroll-conservatively 101))))
   (emacs-buffer-env
    (setopt buffer-env-script-name "guix.scm")
    (add-hook 'hack-local-variables-hook #'buffer-env-update)
    (add-hook 'comint-mode-hook #'buffer-env-update))
   (emacs-inheritenv
    (with-eval-after-load 'comint
      (require 'inheritenv)
      (inheritenv-add-advice #'make-comint)))))


(define systemic-shell-service-type
  (service-type
    (name 'systemic-shell)
    (description "Shell configuration")
    (default-value #f)
    (extensions
     (list
      (service-extension home-bash-service-type bash-extension)
      (service-extension home-emacs-service-type emacs-extension)))))
