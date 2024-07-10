(define-module (systemic home idris)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:use-module (rde home services emacs)
  #:export (services))

(define services
  (list
   (simple-service
    'idris-emacs home-emacs-service-type
    (emacs-configuration-extension
     (emacs-idris-mode
      (with-eval-after-load 'flycheck
        (require 'flycheck-idris)
        (add-hook 'idris-mode-hook #'flycheck-mode))
      (with-eval-after-load 'aggressive-indent
        (add-to-list 'aggressive-indent-excluded-modes 'idris-mode))
      (setopt idris-interpreter-path "idris2"
              idris-stay-in-current-window-on-compiler-error t))))
   (simple-service
    'idris-env
    home-environment-variables-service-type
    `(("PATH" . "$HOME/.pack/bin:$PATH")))))

