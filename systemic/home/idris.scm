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
      (with-eval-after-load 'aggressive-indent
        (add-to-list 'aggressive-indent-excluded-modes 'idris-mode))
      (setopt idris-interpreter-path "idris2"
              idris-repl-show-repl-on-startup nil
              idris-hole-show-on-load nil
              idris-show-help-text nil
              idris-completion-via-compiler nil
              idris-warnings-printing (list 'warnings-repl)
              idris-stay-in-current-window-on-compiler-error t)
      (remove-hook 'idris-mode-hook 'turn-on-eldoc-mode)
      (add-hook 'idris-mode-hook (lambda () (eldoc-mode -1)))
      (add-hook 'idris-mode-hook 'idris-simple-indent-mode))))))

