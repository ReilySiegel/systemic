(define-module (systemic home haskell)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (systemic home emacs-utils)
  #:export (services))

(define services
  (list (simple-service
         'haskell-emacs home-emacs-service-type
         (emacs-configuration-extension
          (emacs-haskell-mode)))))
