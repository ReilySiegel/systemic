(define-module (systemic home kotlin)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (rde home services emacs)
  #:use-module (systemic home emacs-utils)
  #:export (services))

(define services
  (list
   (simple-service 'kotlin-emacs home-emacs-service-type
                   (emacs-configuration-extension
                    (emacs-kotlin-mode)))))
