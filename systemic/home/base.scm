(define-module (systemic home base)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (srfi srfi-26)
  #:use-module ((systemic home emacs) #:prefix emacs:)
  #:use-module (systemic home git))

(define-public environment
  (home-environment
   (services
    (append
     emacs:services
     (list
      (service systemic-git-service-type))))))
