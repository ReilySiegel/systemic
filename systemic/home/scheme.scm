(define-module (systemic home scheme)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu services)
  #:use-module (rde home services emacs)
  #:use-module (systemic home emacs-utils)
  #:export (scheme-service-type))

(define scheme-service-type
  (service-type
   (name 'systemic-scheme)
   (description "Adds necessary configuration for a guile environment.")
   (default-value #f)
   (extensions
    (list
     (service-extension home-profile-service-type (const (list guile-3.0)))
     (service-extension home-emacs-service-type
                        (const (emacs-configuration-extension
                                (emacs-guix
                                 (setopt geiser-repl-per-project-p t)))))))))
