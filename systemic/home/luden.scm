(define-module (systemic home luden)
  #:use-module (gnu home)
  #:use-module (gnu services)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services ssh)
  #:use-module (systemic home desktop)
  #:use-module ((systemic home base) #:prefix base:))

(define-public environment
  (home-environment
   (services
    (cons*
     (service picom-service-type #f)
     (service exwm-service-type)
     (service home-gnupg-service-type
	      (home-gnupg-configuration
               (gpg-agent-config
                (home-gpg-agent-configuration
                 (ssh-agent? #t)
                 (pinentry-flavor 'emacs)))))            
     (service home-ssh-service-type
              (home-ssh-configuration
               (toplevel-options
                '((match .
                    "host * exec \"gpg-connect-agent UPDATESTARTUPTTY /bye\"")))))
     (home-environment-user-services base:environment)))))
