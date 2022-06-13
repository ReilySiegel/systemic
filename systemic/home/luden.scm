(define-module (systemic home luden)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services mail)
  #:use-module (gnu home-services password-utils)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (systemic home desktop)
  #:use-module ((systemic home base) #:prefix base:)
  #:use-module (systemic home bibliography)
  #:use-module (systemic home mail))

(define-public environment
  (home-environment
   (services
    (cons*
     (simple-service 'syncthing
                     home-shepherd-service-type
                     (list (shepherd-service
                            (documentation "Run Syncthing.")
                            (provision '(syncthing))
                            (start #~(make-forkexec-constructor '("syncthing"
                                                                  "-no-browser"
                                                                  "-no-restart")))
                            (stop #~(make-kill-destructor)))))
     (service systemic-mail-service-type)
     (service bibliography-service-type
              (bibliography-configuration
               (bibtex-file "~/org/references.bib")
               (csl-directory "~/org/csl")))
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
