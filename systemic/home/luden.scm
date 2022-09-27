(define-module (systemic home luden)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services mail)
  #:use-module (gnu home-services password-utils)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (systemic home desktop)
  #:use-module ((systemic home base) #:prefix base:)
  #:use-module (systemic home bibliography)
  #:use-module ((systemic home clojure) #:prefix clojure:)
  #:use-module (systemic home mail)
  #:use-module ((systemic home javascript) #:prefix javascript:)
  #:use-module ((systemic home kotlin) #:prefix kotlin:))

(define-public environment
  (home-environment
   (services
    (append
     (list
      (simple-service 'syncthing
                      home-shepherd-service-type
                      (list (shepherd-service
                             (documentation "Run Syncthing.")
                             (provision '(syncthing))
                             (start #~(make-forkexec-constructor '("syncthing"
                                                                   "-no-browser"
                                                                   "-no-restart")))
                             (stop #~(make-kill-destructor)))))
      (service home-bash-service-type (home-bash-configuration))
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
                (default-options
                  '((forward-x11 . #t)
                    (forward-x11-trusted . #t)))
                (toplevel-options
                 '((match .
                     "host * exec \"gpg-connect-agent UPDATESTARTUPTTY /bye\"")))))
      (simple-service
       'systemic-channel
       home-channels-service-type
       (list
        (channel
         (name 'systemic)
         (url "file:///home/reily/src/systemic")
         (introduction
          (make-channel-introduction
           "89ff5b4374e472194eff08f2a69153d5cde6784e"
           (openpgp-fingerprint
            "0FA2 FE4C 164F 60C6 7F6B  EA7E 508A 5AD0 A50F 88AF")))))))
     clojure:services
     javascript:services
     kotlin:services
     (home-environment-user-services base:environment)))))
