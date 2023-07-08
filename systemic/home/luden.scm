(define-module (systemic home luden)
  #:use-module (dwl-guile home-service)
  #:use-module (dwl-guile packages)
  #:use-module (dwl-guile patches)
  #:use-module (gnu home services)
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
  #:use-module ((systemic home desktop) #:prefix desktop:)
  #:use-module ((systemic home base) #:prefix base:)
  #:use-module (systemic home bibliography)
  #:use-module ((systemic home clojure) #:prefix clojure:)
  #:use-module (systemic home mail)
  #:use-module ((systemic home javascript) #:prefix javascript:)
  #:use-module ((systemic home kotlin) #:prefix kotlin:)
  #:use-module (systemic home scheme))

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
      (service home-gnupg-service-type
	       (home-gnupg-configuration
                (gpg-agent-config
                 (home-gpg-agent-configuration
                  (ssh-agent? #t)
                  (pinentry-flavor 'bemenu)))))
      (service home-ssh-service-type
               (home-ssh-configuration                
                (toplevel-options
                 '((match .
                     "host * exec \"gpg-connect-agent UPDATESTARTUPTTY /bye\"")))))
      (service scheme-service-type)
      (simple-service
       'systemic-channel
       home-channels-service-type
       (list
        (channel
         (name 'systemic)
         (url "https://github.com/ReilySiegel/systemic")
         (introduction
          (make-channel-introduction
           "89ff5b4374e472194eff08f2a69153d5cde6784e"
           (openpgp-fingerprint
            "0FA2 FE4C 164F 60C6 7F6B  EA7E 508A 5AD0 A50F 88AF"))))))
      (simple-service
       'some-useful-env-vars-service
       home-environment-variables-service-type
       `(("GUILE_LOAD_PATH" .
          "$XDG_CONFIG_HOME/guix/current/share/guile/site/3.0:$GUILE_LOAD_PATH")
         ("GUILE_LOAD_COMPILED_PATH" .
          "$XDG_CONFIG_HOME/guix/current/lib/guile/3.0/site-ccache:$GUILE_LOAD_COMPILED_PATH")
         ("GUILE_AUTO_COMPILE" . "0")
         ("PATH" . "$HOME/.local/bin:$PATH")
         ;; HACK: https://issues.guix.gnu.org/52672
         ("QTWEBENGINE_CHROMIUM_FLAGS" . "--disable-seccomp-filter-sandbox"))))
     desktop:services
     clojure:services
     javascript:services
     kotlin:services
     (home-environment-user-services base:environment)))))
