(define-module (systemic home luden)
  #:use-module (gnu home)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services mail)
  #:use-module (gnu home-services password-utils)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages pulseaudio)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-26)
  #:use-module (systemic home bibliography)
  #:use-module (systemic home desktop)
  #:use-module ((systemic home emacs) #:prefix emacs:)
  #:use-module (systemic home git)
  #:use-module (systemic home mail))

(define-public reily-home-environment
  (home-environment
   (services
    (append
     emacs:services
     (list
      (service picom-service-type #f)
      (service exwm-service-type)
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
      (service home-gnupg-service-type
	       (home-gnupg-configuration
                (gpg-agent-config
                 (home-gpg-agent-configuration
                  (ssh-agent? #t)
                  (pinentry-flavor 'emacs)))))
      (service systemic-git-service-type)
      (service home-ssh-service-type
               (home-ssh-configuration
                (toplevel-options
                 '((match .
                     "host * exec \"gpg-connect-agent UPDATESTARTUPTTY /bye\"")))))
      (service home-bash-service-type
               (home-bash-configuration))
      (simple-service
       'some-useful-env-vars-service
       home-environment-variables-service-type
       `(("GUILE_LOAD_PATH" .
          "$XDG_CONFIG_HOME/guix/current/share/guile/site/3.0:$GUILE_LOAD_PATH")
         ("GUILE_LOAD_COMPILED_PATH" .
          "$XDG_CONFIG_HOME/guix/current/lib/guile/3.0/site-ccache:$GUILE_LOAD_COMPILED_PATH")
         ("GUILE_AUTO_COMPILE" . "0")
         ("PATH" . "$HOME/.local/bin:$PATH")
         ("npm_config_prefix" . "$HOME/.local")
         ("NODE_PATH" . "$HOME/.local/lib/node_modules:$NODE_PATH")
         ;; HACK: https://issues.guix.gnu.org/52672
         ("QTWEBENGINE_CHROMIUM_FLAGS" . "--disable-seccomp-filter-sandbox"))))))))
