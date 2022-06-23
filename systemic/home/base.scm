(define-module (systemic home base)
  #:use-module (gnu home)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services)
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
      (service systemic-git-service-type)
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
         ;; HACK: https://issues.guix.gnu.org/52672
         ("QTWEBENGINE_CHROMIUM_FLAGS" . "--disable-seccomp-filter-sandbox"))))))))
