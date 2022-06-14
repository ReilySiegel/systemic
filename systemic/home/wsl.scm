(define-module (systemic home wsl)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)
  #:use-module ((systemic home base) #:prefix base:)
  #:use-module ((systemic home javascript) #:prefix javascript:))

(define-public environment
  (home-environment
   (services
    (append
     (list
      (simple-service
       'runtime-dir
       home-xdg-base-directories-service-type
       (home-xdg-base-directories-configuration
        (runtime-dir "${XDG_RUNTIME_DIR:-/tmp/run/$UID}")))
      (simple-service
       'windows-x-forwarding
       home-environment-variables-service-type
       `(("DISPLAY" . ":0.0"))))
     javascript:services
     (home-environment-user-services base:environment)))))
