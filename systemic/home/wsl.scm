(define-module (systemic home wsl)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home-services version-control)
  #:use-module (gnu home services xdg)
  #:use-module (gnu services)
  #:use-module ((systemic home base) #:prefix base:)
  #:use-module (systemic home git)
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
       `(("DISPLAY" . ":0.0")))
      (simple-service
       'portable-git
       home-git-service-type
       (home-git-extension
        (config
         `((core
            ((checkStat . "minimal")
             (trustctime . #f)
             (filemode . #f))))))))
     csharp:services
     javascript:services
     (modify-services (home-environment-user-services base:environment)
       (systemic-git-service-type
        config =>
        '("Reily Siegel" "rsiegel@alarm.com" #f)))))))
