(define-module (systemic machines luden)
  #:use-module (dwl-guile home-service)
  #:use-module (gnu home)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services pm)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (systemic home bibliography)
  #:use-module (systemic home mail)
  #:use-module ((systemic machines base) #:prefix base:)
  #:use-module (systemic system vpn))

(define-public system
  (operating-system
    (inherit base:system)
    (host-name "luden")
    (services
     (cons*
      (udev-rules-service 'backlight brightnessctl)
      (service tlp-service-type
               (tlp-configuration
                (tlp-default-mode "BAT")
                (cpu-scaling-governor-on-ac '("performance"))
                (cpu-scaling-governor-on-bat '("powersave"))
                (cpu-boost-on-ac? #t)
                (sched-powersave-on-bat? #t)))
      (service netbird-service-type)
      (operating-system-user-services base:system)))))

(define-public home
  (home-environment
   (inherit base:home)
   (services
    (cons*
     (service systemic-mail-service-type)
     (service bibliography-service-type
              (bibliography-configuration
               (bibtex-file "~/org/references.bib")
               (csl-directory "~/org/csl")))
     (simple-service
      'laptop
      home-dwl-guile-service-type
      `((set-monitor-rules '((scale . ,(/ 3000 1920))))
        (setq natural-scrolling? #t
              tap-to-click? #f
              tap-and-drag? 0)))
     (home-environment-user-services base:home)))))
