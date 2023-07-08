(define-module (systemic machines deathfire)
  #:use-module (gnu home)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu system pam)
  #:use-module (gnu services pm)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module ((systemic home) #:prefix base:)
  #:use-module ((systemic system) #:prefix base:)
  #:use-module (systemic system vpn))

(define-public system
  (operating-system
    (inherit base:system)
    (host-name "deathfire")
    (services
     (cons*
      (service pam-limits-service-type
               ;; For Lutris / Wine esync
               (list (pam-limits-entry "*" 'hard 'nofile 524288)))
      (service netbird-service-type)
      (operating-system-user-services base:system)))
    (file-systems (cons* (file-system
                           (mount-point "/boot/efi")
                           (device (uuid "13F7-A75E"
                                         'fat32))
                           (type "vfat"))
                         (file-system
                           (mount-point "/")
                           (device (uuid
                                    "887bbf14-aca2-44cf-8ee2-f40b5112c587"
                                    'ext4))
                           (type "ext4")) %base-file-systems))))

(define-public home
  (home-environment
   (inherit base:home)))
