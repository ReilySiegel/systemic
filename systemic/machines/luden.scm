(define-module (systemic machines luden)
  #:use-module (gnu home)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services pm)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module ((systemic home) #:prefix base:)
  #:use-module ((systemic system) #:prefix base:)
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
      (operating-system-user-services base:system)))
    (file-systems
     (cons* (file-system
              (mount-point "/")
              (device
               (uuid "6fad59fe-c558-4443-9662-16d3ec2fa863"
                     'btrfs))
              (type "btrfs"))
            (file-system
              (mount-point "/boot/efi")
              (device (uuid "1F04-0E57" 'fat32))
              (type "vfat"))
            %base-file-systems))))

(define-public home
  (home-environment
   (inherit base:home)))
