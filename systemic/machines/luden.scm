(define-module (systemic machines luden)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services pm)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (systemic machines base))

(define-public luden-operating-system
  (operating-system
   (inherit base-operating-system)
   (host-name "luden")
   (timezone "Europe/Copenhagen")
   (services
    (append
     (list
      (udev-rules-service 'backlight brightnessctl)
      (service tlp-service-type
               (tlp-configuration
                (tlp-default-mode "BAT")
                (cpu-scaling-governor-on-ac '("performance"))
                (cpu-scaling-governor-on-bat '("powersave"))
                (cpu-boost-on-ac? #t)
                (sched-powersave-on-bat? #t))))
     (modify-services (operating-system-user-services base-operating-system)
                      ;; TODO: Auto-power off on low battery
                      (elogind-service-type
                       config =>
                       (elogind-configuration
                        (inherit config)
                        (handle-power-key 'suspend)
                        ;; FIXME: Laptop always reports OnExternalPower=yes
                        (handle-lid-switch-external-power 'suspend)
                        (idle-action 'suspend)
                        (idle-action-seconds (* 5 60)))))))
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
