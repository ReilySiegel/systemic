;; This is an operating system configuration generated
;; by the graphical installer.

(define-module (system))

(use-modules (gnu)
             (gnu packages linux)
             (gnu packages cups)
             (gnu system nss)
             (guix channels)
             (guix packages)
             (guix inferior)
	     (nongnu packages linux)
             (nongnu system linux-initrd)
             (srfi srfi-1))
(use-service-modules desktop cups pm networking ssh xorg)

(operating-system
  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))
  (locale "en_US.utf8")
  (timezone "America/New_York")
  (keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))
  (host-name "reilysiegel-laptop")
  (name-service-switch %mdns-host-lookup-nss)
  (users (cons* (user-account
                 (name "reily")
                 (comment "Reily")
                 (group "users")
                 (home-directory "/home/reily")
                 (supplementary-groups
                  '("wheel" "netdev" "audio" "video" "input")))
                %base-user-accounts))
  (packages
   (append
       (list (specification->package "emacs")
             (specification->package "emacs-exwm")
             (specification->package "nss-certs")
             (specification->package "pulseaudio")
             (specification->package "mesa")
             (specification->package "alsa-utils"))
       %base-packages))
  (services
   (append
       (list
        (service slim-service-type
                 (slim-configuration
                  (xorg-configuration
                   (xorg-configuration
                    (modules (delq (specification->package "xf86-input-synaptics")
                                   %default-xorg-modules))
                    (drivers '("modesetting" "intel"))
                    (keyboard-layout keyboard-layout)
                    (extra-config '("
Section \"InputClass\"
Identifier \"devname\"
MatchIsTouchpad \"on\"
Driver \"libinput\"
Option \"NaturalScrolling\" \"true\"
EndSection"))))))
        (service openssh-service-type)
        (udev-rules-service 'backlight brightnessctl)
        (service bluetooth-service-type
                 (bluetooth-configuration
                  (auto-enable? #t)))
        (service tlp-service-type)
        (service cups-service-type
                 (cups-configuration
                  (web-interface? #t)
                  (max-clients 1000)
                  (timeout 10)
                  (extensions (list cups-filters
                                    epson-inkjet-printer-escpr
                                    hplip-minimal)))))
       (modify-services %desktop-services
         (delete gdm-service-type)
         (elogind-service-type
          config =>
          (elogind-configuration
           (inherit config)
           (handle-power-key 'suspend)
           ;; FIXME: Laptop always reports OnExternalPower=yes
           (handle-lid-switch-external-power 'suspend)
           (idle-action 'suspend)
           (idle-action-seconds (* 5 60))))
         ;; TODO: Auto-power off on low battery
         (tlp-service-type
          config =>
          (tlp-service-configuration
           (inherit config)
           (tlp-default-mode "BAT")
           (cpu-scaling-governor-on-ac "performance")
           (cpu-scaling-governor-on-bat "powersave")
           (cpu-boost-on-ac? #t)
           (sched-powersave-on-bat? #t)))
         (guix-service-type
          config =>
          (guix-configuration
           (inherit config)
           (substitute-urls
            (append %default-substitute-urls
                (list "https://mirror.brielmaier.net")))
           (authorized-keys
            (append (list (local-file "mirror.brielmaier.net.pub"))
                %default-authorized-guix-keys)))))))
  (bootloader
   (bootloader-configuration
    (bootloader grub-efi-bootloader)
    (targets '("/boot/efi"))
    (keyboard-layout keyboard-layout)))
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
          %base-file-systems)))
