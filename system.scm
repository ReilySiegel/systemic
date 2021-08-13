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
 (kernel (let*
             ((channels
               (list (channel
                      (name 'nonguix)
                      (url "https://gitlab.com/nonguix/nonguix")
                      (commit "d81564f21e7d8800e6f6187fe2e1f6476e06bc30"))
                     (channel
                      (name 'guix)
                      (url "https://git.savannah.gnu.org/git/guix.git")
                      (commit "299c3c18603f4f92f187908ad48eeb6e5b3b6630"))))
              (inferior
               (inferior-for-channels channels)))
           (first (lookup-inferior-packages inferior "linux" "5.12.9"))))
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
              (extensions
               (list cups-filters epson-inkjet-printer-escpr hplip-minimal))))
    (set-xorg-configuration
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
EndSection")))))
   (modify-services %desktop-services
                    (elogind-service-type
                     config =>
                     (elogind-configuration
                      (handle-power-key 'suspend)
                      (idle-action 'suspend)
                      (idle-action-seconds (* 5 60))))
                    (upower-service-type
                     config =>
                     (upower-configuration
                      (use-percentage-for-policy? #t)
                      (percentage-low 15)
                      (percentage-critical 10)
                      (percentage-action 5)
                      (critical-power-action 'power-off)))
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
   (target "/boot/efi")
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
