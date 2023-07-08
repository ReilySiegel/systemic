(define-module (systemic machines base)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services cups)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services sound)
  #:use-module (gnu services xorg)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system nss)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux))

(define-public base-operating-system
  (operating-system
    (kernel linux)
    (firmware (list linux-firmware))
    
    (host-name "base")
    (timezone "America/New_York")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))
    
    (users (cons*
            (user-account
             (name "reily")
             (comment "Reily")
             (group "users")
             (home-directory "/home/reily")
             (supplementary-groups
              '("wheel" "netdev" "audio" "video" "input" "docker")))
            %base-user-accounts))
    (packages
     (append
      (list
       (with-emacs-next (specification->package "emacs-exwm"))
       (specification->package "sway")
       (specification->package "nss-certs")
       (specification->package "pulseaudio")
       (specification->package "mesa")
       (specification->package "alsa-utils"))
      %base-packages))
    (services
     (append
      (list
       ;; Needed for emacs-password-store-otp
       (simple-service 'zbar-dbus-service dbus-root-service-type (list zbar))
       (service docker-service-type)
       (service openssh-service-type)
       (service network-manager-service-type)
       (service wpa-supplicant-service-type)
       (service modem-manager-service-type)
       (service usb-modeswitch-service-type)
       (service ntp-service-type)
       (service elogind-service-type)
       (service pulseaudio-service-type)
       (service alsa-service-type)
       (service bluetooth-service-type
                (bluetooth-configuration
                 (auto-enable? #t)))
       (service cups-service-type
                (cups-configuration
                 (web-interface? #t)
                 (max-clients 1000)
                 (timeout 10)
                 (extensions (list cups-filters
                                   epson-inkjet-printer-escpr
                                   hplip-minimal))))
       ;; (service greetd-service-type
       ;;          (greetd-configuration
       ;;           (greeter-supplementary-groups (list "video" "input" "seat"))
       ;;           (terminals
       ;;            (list (greetd-terminal-configuration
       ;;                   (terminal-vt "1")
       ;;                   (terminal-switch #t)
       ;;                   (default-session-command
       ;;                     (greetd-wlgreet-sway-session)))))))
       ;; (service slim-service-type
       ;;                 (slim-configuration
       ;;                  (xorg-configuration
       ;;                   (xorg-configuration
       ;;                    (modules (delq (specification->package "xf86-input-synaptics")
       ;;                                   %default-xorg-modules))
       ;;                    (drivers '("modesetting" "intel"))
       ;;                    (keyboard-layout keyboard-layout)
       ;;                    (extra-config '("
       ;; Section \"InputClass\"
       ;; Identifier \"devname\"
       ;; MatchIsTouchpad \"on\"
       ;; Driver \"libinput\"
       ;; Option \"NaturalScrolling\" \"true\"
       ;; EndSection"))))))
       )
      (modify-services %base-services
        (guix-service-type
         config =>
         (guix-configuration
          (inherit config)
          (substitute-urls
           (append (list "https://substitutes.nonguix.org")
                   %default-substitute-urls))
          (authorized-keys
           (append (list (plain-file "nonguix.pub"
                                     "(public-key 
 (ecc 
  (curve Ed25519)
  (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))"))
                   %default-authorized-guix-keys)))))))
    (name-service-switch %mdns-host-lookup-nss)
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))))
    (file-systems (cons*
                   (file-system
                     (mount-point "/")
                     (device "none")
                     (type "tmpfs")
                     (check? #f))
                   %base-file-systems))))
base-operating-system
