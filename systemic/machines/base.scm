(define-module (systemic machines base)
  #:use-module (dwl-guile home-service)
  #:use-module (dwl-guile packages)
  #:use-module (dwl-guile patches)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services password-utils)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services cups)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services networking)
  #:use-module (gnu services sound)
  #:use-module (gnu services ssh)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system nss)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux)
  #:use-module (systemic channels)
  #:use-module (systemic home git)
  #:use-module (systemic home scheme)
  #:use-module ((systemic home clojure) #:prefix clojure:)
  #:use-module ((systemic home desktop) #:prefix desktop:)
  #:use-module ((systemic home emacs) #:prefix emacs:)
  #:use-module ((systemic home haskell) #:prefix haskell:)
  #:use-module ((systemic home idris) #:prefix idris:)
  #:use-module ((systemic home javascript) #:prefix javascript:)
  #:use-module ((systemic home kotlin) #:prefix kotlin:))

(define-public home
  (home-environment
   (services
    (append
     (list
      (service home-bash-service-type (home-bash-configuration))
      (service home-gnupg-service-type
	       (home-gnupg-configuration
                (gpg-agent-config
                 (home-gpg-agent-configuration
                  (ssh-agent? #t)
                  (pinentry-flavor 'bemenu)))))
      (service
       home-ssh-service-type
       (home-ssh-configuration                
        (toplevel-options
         '((match .
             "host * exec \"gpg-connect-agent UPDATESTARTUPTTY /bye\"")))))
      (service scheme-service-type)
      (simple-service
       'some-useful-env-vars-service
       home-environment-variables-service-type
       `(("GUILE_LOAD_PATH" .
          "$XDG_CONFIG_HOME/guix/current/share/guile/site/3.0:$GUILE_LOAD_PATH")
         ("GUILE_LOAD_COMPILED_PATH" .
          "$XDG_CONFIG_HOME/guix/current/lib/guile/3.0/site-ccache:$GUILE_LOAD_COMPILED_PATH")
         ("GUILE_AUTO_COMPILE" . "0")
         ("CC" . "gcc")
         ("PATH" . "$HOME/.local/bin:$HOME/.local/share/flatpak/exports/bin/:$PATH")
         ;; HACK: https://issues.guix.gnu.org/52672
         ("QTWEBENGINE_CHROMIUM_FLAGS" . "--disable-seccomp-filter-sandbox")))
      (service systemic-git-service-type))
     desktop:services
     clojure:services
     haskell:services
     idris:services
     javascript:services
     kotlin:services
     emacs:services))))

(define-public system
  (operating-system
    (kernel linux)
    (firmware (list linux-firmware))
    (host-name "base")
    (timezone "America/New_York")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))
    (skeletons (channel-skel (default-skeletons)))
    
    (users (cons*
            (user-account
             (name "reily")
             (comment "Reily")
             (group "users")
             (home-directory "/home/reily")
             (supplementary-groups
              '("wheel" "netdev" "audio" "video" "input")))
            %base-user-accounts))
    (packages
     (append
      (list (specification->package "mesa"))
      %base-packages))
    (services
     (append
      (list
       ;; Needed for emacs-password-store-otp
       (simple-service 'zbar-dbus-service dbus-root-service-type (list zbar))
       (service openssh-service-type)
       (service network-manager-service-type)
       (service wpa-supplicant-service-type)
       (service modem-manager-service-type)
       (service usb-modeswitch-service-type)
       (service ntp-service-type)
       (service elogind-service-type)
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
                                   hplip-minimal)))))
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
    (file-systems
     (cons* (file-system
              (device (file-system-label "btrfs-pool"))
              (mount-point "/")
              (type "btrfs")
              (options "subvol=rootfs"))
            (file-system
              (device (file-system-label "efi"))
              (mount-point "/boot/efi")
              (type "vfat"))
            %base-file-systems))))
