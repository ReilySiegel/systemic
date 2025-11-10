(define-module (systemic home desktop)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services niri)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages music)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu system keyboard)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (services))

(define mako-service-type
  (service-type
   (name 'mako)
   (default-value #f)
   (extensions
    (list (service-extension
           home-shepherd-service-type
           (lambda (_)
             (list (shepherd-service
                    (provision '(mako))
                    (requirement '(dbus niri))
                    (start #~(make-forkexec-constructor
                              (list (string-append #$mako "/bin/mako")
                                    "--ignore-timeout" "1")))
                    (stop #~(make-kill-destructor))))))))
   (description "Mako notfification daemon.")))

(define services
  (list 
   (service home-niri-service-type)
   (service home-dbus-service-type)
   (service home-pipewire-service-type)
   (service mako-service-type)
   (simple-service
    'flatpak-profile home-profile-service-type (list flatpak))
   (simple-service
    'desktop-profile home-profile-service-type
    (list swayidle swaylock fuzzel waybar wireplumber brightnessctl
          playerctl font-awesome
          xdg-desktop-portal-gtk xdg-desktop-portal-gnome))
   (simple-service
    'flatpak-env home-environment-variables-service-type
    '(("XDG_DATA_DIRS" .
       "$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS")))))
