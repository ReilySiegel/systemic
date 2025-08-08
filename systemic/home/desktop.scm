(define-module (systemic home desktop)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services sway)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
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
                    (requirement '(dbus sway))
                    (start #~(make-forkexec-constructor
                              (list (string-append #$mako "/bin/mako")
                                    "--ignore-timeout" "1")))
                    (stop #~(make-kill-destructor))))))))
   (description "Mako notfification daemon.")))

(define services
  (list 
   (service home-sway-service-type
            (sway-configuration
             (inputs
              (list
               (sway-input (identifier "type:keyboard")
                           (layout
                            (keyboard-layout "us" #:options '("ctrl:nocaps"))))
               (sway-input (identifier "type:touchpad")
                           (extra-content (list "natural_scroll enabled")))))
             (outputs
              (list (sway-output
                     (identifier '*)
                     (background (file-append sway "/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png"))
                     (extra-content (list "scale 1.5")))))
             (extra-content
              (list "default_border none"))))
   (service home-dbus-service-type)
   (service home-pipewire-service-type)
   (service mako-service-type)
   (simple-service 'sway-launch
                   home-shepherd-service-type
                   (list
                    (shepherd-service
                     (provision '(sway))
                     (requirement '(dbus))
                     (start #~(make-forkexec-constructor
                               (list (string-append #$sway "/bin/sway"))))
                     (stop #~ (make-kill-destructor)))))
   (simple-service
    'flatpak-profile home-profile-service-type (list flatpak))
   (simple-service
    'flatpak-env home-environment-variables-service-type
    '(("XDG_DATA_DIRS" .
       "$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS")))))
