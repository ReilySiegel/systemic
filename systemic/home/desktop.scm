(define-module (systemic home desktop)
  #:use-module (dtao-guile home-service)
  #:use-module (dwl-guile home-service)
  #:use-module (dwl-guile packages)
  #:use-module (dwl-guile patches)
  #:use-module (gnu home services)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services sound)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (services))

(define (guix:spawn pkg path . args)
  `(quote ,(cons* 'dwl:spawn (file-append pkg path) args)))

(define dwl-guile-service
  (service home-dwl-guile-service-type
           (home-dwl-guile-configuration
            (package
              (patch-dwl-guile-package
               dwl-guile
               #:patches
               (list %patch-xwayland)))
            (auto-start? #t)
            (environment-variables
             (alist-delete "GDK_BACKEND" %dwl-guile-base-env-variables))
            (config
             `(((setq inhibit-defaults? #t)
                (dwl:start-repl-server)
                
                (set-xkb-rules '((options . "ctrl:nocaps")))
                (setq accel-profile ACCEL-PROFILE-FLAT)
                
                (dwl:set-tty-keys "C-M")
                (dwl:set-tag-keys "s" "s-S")

                (set-keys
                 "s-e" ,(guix:spawn emacs-next-pgtk "/bin/emacs")
                 "s-<return>" ,(guix:spawn bemenu "/bin/bemenu-run" "-i")
                 "s-n" ,(guix:spawn mako "/bin/makoctl" "dismiss")
                 "s-j" '(dwl:focus-stack 1)
                 "s-k" '(dwl:focus-stack -1)
                 "s-l" '(dwl:change-master-factor 0.05)
                 "s-h" '(dwl:change-master-factor -0.05)
                 "s-S-j" '(dwl:change-masters 1)
                 "s-S-k" '(dwl:change-masters -1)
                 "s-t" '(dwl:cycle-layout 1)
                 "s-<left>" '(dwl:focus-monitor 'DIRECTION-LEFT)
                 "s-<right>" '(dwl:focus-monitor 'DIRECTION-RIGHT)
                 "s-<up>" '(dwl:focus-monitor 'DIRECTION-UP)
                 "s-<down>" '(dwl:focus-monitor 'DIRECTION-DOWN)
                 "s-S-<left>" '(dwl:tag-monitor 'DIRECTION-LEFT)
                 "s-S-<right>" '(dwl:tag-monitor 'DIRECTION-RIGHT)
                 "s-S-<up>" '(dwl:tag-monitor 'DIRECTION-UP)
                 "s-S-<down>" '(dwl:tag-monitor 'DIRECTION-DOWN)
                 "s-q" 'dwl:kill-client
                 "s-<space>" 'dwl:zoom
                 "s-<tab>" 'dwl:view
                 "s-S-0" '(dwl:view 0) ;; 0 will show all tags
                 "s-f" 'dwl:toggle-fullscreen
                 "S-s-<space>" 'dwl:toggle-floating
                 "S-s-<escape>" 'dwl:quit
                 "s-<mouse-left>" 'dwl:move
                 "s-<mouse-middle>" 'dwl:toggle-floating
                 "s-<mouse-right>" 'dwl:resize
                 "<XF86MonBrightnessDown>"
                 ,(guix:spawn light "/bin/light" "-U" "5")
                 "<XF86MonBrightnessUp>"
                 ,(guix:spawn light "/bin/light" "-A" "5")
                 "<XF86AudioLowerVolume>"
                 ,(guix:spawn pamixer "/bin/pamixer" "-d" "5")
                 "<XF86AudioRaiseVolume>"
                 ,(guix:spawn pamixer "/bin/pamixer" "-i" "5"))))))))
(define %time-block
  (dtao-block
   (interval 1)
   (render `(strftime "%A, %d %b %T" (localtime (current-time))))))

(define %battery-block
  (dtao-block
   (interval 5)
   (render
    `(let ((path (first
                  (filter file-exists?
                          (map (cut string-append "/sys/class/power_supply/" <>)
                               (list "BAT0" "BAT1"))))))
       (or (when path
             (string-append
              (call-with-input-file
                  (string-append path "/capacity") get-line)
              "% "
              (call-with-input-file
                  (string-append path "/status") get-line)))
           "")))))

(define dtao-guile-service
  (service home-dtao-guile-service-type
           (home-dtao-guile-configuration
            (auto-start? #t)
            (config
             (dtao-config
              (font "Hack:size=18")
              (block-spacing 8)
              (modules (list '(ice-9 textual-ports)
                             '(ice-9 rdelim)
                             '(ice-9 popen)
                             '(srfi srfi-1)
                             '(srfi srfi-26)))
              (right-blocks
               (list %battery-block %time-block)))))))

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
                    (requirement '(dbus dwl-guile))
                    (start #~(make-forkexec-constructor
                              (list (string-append #$mako "/bin/mako")
                                    "--ignore-timeout" "1")))
                    (stop #~ (make-kill-destructor))))))))
   (description "Mako notfification daemon.")))

(define services
  (list dwl-guile-service
        dtao-guile-service
        (service home-dbus-service-type)
        (service home-pipewire-service-type)
        (service mako-service-type)
        (simple-service
         'flatpak-profile home-profile-service-type (list flatpak))
        (simple-service
         'flatpak-env home-environment-variables-service-type
         '(("XDG_DATA_DIRS" .
            "$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS")))))
