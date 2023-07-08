(define-module (systemic home desktop)
  #:use-module (dtao-guile home-service)
  #:use-module (dwl-guile home-service)
  #:use-module (dwl-guile packages)
  #:use-module (dwl-guile patches)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
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
                (set-monitor-rules '((scale . 2)))
                
                (set-xkb-rules '((options . "ctrl:nocaps")))

                (setq natural-scrolling? #t
                      tap-to-click? #f
                      tap-and-drag? 0)
                
                (dwl:set-tty-keys "C-M")
                (dwl:set-tag-keys "s" "s-S")

                (set-keys
                 "s-d" ,(guix:spawn bemenu "/bin/bemenu-run")
                 "s-<return>" ,(guix:spawn emacs-next-pgtk "/bin/emacs")
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
                 ,(guix:spawn light "/bin/light" "-U" "10")
                 "<XF86MonBrightnessUp>"
                 ,(guix:spawn light "/bin/light" "-A" "10"))))))))
(define %time-block
  (dtao-block
   (interval 1)
   (render `(strftime "%A, %d %b %T" (localtime (current-time))))))

(define %battery-block
  (dtao-block
   (interval 5)
   (render
    `(string-append
      (call-with-input-file
          "/sys/class/power_supply/BAT0/capacity" get-line)
      "% "
      (call-with-input-file
          "/sys/class/power_supply/BAT0/status" get-line)))))

(define dtao-guile-service
  (service home-dtao-guile-service-type
           (home-dtao-guile-configuration
            (auto-start? #t)
            (config
             (dtao-config
              (font "Hack:size=14")
              (block-spacing 8)
              (modules (list '(ice-9 textual-ports)))
              (right-blocks
               (list %battery-block %time-block)))))))

(define services
  (list dwl-guile-service
        dtao-guile-service
        (simple-service
         'flatpak-profile home-profile-service-type (list flatpak))
        (simple-service
         'flatpak-env home-environment-variables-service-type
         '(("XDG_DATA_DIRS" .
            "$HOME/.local/share/flatpak/exports/share:$XDG_DATA_DIRS")))))
