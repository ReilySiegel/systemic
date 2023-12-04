(define-module (systemic machines luden)
  #:use-module (dwl-guile home-service)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services pm)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (guix gexp)
  #:use-module (systemic home mail)
  #:use-module ((systemic machines base) #:prefix base:)
  #:use-module (systemic pass)
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
      (systemic-wireguard-service "10.10.50.1")
      (operating-system-user-services base:system)))))

(define-public home
  (home-environment
   (inherit base:home)
   (services
    (cons*
     (simple-service 'zoho-pass home-activation-service-type
                     (pass-activation "zoho.com" "isync/secret"))
     (service systemic-mail-service-type
              (systemic-mail-configuration
               (address "mail@reilysiegel.com")
               (imap "imap.zoho.com")
               (smtp "smtppro.zoho.com")
               (secret #~(string-append
                          #$coreutils
                          "/bin/cat"
                          " $XDG_STATE_HOME/isync/secret"))
               (notmuch-tags
                '( ;; Tag messages with unsubscribe options as promotional
                  ("unsubscribe" ("+promotional" "-inbox"))
                  ;; Tag messages from noreply as automated
                  ;; Use reply to catch all variations
                  ("from:reply" ("+automated" "-inbox"))
                  ;; Messages to mailing lists should not be in inbox unless
                  ;; they are to me
                  ("tag:lists AND NOT tag:to-me" ("-inbox"))))))
     (simple-service
      'laptop
      home-dwl-guile-service-type
      `((set-monitor-rules '((scale . ,(/ 2256 1920))))
        (setq natural-scrolling? #t
              tap-to-click? #f
              tap-and-drag? 0)))
     (home-environment-user-services base:home)))))
