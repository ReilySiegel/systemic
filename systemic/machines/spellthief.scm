(define-module (systemic machines spellthief)
  #:use-module (dwl-guile home-service)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python-web)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services pm)
  #:use-module (gnu system)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (systemic home mail)
  #:use-module ((systemic machines base) #:prefix base:)
  #:use-module (systemic packages cyrus-sasl)
  #:use-module (systemic system vpn))

(define-public system
  (operating-system
    (inherit base:system)
    (host-name "spellthief")
    (users (cons*
            (user-account
             (name "rsiegel")
             (comment "Reily")
             (group "users")
             (home-directory "/home/reily")
             (supplementary-groups
              '("wheel" "netdev" "audio" "video" "input" "docker")))
            %base-user-accounts))

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
      (operating-system-user-services base:system)))))

(define-public home
  (home-environment
   (inherit base:home)
   (services
    (cons*
     (simple-service 'cyrus-xoauth2 home-profile-service-type
                     (list cyrus-sasl cyrus-sasl-xoauth2))
     (service systemic-mail-service-type
              (systemic-mail-configuration
               (address "rsiegel@wpi.edu")
               (imap "outlook.office365.com")
               (smtp "smtp.office365.com")
               (auth-mechs "XOAUTH2")
               (secret #~#$(file-append oauth2ms "/bin/oauth2ms"))))
     (simple-service
      'laptop
      home-dwl-guile-service-type
      '((setq natural-scrolling? #t
              tap-to-click? #f
              tap-and-drag? 0)))
     (home-environment-user-services base:home)))))
