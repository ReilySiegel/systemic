(define-module (systemic machines luden)
  #:use-module (dwl-guile home-service)
  #:use-module (gnu home)
  #:use-module (gnu home services)
  #:use-module (gnu home services syncthing)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu services pm)
  #:use-module (gnu services vpn)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (guix gexp)
  #:use-module (nongnu packages linux)
  #:use-module (systemic home mail)
  #:use-module ((systemic machines base) #:prefix base:)
  #:use-module (systemic pass))

(define-public system
  (operating-system
    (inherit base:system)
    (kernel (corrupt-linux linux-libre #:configs '("CONFIG_MT7921E=m")))
    
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
      (service wireguard-service-type
               (wireguard-configuration
                (addresses '("192.168.50.18"))
                (dns '("192.168.10.1"))
                (interface "crowlab")
                (peers
                 (list
                  (wireguard-peer
                   (name "bard")
                   (endpoint "ext.axpdsp.org:51820")
                   (public-key "B3ksprX400fUNk2iMjsWic7IniUb8l12pCMaKeMqn3M=")
                   (allowed-ips (list "192.168.10.0/24"
                                      "192.168.20.0/24"
                                      "192.168.30.0/24"
                                      "192.168.40.0/24")))))))
      (operating-system-user-services base:system)))))

(define-public home
  (home-environment
   (inherit base:home)
   (services
    (cons*
     (simple-service 'zoho-pass home-activation-service-type
                     (pass-activation "zoho.com" "isync/secret"))
     (service home-syncthing-service-type)
     (service systemic-mail-service-type
              (systemic-mail-configuration
               (address "mail@reilysiegel.com")
               (imap "imap.zoho.com")
               (smtp "smtp.zoho.com")
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
