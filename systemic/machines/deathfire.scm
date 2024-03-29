(define-module (systemic machines deathfire)
  #:use-module (gnu home)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services desktop)
  #:use-module (gnu system pam)
  #:use-module (gnu services pm)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module ((systemic machines base) #:prefix base:))

(define-public system
  (operating-system
    (inherit base:system)
    (host-name "deathfire")
    (services
     (cons*
      (service pam-limits-service-type
               ;; For Lutris / Wine esync
               (list (pam-limits-entry "*" 'hard 'nofile 524288)))
      (operating-system-user-services base:system)))))

(define-public home
  (home-environment
   (inherit base:home)))
