(define-module (systemic system vpn)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (systemic packages vpn)
  #:export (netbird-configuration
            netbird-service-type))

(define-configuration/no-serialization netbird-configuration
  (netbird (package netbird) "The netbird client package to use.")
  (management-url (string "https://api.wiretrustee.com:443")
                  "Management server url.")
  (admin-url (string "https://app.netbird.io") "Admin panel url."))

(define (netbird-shepherd-extension config)
  (let ((netbird (netbird-configuration-netbird config))
        (admin-url (netbird-configuration-admin-url config))
        (management-url (netbird-configuration-management-url config)))
    (list (shepherd-service
           (documentation "Netbird Service.")
           (provision '(netbird))
           (requirement '(networking))
           (start #~(make-forkexec-constructor
                     (list
                      (string-append #$netbird "/bin/netbird")
                      "service" "run"
                      "--admin-url" #$admin-url
                      "--management-url" #$management-url)))
           (stop #~(make-kill-destructor))))))

(define netbird-service-type
  (service-type
   (name 'netbird)
   (description "Netbird service.")
   (default-value (netbird-configuration))
   (extensions
    (list
     (service-extension shepherd-root-service-type netbird-shepherd-extension)))))
