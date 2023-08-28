(define-module (systemic system vpn)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu services)
  #:use-module (gnu services vpn)
  #:export (systemic-wireguard-service))

(define (systemic-wireguard-service ip)
  (service wireguard-service-type
           (wireguard-configuration
            (addresses (list ip))
            (dns '("10.10.0.1"))
            (interface "bard")
            (peers
             (list
              (wireguard-peer
               (name "bard")
               (endpoint "bard.reilysiegel.com:51820")
               (public-key "uyhDxjDWrNP/HAcImeFWfeLHk4Io3nPnyY6lvQFoph8=")
               (allowed-ips (list "10.10.0.0/16"))))))))
