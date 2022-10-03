(define-module (systemic home deathfire)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home)
  #:use-module (gnu home services guix)
  #:use-module (gnu home-services gnupg)
  #:use-module (gnu home-services mail)
  #:use-module (gnu home-services password-utils)
  #:use-module (gnu home-services ssh)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix channels)
  #:use-module (systemic home desktop)
  #:use-module ((systemic home base) #:prefix base:)
  #:use-module (systemic home bibliography)
  #:use-module ((systemic home clojure) #:prefix clojure:)
  #:use-module (systemic home mail)
  #:use-module ((systemic home javascript) #:prefix javascript:))

(define-public environment
  (home-environment
   (services
    (append
     (list
      (simple-service
       'systemic-channel
       home-channels-service-type
       (list
        (channel
         (name 'systemic)
         (url "https://github.com/ReilySiegel/systemic")
         (introduction
          (make-channel-introduction
           "89ff5b4374e472194eff08f2a69153d5cde6784e"
           (openpgp-fingerprint
            "0FA2 FE4C 164F 60C6 7F6B  EA7E 508A 5AD0 A50F 88AF")))))))
     (home-environment-user-services base:environment)))))
