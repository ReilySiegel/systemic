(define-module (gnu home services channels)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (home-channels-service-type))

(define (home-xdg-configuration-files-service-extension channels)
  `(("guix/channels.scm"
     ,(plain-file
       "channels.scm"
       (call-with-output-string
         (lambda (port)
           (pretty-print (cons 'list (map channel->code channels)) port)))))))

(define home-channels-service-type
  (service-type
   (name 'home-channels)
   (default-value %default-channels)
   (compose concatenate)
   (extend append)
   (extensions
    (list (service-extension home-xdg-configuration-files-service-type
                             home-xdg-configuration-files-service-extension)))
   (description "Manages $XDG_CONFIG_HOME/guix/channels.scm")))
