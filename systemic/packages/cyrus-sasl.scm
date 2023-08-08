(define-module (systemic packages cyrus-sasl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (guix packages))

(define-public cyrus-sasl/search-paths
  (package/inherit cyrus-sasl
    (native-search-paths
     (list (search-path-specification
            (variable "SASL_PATH")
            (files (list "lib/sasl2")))))))

(module-define!
 (resolve-module '(gnu packages cyrus-sasl))
 'cyrus-sasl
 (package/inherit cyrus-sasl
   (replacement cyrus-sasl/search-paths)))
