(define-module (systemic packages cyrus-sasl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

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

(define-public cyrus-sasl-xoauth2
  (package
    (name "cyrus-sasl-xoauth2")
    (version "0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/moriyoshi/cyrus-sasl-xoauth2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1py9f1mn5k5xihrk0lfrwr6723c22gjb7lmgya83ibvislm2x3wl"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~(list (string-append "--with-cyrus-sasl="
                                                    #$output)
                                     "--disable-static")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-autogen
                          (lambda _
                            (substitute* "autogen.sh"
                              ;; Add shebang to autogen.sh
                              (("libtoolize") "#!/bin/sh\nlibtoolize")))))))
    (inputs (list cyrus-sasl))
    (native-inputs (list autoconf automake libtool))
    (home-page "https://github.com/moriyoshi/cyrus-sasl-xoauth2")
    (synopsis "XOAUTH2 plugin for Cyrus SASL")
    (description "Adds support for XOAUTH2 authentication to Cyrus SASL.  This
package can be used with isync to fetch mail from servers that support it.")
    (license license:expat)))
