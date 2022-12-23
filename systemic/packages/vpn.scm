(define-module (systemic packages vpn)
  #:use-module (gnu packages dns)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public netbird
  (package
    (name "netbird")
    (version "0.11.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/netbirdio/netbird/releases/download/v"
         version "/netbird_" version "_linux_amd64.tar.gz"))
       (sha256 (base32 "080h8cgdkww508ngzr5p6dnk8hm2w9m4a977zka3ifh0bp52a374"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("netbird" "bin/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-netbird
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (openresolv (string-append (assoc-ref inputs "openresolv")
                                               "/sbin")))
               (wrap-program (string-append out "/bin/netbird")
                 `("PATH" ":" prefix ,(list openresolv)))))))))
    (inputs (list openresolv))
    (home-page "https://netbird.io/")
    (synopsis "Connect your devices into a single secure private WireGuard®-based
mesh network with SSO/MFA and simple access controls")
    (description "NetBird is an open-source VPN management platform built on top of
WireGuard® making it easy to create secure private networks for your organization
or home.

It requires zero configuration effort leaving behind the hassle of opening
ports, complex firewall rules, VPN gateways, and so forth.

NetBird uses NAT traversal techniques to automatically create an overlay
peer-to-peer network connecting machines regardless of location (home, office,
data center, container, cloud, or edge environments), unifying virtual private
network management experience.")
    (license license:bsd-3)))
