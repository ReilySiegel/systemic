(define-module (systemic packages vpn)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:))

(define-public netbird
  (package
    (name "netbird")
    (version "0.10.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://github.com/netbirdio/netbird/releases/download/v0.10.8/netbird_"
         version "_linux_amd64.tar.gz"))
       (sha256 (base32 "1lhww9qam1k8bfpx92nx2ixa0j75fdhybdyfd6l1x1500vaq02ik"))))
    (build-system copy-build-system)
    (arguments '(#:install-plan
                 '(("netbird" "bin/"))))
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
