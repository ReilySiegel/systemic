(define-module  (systemic packages wm)
  #:use-module (gnu packages wm)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public wlroots-0.16
  (package
    (inherit wlroots-0.17)
    (name "wlroots-0.16")
    (version "0.16.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m12nv6avgnz626h3giqp6gcx44w1wq6z0jy780mx8z255ic7q15"))))
    (propagated-inputs (modify-inputs (package-propagated-inputs wlroots-0.17)
                                      (delete libdisplay-info)))))
