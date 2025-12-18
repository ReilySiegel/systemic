(define-module  (systemic packages emacs-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages music)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public systemic-emacs-desktop-environment
  (package
    (inherit emacs-desktop-environment)
    (propagated-inputs (list alsa-utils
                             brightnessctl
                             scrot
                             slock
                             upower
                             tlp
                             playerctl))))


(define-public emacs-magit-email
  (let ((commit "5ea5fc1bba5366c463ef41d41cd1f551f4008de4")
        (revision "1"))
    (package
      (name "emacs-magit-email")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ReilySiegel/magit-email")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0czvd5riibdxfzjiviwpgwq43clg32nbzkvz7ddha56qs5v5qqs0"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-magit notmuch))
      (home-page "https://github.com/ReilySiegel/magit-email")
      (synopsis "Integrates magit and email with Emacs")
      (description "magit-email provides facilities for sending git patches as
emails using a valid emacs mail user agent.")
      (license license:mpl2.0))))

(define-public emacs-eglot-grammarly
  (let ((commit "6c199d60586a6d9c75031a2730edc8bc16ddc62f")
        (revision "2"))
    (package
      (name "emacs-eglot-grammarly")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/emacs-grammarly/eglot-grammarly")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1bccx7kgvlwf9m933v8mnj7qgsmjxbvyvygz5jpj0mpfhvky8s1f"))))
      (build-system emacs-build-system)
      (arguments '(#:tests? #f))
      (propagated-inputs (list emacs-eglot))
      (home-page "https://github.com/emacs-grammarly/eglot-grammarly")
      (synopsis "Eglot client for Grammarly")
      (description "Eglot client for Grammarly.")
      (license license:gpl3+))))

(define-public emacs-idris-mode
  (let ((commit "85928dc4cc2c22010fa91661abd55e6bd3dbacee")
        (revision "1"))
    (package
      (name "emacs-idris-mode")
      (version (git-version "1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/idris-hackers/idris-mode")
               (commit commit)))
         (file-name (git-file-name name commit))
         (sha256
          (base32
           "169a19x861yj18wrkjx4mxm12lvfdzw320f9lddsc2xcsdlrmm5v"))))
      (build-system emacs-build-system)
      (arguments '(#:tests? #f))
      (propagated-inputs
       (list emacs-prop-menu emacs-flycheck))
      (home-page
       "https://github.com/idris-hackers/idris-mode")
      (synopsis "Major mode for editing Idris code")
      (description
       "This is an Emacs mode for editing Idris code.  It is compatible with
the latest versions of Idris 1.")
      (license license:gpl3+))))
