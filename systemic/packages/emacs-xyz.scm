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
  (let ((commit "c245f45b599504953d3936d897f1538e02d1ba5e")
        (revision "1"))
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
                  "0xnzbahjbd8qa0j5qh4nb226wks0psw9kq28z4x29bvw7y1n73r8"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-eglot))
      (home-page "https://github.com/emacs-grammarly/eglot-grammarly")
      (synopsis "Eglot client for Grammarly")
      (description "Eglot client for Grammarly.")
      (license license:gpl3+))))

(define-public emacs-nano-theme
  ;; No tagged release upstream.  The commit below matches latest version
  ;; bump.
  (let ((commit "c4f296d349cf5ef2efd88d68535a4dbf577b9a87"))
    (package
      (name "emacs-nano-theme")
      (version "0.3.1")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rougier/nano-theme")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "095n3cv0c900vgl9592ydwnln11cziz165ddx14hfp35yqjvw9y8"))))
      (build-system emacs-build-system)
      (home-page "https://github.com/rougier/nano-theme")
      (synopsis "A consistent theme for GNU Emacs.")
      (description "Nano theme provides a light theme based on Material colors,
and a dark theme based on Nord colors. The theme is based on a set of six faces.")
      (license license:gpl3+))))
