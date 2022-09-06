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

(define-public emacs-git-email
  (package
   (name "emacs-git-email")
   (version "0.2.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://git.sr.ht/~yoctocell/git-email")
                  (commit (string-append "v" version))))
            (snippet
             ;; Not yet in Guix proper
             '(delete-file "git-email-piem.el"))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "09vmh3x1rjxxl9g9p01afil1zlpk7rf0pjmzyvcbid9wczyllkhq"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-magit notmuch))
   (home-page "https://sr.ht/~yoctocell/git-email/")
   (synopsis "Integrates git and email with Emacs")
   (description "git-email provides functions for formatting and sending Git patches
via email, without leaving Emacs.")
   (license license:gpl3+)))

(define-public emacs-app-launcher
  (let ((commit "80a9ed37892ee6e21fe44487ed11f66a15e3f440")
        (revision "1"))
    (package
     (name "emacs-app-launcher")
     (version (git-version "0.0" revision commit))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SebastienWae/app-launcher")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ywhfx8604ifmvcy2397bmvq2wj03jyqnm0g7lmqqi5p97rjbdgc"))))
     (build-system emacs-build-system)
     (home-page "https://github.com/SebastienWae/app-launcher")
     (synopsis "Launch system applications from emacs")
     (description "Uses Emacs standard completion feature to select an
application installed on your machine and launch it.")
     (license license:gpl3+))))

(define-public emacs-inflections
  (package
    (name "emacs-inflections")
    (version "2.5")
    (home-page "https://github.com/eschulte/jump.el")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "55caa66a7cc6e0b1a76143fd40eff38416928941")))
              (file-name (git-file-name name "55caa66a7cc6e0b1a76143fd40eff38416928941"))
              (sha256
               (base32
                "03fh7i6blnbc0zbmp83fk095hr3q4fdvrvfxad74zghcbc2nk7b7"))
              (snippet
               '(begin (delete-file "jump.el")
                       #t))))
    (build-system emacs-build-system)
    (synopsis "Emacs utility to convert english words between singular and
plural")
    (description "Emacs utility to convert english words between singular and
plural")
    (license license:gpl3+)))

(define-public emacs-clj-refactor
  (package
   (name "emacs-clj-refactor")
   (version "3.0.0")
   (home-page "https://github.com/clojure-emacs/clj-refactor.el")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit version)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0i759k6qm752lmdsxrgp0bh3akl01as8k8q6x1a7cpdh6yddwajs"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-yasnippet
                            emacs-paredit
                            emacs-multiple-cursors
                            emacs-clojure-mode
                            emacs-cider
                            emacs-parseedn
                            emacs-inflections
                            emacs-hydra))
   (synopsis "Support for refactoring Clojure code in Emacs.")
   (description "@code{clj-refactor} provides refactoring support for Clojure
projects. It complements the refactoring functionality you'd find in
clojure-mode and CIDER.")
   (license license:gpl3+)))

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

(define-public emacs-org-minutes
  (package
   (name "emacs-org-minutes")
   (version "0.0.1")
   (home-page "https://github.com/ReilySiegel/org-minutes")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url home-page)
                  (commit "d9ca4340061c9fe1dcd04e6a1320054f7506dc3a")))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0l9r1i86xzdhifqn58xg1f6rndi8rqhzc3kgs3842kkwd31d9lp5"))))
   (build-system emacs-build-system)
   (arguments
    `(#:include (cons* "^snippets\\/" %default-include))) 
   (propagated-inputs (list emacs-org
                            emacs-org-roam
                            emacs-yasnippet))
   (synopsis "Tools for taking minutes in org-mode")
   (description "Tools for taking minutes in org-mode. Designed to work with
org-roam and yasnippet.")
   (license license:gpl3+)))

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

(define-public emacs-flymake-languagetool
  (let ((commit "5f9eef8fe3f342f17593e0caeaba906ba7cab24a")
        (revision "1"))
    (package
      (name "emacs-flymake-languagetool")
      (version (git-version "0.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/emacs-languagetool/flymake-languagetool")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1mz18inlv78lg40jgq68qq3v62b6mgmz89lcgczg93lyfh81pjp3"))))
      (build-system emacs-build-system)
      (propagated-inputs (list emacs-s))
      (home-page "https://github.com/emacs-languagetool/flymake-languagetool")
      (synopsis "Flymake support for LanguageTool")
      (description "Flymake support for LanguageTool.")
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
