(define-module  (systemic packages emacs-xyz)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (guix build-system emacs)
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
    (propagated-inputs
     `(("emacs-magit" ,emacs-magit)
       ("notmuch" ,notmuch)))
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
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ig1wdjg914p9ng1nir2fid4mb3xz2dbpmkdnfy1klq2zp0xw2s3"))
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
    (version "2.5.1")
    (home-page "https://github.com/clojure-emacs/clj-refactor.el")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pyskl9xcqrk6r2zbp5i9402inngqps7wwb4nbdbrgi4di9b8in7"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("yasnippet" ,emacs-yasnippet)
       ("paredit" ,emacs-paredit)
       ("multiple-cursors" ,emacs-multiple-cursors)
       ("clojure-mode" ,emacs-clojure-mode)
       ("cider" ,emacs-cider)
       ("parseedn" ,emacs-parseedn)
       ("inflections" ,emacs-inflections)
       ("hydra" ,emacs-hydra)))
    (synopsis "Support for refactoring Clojure code in Emacs.")
    (description "@code{clj-refactor} provides refactoring support for Clojure
projects. It complements the refactoring functionality you'd find in
clojure-mode and CIDER.")
    (license license:gpl3+)))

(define-public emacs-org-transclusion
  (package
    (name "emacs-org-transclusion")
    (version "0.2.0")
    (home-page "https://github.com/nobiot/org-transclusion")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "8bf2ecc2dffc9d365e0f14d45158f44df587fb12")))
              (file-name (git-file-name name version))
              (sha256
               (base32 "12rl97n2w35np6ifslh92d3sbcynfjn94lm4p59202sipqyrv7dv"))))
    (build-system emacs-build-system)
    (propagated-inputs
     `(("org" ,emacs-org)))
    (synopsis "Support for refactoring Clojure code in Emacs.")
    (description "@code{clj-refactor} provides refactoring support for Clojure
projects. It complements the refactoring functionality you'd find in
clojure-mode and CIDER.")
    (license license:gpl3+)))