(define-module (systemic packages networking)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (nonguix build-system binary)
  #:use-module ((guix licenses) #:prefix license:))

(define-public winbox
  (package
    (name "winbox")
    (version "4.0beta23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.mikrotik.com/routeros/winbox/" version
             "/WinBox_Linux.zip"))
       (file-name (string-append "winbox-" version ".zip"))
       (sha256
        (base32 "12plrdmwx3hm8am705xgabjdn6d5lhcdzq5mkd8ypjxa8c0pq0wx"))))
    (build-system binary-build-system)
    (arguments
     `(#:patchelf-plan `(("../WinBox"
                          ("gcc"
                           "glibc"
                           "libxcb"
                           "xcb-util"
                           "xcb-util-renderutil"
                           "xcb-util-image"
                           "xcb-util-keysyms"
                           "xcb-util-wm"
                           "xcb-util-cursor"
                           "libxkbcommon"
                           "libx11"
                           "freetype"
                           "fontconfig-minimal"
                           "mesa"
                           "zlib")))
       #:install-plan `(("../WinBox" "bin/"))))
    (native-inputs (list unzip))
    (inputs (list glibc
                  `(,gcc "lib")
                  libxcb
                  xcb-util
                  xcb-util-renderutil
                  xcb-util-image
                  xcb-util-keysyms
                  xcb-util-wm
                  xcb-util-cursor
                  libxkbcommon
                  libx11
                  freetype
                  fontconfig
                  mesa
                  zlib))
    (supported-systems '("x86_64-linux"))
    (synopsis
     "Blazing fast terminal file manager written in Rust, based on async I/O")
    (description
     "Yazi is a terminal file manager written in Rust, based on non-blocking async I/O. It aims to provide an efficient, user-friendly, and customizable file management experience.")
    (home-page "https://yazi-rs.github.io/")
    (license license:expat)))

winbox
