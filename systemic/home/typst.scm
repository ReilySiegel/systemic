(define-module (systemic home typst)
  #:use-module (gnu home services)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (rde home services emacs)
  #:use-module (systemic home emacs-utils)
  #:export (systemic-typst-service-type))


(define emacs-extension
  (emacs-configuration-extension
   (emacs-typst-ts-mode
    (setopt typst-ts-enable-raw-blocks-highlight t
            typst-ts-preview-function #'find-file-other-window
            typst-ts-grammar-location
            ,(file-append tree-sitter-typst
                          "/lib/tree-sitter/libtree-sitter-typst.so"))

    (add-to-list 'display-buffer-alist
                 '("\\*typst-ts-compilation\\*" (display-buffer-no-window)))


    (add-to-list 'treesit-extra-load-path
                 ,(file-append tree-sitter-typst "/lib/tree-sitter/"))
    (keymap-set typst-ts-mode-map "C-c C-c" #'typst-ts-tmenu)


    (with-eval-after-load 'eglot
      (with-eval-after-load 'typst-ts-mode
        (add-to-list
         'eglot-server-programs
         `((typst-ts-mode) . "rass -- tinymist -- harper-ls --stdio")))))))


(define packages (list typst tree-sitter-typst rassumfrassum))

(define systemic-typst-service-type
  (service-type
    (name 'systemic-typst)
    (description "Typst configuration.")
    (default-value #f)
    (extensions
     (list
      (service-extension home-emacs-service-type (const emacs-extension))
      (service-extension home-profile-service-type (const packages))))))
