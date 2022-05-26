(define-module (systemic home bibliography)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix transformations)
  #:export (bibliography-configuration
            bibliography-service-type))

(define-configuration/no-serialization bibliography-configuration
  (bibtex-file
   (string)
   "Bibliography file to use for bibtex.")
  (csl-directory
   (string)
   "Location for CSL files"))

(define (bibliography-emacs config)
  (home-emacs-extension
   (init-el
    `((use-package
       citar
       :no-require
       :custom
       (org-cite-global-bibliography '(,(bibliography-configuration-bibtex-file
                                         config)))
       (org-cite-insert-processor 'citar)
       (org-cite-follow-processor 'citar)
       (org-cite-activate-processor 'citar)
       (citar-bibliography org-cite-global-bibliography)
       ;; optional: org-cite-insert is also bound to C-c C-x C-@
       :bind
       (:map org-mode-map
        :package org ("C-c b" . (function org-cite-insert))))
      (use-package
       citeproc
       :custom
       (org-cite-csl-locales-dir ,(bibliography-configuration-csl-directory config))
       (org-cite-csl-styles-dir ,(bibliography-configuration-csl-directory config)))))))

(define bibliography-service-type
  (service-type
   (name 'bibliography)
   (description "A basic bibliography service.")
   (extensions
    (list
     (service-extension home-emacs-service-type bibliography-emacs)
     (service-extension home-profile-service-type
                        (lambda _
                          (list emacs-citar emacs-org
                                ((options->transformation
                                  '((with-branch . "emacs-citeproc-el=master")))
                                 emacs-citeproc-el))))))))
