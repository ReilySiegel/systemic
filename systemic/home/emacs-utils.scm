(define-module (systemic home emacs-utils)
  #:use-module (gnu home-services-utils)
  #:use-module (guix build-system emacs)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:export (elisp-configuration-package))

(define (serialize-elisp-config field-name val)
  (define (serialize-list-element elem)
    (cond
     ((gexp? elem)
      elem)
     (else
      #~(string-trim-right
	 (with-output-to-string
	   (lambda ()
	     ((@@ (ice-9 pretty-print) pretty-print)
	      '#$elem
              #:max-expr-width 79)))
	 #\newline))))

  #~(string-append
     #$@(interpose
	 (map serialize-list-element val)
	 "\n" 'suffix)))

(define* (elisp-configuration-package
	  package-name elisp-expressions
	  #:key
	  (elisp-packages '())
	  (autoloads? #t))
  "Takes a list of Elisp expressions, create emacs-NAME-configuration package.
@code{#~\";;;###autoload\"} can be used to make next expression be loaded on
startup."

  (define (package->package-input pkg)
    (list ((@@ (guix packages) package-name) pkg) pkg))

  (define (add-autoloads elisp-expressions)
    (fold-right
     (lambda (e acc)
       (if (list? e)
	   (cons* #~";;;###autoload" e #~"" acc)
	   (cons* e #~"" acc)))
     '() elisp-expressions))

  (package
    (name (string-append "emacs-" package-name "-configuration"))
    (version "1.0.0")
    (build-system emacs-build-system)
    (source
     (mixed-text-file
      (string-append name ".el")
      (serialize-elisp-config
       #f
       ((if autoloads? add-autoloads identity) elisp-expressions))))
    (propagated-inputs (map package->package-input elisp-packages))
    (synopsis "Generated Emacs configuration package.")
    (description "Package generated by @code{elisp-configuration-package}.")
    (home-page "https://www.gnu.org/software/guix/")
    (license license:gpl3+)))
