(define-module (systemic packages)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:export (without-tests))

(define without-tests
  (case-lambda
    ((p) (without-tests p p))
    ((t p) ((options->transformation
             `((without-tests . ,(package-name t))))
            p))))
