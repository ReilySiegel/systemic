(define-module (systemic home rust)
  #:use-module (gnu home services)
  #:use-module (gnu packages rust)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (systemic-rust-service-type))

(define env-vars `(("PATH" . "$HOME/.cargo/bin::$PATH")))

(define packages (list rust))

(define systemic-rust-service-type
  (service-type
    (name 'systemic-rust)
    (description "Rust/Cargo configuration")
    (default-value #f)
    (extensions
     (list
      (service-extension home-environment-variables-service-type (const env-vars))
      (service-extension home-profile-service-type (const packages))))))
