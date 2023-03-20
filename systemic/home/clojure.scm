(define-module (systemic home clojure)
  #:use-module (gnu home services)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages clojure)
  #:use-module (gnu packages java)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (nongnu packages clojure)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:export (services))

(define services
  (list
   (simple-service
    'clojure-packages home-profile-service-type
    (list clojure-tools `(,openjdk17 "jdk") clj-kondo))
   (simple-service
    'clojure-emacs home-emacs-service-type
    (emacs-configuration-extension
     (emacs-clojure-mode
      (setopt clojure-align-forms-automatically t)
      (setopt clojure-toplevel-inside-comment-form t)
      (with-eval-after-load 'clojure-mode
        ;; Define indentation for guardrails macros
        (put-clojure-indent '>def :defn)
        (put-clojure-indent '>defn :defn)
        (put-clojure-indent '>defn- :defn)
        (put-clojure-indent '>fdef :defn)

        ;; Define docstring positions for guardrails macros
        (put '>def 'clojure-doc-string-elt 2)
        (put '>defn 'clojure-doc-string-elt 2)
        (put '>defn- 'clojure-doc-string-elt 2)))
     (emacs-cider
      (setopt cider-font-lock-dynammically t
              cider-clojure-cli-aliases ":cider/dev")
      ;; Cider does not play nice with orderless.
      (add-to-list 'completion-category-defaults '(cider (styles basic))))
     (emacs-clj-refactor
      (setopt cljr-warn-on-eval t)
      (cljr-add-keybindings-with-prefix "C-c C-r")
      (add-hook 'clojure-mode-hook #'clj-refactor-mode))
     (emacs-flymake-kondor
      (with-eval-after-load 'clojure-mode
        (require 'flymake-kondor))
      (add-hook 'clojure-mode-hook #'flymake-kondor-setup))
     (emacs-clj-deps-new)))))
