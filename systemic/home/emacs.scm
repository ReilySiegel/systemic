(define-module (systemic home emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (systemic home mail)
  #:use-module (systemic packages emacs-xyz)
  #:export (emacs-packages))

(define emacs-packages
  (list
   emacs-nord-theme emacs-use-package emacs-exwm emacs-vertico emacs-orderless
   emacs-marginalia emacs-consult emacs-doom-modeline emacs-pinentry 
   emacs-exec-path-from-shell emacs-avy emacs-app-launcher
   emacs-company emacs-git-auto-commit-mode emacs-outshine emacs-aggressive-indent
   emacs-flycheck emacs-lsp-mode emacs-lsp-ui emacs-dap-mode emacs-magit
   emacs-magit-todos emacs-forge emacs-paredit emacs-yasnippet
   emacs-yasnippet-snippets emacs-clojure-mode emacs-cider emacs-gnuplot
   emacs-lsp-java emacs-esup  emacs-flyspell-correct emacs-racket-mode emacs-geiser
   emacs-yaml-mode emacs-plantuml-mode emacs-org emacs-org-super-agenda
   emacs-org-fragtog emacs-pdf-tools emacs-auctex emacs-which-key
   emacs-discover-my-major emacs-no-littering emacs-guix emacs-git-email
   emacs-clj-refactor emacs-origami-el emacs-org-roam emacs-org-transclusion
   notmuch-emacs-configuration))
