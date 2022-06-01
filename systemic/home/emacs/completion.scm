(define-module (systemic home emacs completion)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (systemic home emacs-utils)
  #:export (service))


(define service
  (simple-service
   'emacs-completion-service
   home-emacs-service-type
   (emacs-configuration-extension
    ("minibuffer"
     (setopt resize-mini-windows t
             ;; Do not allow the cursor in the minibuffer prompt
             minibuffer-prompt-properties
             '(read-only t cursor-intangible t face minibuffer-prompt)
             ;; Enable recursive minibuffers
             enable-recursive-minibuffers t)
     (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode))
    (emacs-orderless
     (require 'orderless)
     (setq completion-styles '(orderless basic)
           completion-category-defaults nil
           completion-category-overrides '((file (styles . (partial-completion)))))
     (savehist-mode))
    (emacs-vertico
     (setopt vertico-cycle 1)
     (vertico-mode))
    (emacs-marginalia
     (keymap-set minibuffer-local-map "M-A" 'marginalia-cycle)
     (marginalia-mode))
    (emacs-embark
     (keymap-global-set "C-." 'embark-act)
     (keymap-global-set "M-." 'embark-dwim)
     (keymap-global-set "C-h B" 'embark-bindings)

     (setopt prefix-help-command #'embark-prefix-help-command)

     (with-eval-after-load 'embark
       (add-to-list 'display-buffer-alist
                    '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                      nil
                      (window-parameters (mode-line-format . none)))))
     (with-eval-after-load 'consult
       (require 'embark-consult)))
    (emacs-consult
     (keymap-global-set "C-s" 'consult-line)))))
