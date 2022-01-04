(define-module (systemic home emacs completion)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (systemic home emacs-utils)
  #:export (candidate-configuration))

(define candidate-configuration
  (elisp-configuration-package
   "candidate"
   `(;; Grow and shrink minibuffer
     (setq resize-mini-windows t)

     ;; Do not allow the cursor in the minibuffer prompt
     (setq minibuffer-prompt-properties
           '(read-only t cursor-intangible t face minibuffer-prompt))
     (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)

     ;; Enable recursive minibuffers
     (setq enable-recursive-minibuffers t)
     (setq vertico-cycle t)

     (setq completion-styles '(orderless basic)
           completion-category-defaults nil
           completion-category-overrides
           '((file (styles . (partial-completion)))))

     (add-hook 'after-init-hook (lambda nil
                                  (vertico-mode)
                                  (savehist-mode)
                                  (marginalia-mode)))

     (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

     (global-set-key (kbd "C-.") 'embark-act)
     (global-set-key (kbd "M-.") 'embark-dwim)
     (global-set-key (kbd "C-h B") 'embark-bindings)
     
     (setq prefix-help-command (function embark-prefix-help-command))
     
     (with-eval-after-load
      'embark
      (add-to-list 'display-buffer-alist
                   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                     nil
                     (window-parameters (mode-line-format . none))))
      
      (with-eval-after-load
       'consult
       (require 'embark-consult)))

     (advice-add (function completing-read-multiple) :override
                 (function consult-completing-read-multiple))

     (global-set-key (kbd "C-s") 'consult-line))
   #:autoloads? #t
   #:elisp-packages (list emacs-vertico
                          emacs-marginalia
                          emacs-orderless
                          emacs-embark
                          emacs-consult)))
