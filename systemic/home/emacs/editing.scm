(define-module (systemic home emacs editing)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (guix transformations)
  #:use-module (rde home services emacs)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:export (service))

(define service
  (simple-service
   'emacs-edititng home-emacs-service-type
   (emacs-configuration-extension
    ('keybind
     (define-keymap :keymap global-map
       "C-x C-d" #'duplicate-dwim))
    ('whitespace
     (add-hook 'before-save-hook #'delete-trailing-whitespace))
    ('flymake
     (add-hook 'text-mode-hook #'flymake-mode)
     (add-hook 'prog-mode-hook #'flymake-mode))
    ('eldoc
     (setopt eldoc-documentation-strategy 'eldoc-documentation-compose))
    (emacs-visual-fill-column
     (setopt visual-fill-column-center-text t)

     (add-hook 'text-mode-hook #'visual-line-mode)
     (global-visual-wrap-prefix-mode)
     (global-visual-fill-column-mode))
    (emacs-eglot
     (with-eval-after-load 'eglot
       (add-hook 'eglot--managed-mode-hook
                 (lambda nil
                   (setq-local eldoc-documentation-strategy
                               'eldoc-documentation-compose)))

       (define-keymap :keymap eglot-mode-map
         "C-c a" #'eglot-code-actions
         "C-c r" #'eglot-rename
         "C-c R" #'eglot-reconnect)


       (add-to-list
        'eglot-server-programs
        '((text-mode org-mode markdown-mode message-mode) .
          ("harper-ls" "--stdio")))

       (add-hook 'text-mode-hook #'eglot-ensure)))
    (emacs-aggressive-indent
     (electric-indent-mode -1)
     (global-aggressive-indent-mode 1))
    (emacs-multiple-cursors
     (define-keymap :keymap global-map
       "C-c M-l"   #'mc/edit-lines
       "C-c M-n"   #'mc/mark-next-like-this
       "C-c M-p"   #'mc/mark-previous-like-this
       "C-c M-a"   #'mc/mark-all-like-this
       "C-c M-d"   #'mc/mark-all-dwim
       "C-c M-v"   #'mc/vertical-align
       "C-c M-i n" #'mc/insert-numbers
       "C-c M-i l" #'mc/insert-letter)))))
