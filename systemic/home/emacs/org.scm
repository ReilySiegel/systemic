(define-module (systemic home emacs org)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:use-module (guix transformations)
  #:export (service))

(define org-directory '(file-truename "~/org"))

(define service
  (simple-service
   'emacs-org-service
   home-emacs-service-type
   (emacs-configuration-extension
    (emacs-org
     (with-eval-after-load 'org
       (setopt org-directory ,org-directory
               org-edit-src-content-indentation 0
               org-confirm-babel-evaluate nil
               org-adapt-indentation nil
               org-log-done (quote time)
               org-log-redeadline nil
               org-log-reschedule nil
               ;; Force use emacs for PDF files.
               org-file-apps (butlast org-file-apps)
               org-latex-compiler "xelatex")

       ;; Default to using /tmp as export 
       (define-advice org-export-output-file-name
         (:filter-args (r) org-add-export-dir)
         (pcase-let ((`(,r1 ,r2 ,export-dir) r))
                    (list r1 r2 (or export-dir "/tmp"))))

       (org-babel-do-load-languages
        'org-babel-load-languages
        '((plantuml . t)
          (scheme . t)
          (gnuplot . t)
          (java . t)
          (clojure . t)
          (python . t)
          (R . t)))
       
       ;; Save buffer after clocking
       (add-hook 'org-clock-in-hook 'save-buffer)
       (add-hook 'org-clock-out-hook 'save-buffer)

       ;; This keybind is not useful with many agenda files, and is used for
       ;; avy-goto-char.
       (keymap-unset org-mode-map "C-'" t)))
    (emacs-plantuml-mode
     (with-eval-after-load 'org
       (require 'plantuml-mode)
       (setopt org-plantuml-jar-path plantuml-jar-path)))
    (emacs-org-super-agenda
     (with-eval-after-load 'org
       (require 'org-super-agenda)
       (org-super-agenda-mode)
       (keymap-global-set "C-c a" #'org-agenda-list)
       (setopt org-agenda-span 1
               org-agenda-files (list ,org-directory
                                      (concat ,org-directory "/daily")))
       
       (setq
        org-super-agenda-groups '((:log t)  ; Automaticaally named "Log"
                                  (:name "Schedule"
                                         :time-grid t)
                                  (:name "Today"
                                         :scheduled today)
                                  (:habit t)
                                  (:name "Due today"
                                         :deadline today)
                                  (:name "Overdue"
                                         :deadline past)
                                  (:name "Due soon"
                                         :deadline future)
                                  (:name "Todo"
	                                 :todo "TODO")))

       (defun org-class-days (y1 m1 d1 y2 m2 d2 days)
         (seq-filter (lambda (day) (org-class y1 m1 d1 y2 m2 d2 day))
                     days))))
    (emacs-org-roam
     (setopt org-roam-directory ,org-directory
             org-roam-v2-ack t)

     (keymap-global-set "C-c n l" 'org-roam-buffer-toggle)
     (keymap-global-set "C-c n f" 'org-roam-node-find)
     (keymap-global-set "C-c n g" 'org-roam-graph)
     (keymap-global-set "C-c n i" 'org-roam-node-insert)
     (keymap-global-set "C-c n c" 'org-roam-capture)
     (keymap-global-set "C-c n t" 'org-roam-tag-add)
     (keymap-global-set "C-c n d" 'org-roam-dailies-map)

     (with-eval-after-load 'org-roam
       (org-roam-db-autosync-mode))))))
