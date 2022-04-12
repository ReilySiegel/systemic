(define-module (systemic home desktop)
  #:use-module (gnu home-services emacs)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:export (picom-service-type
            exwm-service-type))

(define picom-config
  "backend = \"glx\";

glx-no-stencil = true;
glx-no-rebind-pixmap = true;
use-damage = true;
xrender-sync-fence = true;
refresh-rate = 0;
vsync = true;
 
mark-wmwin-focused = true;
mark-ovredir-focused = true;
use-ewmh-active-win = true;

shadow = true;
shadow-radius = 3;
shadow-offset-x = -3;
shadow-offset-y = -3;
shadow-opacity = 0.5;
shadow-exclude = [
 \"! name~=''\",
 \"name = 'Notification'\",
 \"name = 'xfce4-notifyd'\",
 \"name *= 'picom'\",
 \"name *= 'Chromium'\",
 \"class_g = 'Navigator' && argb\",
 \"class_g ?= 'Notify-osd'\",
 \"class_g ?= 'Cairo-dock'\",
 \"class_g ?= 'Xfce4-notifyd'\",
 \"class_g ?= 'Xfce4-power-manager'\",
 \"_GTK_FRAME_EXTENTS@:c\",
 \"bounding_shaped && !rounded_corners\"
];

fading = true;
fade-delta = 5;
fade-in-step = 0.03;
fade-out-step = 0.03;
no-fading-openclose = true;
fade-exclude = [ ];

active-opacity = 1;
inactive-opacity = 0.9;
frame-opacity = 1;
inactive-opacity-override = false;

opacity-rule = [
 \"100:class_g = 'Termite' && _NET_WM_STATE@:32a\",
 \"95:class_g = 'Termite' && !_NET_WM_STATE@:32a\",
 \"0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'\"
];

# blur-background = true;
# blur-background-frame = true;

blur-background-fixed = false;
blur-kern = \"3x3box\";
blur-background-exclude = [
 \"window_type = 'dock'\",
 \"window_type = 'desktop'\",
 \"_GTK_FRAME_EXTENTS@:c\"
];

unredir-if-possible = false;
 
detect-rounded-corners = true;
detect-client-opacity = true;
detect-transient = true;
detect-client-leader = true;
 
wintypes:
{
 tooltip = { opacity = 0.95; shadow = false; fade = true; focus = true; };
 dock = { shadow = false; };
 dnd = { shadow = false; };
};")

(define picom-service-type
  (service-type
   (name 'picom)
   (extensions
    (list
     (service-extension
      home-xdg-configuration-files-service-type
      (const `(("picom/picom.conf"
                ,(mixed-text-file "picom.conf" picom-config)))))
     (service-extension
      home-profile-service-type
      (const (list picom)))
     (service-extension
      home-shepherd-service-type
      (const (list
              (shepherd-service
               (documentation "Run picom.")
               (provision '(picom))
               (start #~(make-system-constructor "picom -b"))
               (stop #~(make-system-destructor "pkill picom -SIGKILL"))))))))))

(define (exwm-emacs-extension config)
  (home-emacs-extension
   (elisp-packages (list emacs-app-launcher
                         emacs-exec-path-from-shell
                         emacs-exwm
                         emacs-pinentry
                         systemic-emacs-desktop-environment))
   (init-el
    `(;; Start a server for external processes to communicate with.
      (server-start)
      
      (use-package
       exwm
       :demand t
       :hook ((exwm-update-class
               .
               (lambda ()
                 (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                             (string= "gimp" exwm-instance-name))
                   (exwm-workspace-rename-buffer exwm-class-name))))
              (exwm-update-title
               .
               (lambda ()
                 (when (or (not exwm-instance-name)
                           (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                           (string= "gimp" exwm-instance-name))
                   (exwm-workspace-rename-buffer exwm-title)))))
       :config
       (require 'exwm-config)
       
       ;; 's-r': Reset
       (exwm-input-set-key (kbd "s-r") (function exwm-reset))

       ;; Line-editing shortcuts
       (setq
        exwm-manage-force-tiling t
        exwm-input-simulation-keys
        '(
          ;; movement
          ([?\C-b] . [left])
          ([?\M-b] . [C-left])
          ([?\C-f] . [right])
          ([?\M-f] . [C-right])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\M-v] . [prior])
          ([?\C-v] . [next])
          ([?\C-d] . [delete])
          ([?\C-k] . [S-end delete])
          ;; cut/paste.
          ([?\C-w] . [?\C-x])
          ([?\M-w] . [?\C-c])
          ([?\C-y] . [?\C-v])
          ;; search
          ([?\C-s] . [?\C-f])
          ;; save
          ([?\C-x?\C-s] . [?\C-s])
          ;; quit
          ([?\C-g] . [escape])))

       ;; Enable EXWM
       (exwm-enable))

      (use-package
       exwm-randr
       :after (exwm)
       :hook (exwm-randr-screen-change
              .
              (lambda ()
                (start-process-shell-command
                 "xrandr" nil "xrandr --output eDP-1 --right-of HDMI-1 --auto")))
       :config
       ;; EXWM Randr
       (setq exwm-workspace-number 1)
       (setq exwm-randr-workspace-output-plist '(1 "eDP-1"))
       (exwm-randr-enable))

      (use-package
       desktop-environment
       :after (exwm)
       :bind (:map desktop-environment-mode-map
              ;; Keyboard does not send x86 keys with shift
              ("S-<f1>" . desktop-environment-brightness-decrement-slowly)
              ("S-<f2>" . desktop-environment-brightness-increment-slowly)
              ("S-<f5>" . desktop-environment-volume-decrement-slowly)
              ("S-<f6>" . desktop-environment-volume-increment-slowly))
       :defer 5
       :config
       (setq desktop-environment-update-exwm-global-keys :prefix)
       (desktop-environment-mode))

      (use-package
       app-launcher
       :after (exwm)
       :bind (("s-SPC" . app-launcher-run-app)
              ("C-c l" . app-launcher-run-app)))

      (use-package
       exec-path-from-shell
       :hook ((after-init . exec-path-from-shell-initialize)
              (after-init . pinentry-start))
       :config
       (setenv "PINENTRY_USER_DATA" "USE_CURSES=0")
       (setenv "GPG_TTY" "/dev/pts/1")
       (setq epa-pinentry-mode 'loopback
             pinentry-popup-prompt-window nil)
       (setq exec-path-from-shell-variables
             (append
              '("SSH_AUTH_SOCK" "SSH_AGENT_PID")
              exec-path-from-shell-variables)))))))

(define exwm-service-type
  (service-type
   (name 'exwm)
   (default-value #f)
   (extensions
    (list
     (service-extension home-emacs-service-type exwm-emacs-extension)))))
