(define-module (systemic home desktop)
  #:use-module (gnu home-services base)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix gexp)
  #:use-module (systemic home emacs-utils)
  #:use-module (systemic packages emacs-xyz)
  #:export (picom-service
            exwm-configuration))

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

(define picom-service
  (home-generic-service
   'picom
   #:files `(("config/picom/picom.conf"
              ,(mixed-text-file "picom.conf" picom-config)))
   #:packages (list picom)
   #:extensions
   `((,home-shepherd-service-type
      .
      ,(list
        (shepherd-service
         (documentation "Run picom.")
         (provision '(picom))
         (start #~(make-system-constructor "picom -b"))
         (stop #~(make-system-destructor "pkill picom -SIGKILL"))))))))

(define exwm-configuration
  (elisp-configuration-package
   "exwm"
   `((require 'exwm)
     (require 'exwm-config)
     (require 'exwm-randr)
     
     ;; Start a server for external processes to communicate with.
     (server-start)
     ;; Make class name the buffer name
     (add-hook 'exwm-update-class-hook
               (lambda ()
                 (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                             (string= "gimp" exwm-instance-name))
                   (exwm-workspace-rename-buffer exwm-class-name))))
     (add-hook 'exwm-update-title-hook
               (lambda ()
                 (when (or (not exwm-instance-name)
                           (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                           (string= "gimp" exwm-instance-name))
                   (exwm-workspace-rename-buffer exwm-title))))
     ;; 's-r': Reset
     (exwm-input-set-key (kbd "s-r") ,#~"#'exwm-reset")
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
     (exwm-enable)
     
     ;; EXWM Randr
     (setq exwm-workspace-number 1)
     (setq exwm-randr-workspace-output-plist '(1 "eDP-1"))
     (add-hook 'exwm-randr-screen-change-hook
               (lambda ()
                 (start-process-shell-command
                  "xrandr" nil "xrandr --output eDP-1 --right-of HDMI-1 --auto")))
     (exwm-randr-enable)

     ;; Desktop Environment
     (require 'desktop-environment)
     (desktop-environment-mode)

     ;; Use pinentry in emacs
     (add-hook 'after-init-hook 'pinentry-start)
     (setenv "PINENTRY_USER_DATA" "USE_CURSES=0")
     (setenv "GPG_TTY" "/dev/pts/1")
     (setq epa-pinentry-mode 'loopback
           pinentry-popup-prompt-window nil)

     (require 'exec-path-from-shell)

     (setq exec-path-from-shell-variables
           (append
            '("SSH_AUTH_SOCK" "SSH_AGENT_PID")
            exec-path-from-shell-variables))
     (add-hook 'after-init-hook 'exec-path-from-shell-initialize)
     
     ;; Enable launching apps
     (global-set-key (kbd "s-SPC") 'app-launcher-run-app))
   #:elisp-packages (list emacs-app-launcher
                          emacs-exec-path-from-shell
                          emacs-exwm
                          emacs-pinentry
                          systemic-emacs-desktop-environment)
   #:autoloads? #t))
