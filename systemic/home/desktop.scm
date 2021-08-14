(define-module (systemic home desktop)
  #:use-module (gnu home-services base)
  #:use-module (gnu home-services shepherd)
  #:use-module (gnu packages compton)
  #:use-module (guix gexp)
  #:export (picom-service))

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
         (stop #~(make-system-destructor "pkill picom -SIGKILL")))))))
  )
