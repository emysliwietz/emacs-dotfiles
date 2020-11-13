(require 'bgex)

;; Image on frame
(when (boundp 'bgex-exist-p)
  (bgex-set-image-default "~/.config/emacs/images/background.xpm"))

;; Image on frame (dynamic color mode (SRC * DST / factor))
(bgex-set-image-default "~/.config/emacs/images/background.xpm" t)

;; Color for HTML-mode (dynamic color mode)
(bgex-set-color "HTML" 'bgex-identifier-type-major-mode '(60000 40000 40000) t)

;; Color for buffer-name (*scratch*)
(bgex-set-color "*scratch*" 'bgex-identifier-type-buffer-name "skyblue")

;; XPM string
(bgex-set-xpm-string "*scratch*" 'bgex-identifier-type-buffer-name "XPM string" t)
(bgex-set-xpm-string-default "XPM string" t)


;(set-background-color "#000000")
(set-frame-parameter nil 'bg-image-enable-flag t)
(set-frame-parameter nil 'bg-fill-alpha 0.2)
(set-frame-parameter nil 'bg-image-filename "/home/user/.config/emacs/images/background.xpm")

(provide 'bgex-tweaks)
