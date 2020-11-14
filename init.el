(setq gc-cons-threshold (* 50 1000 1000))
;; Portion of heap used for allocation.  Defaults to 0.1.
(setq gc-cons-percentage 0.6)

(add-to-list 'load-path "~/.config/emacs/modules")
(load-theme 'monokai t)
(setq byte-compile-warnings '(not obsolete))

;; Remove warnings and unnecessary output
(make-thread (require 'quiet))
;; Melpa settings and use-package
(make-thread (require 'package-management))
;; General settings
(make-thread (require 'general))
;; Interface tweaks
(make-thread (require 'interface))
;; Exwm
(make-thread (require 'exwm-tweaks))
;; Bgex
;(make-thread (require 'bgex))
;(make-thread (require 'bgex-tweaks))



(defun defered-loading ()
  ;; Emacs startup time profiling
  (require 'esup)
  ;; Config visiting and editing
  (make-thread (require 'config))
  ;; Better documentation
  (make-thread (require 'documentation))
  ;; Multiple cursors
  (make-thread (require 'multicursor))
  ;; Navigation tweaks
  (make-thread (require 'navigation))
  ;; Improved searching (ido, counsel)
  (make-thread (require 'search))
  ;; ANSI Term and bash completion
  (make-thread (require 'console))
  ;; File shortcuts
  (make-thread (require 'shortcuts))
  ;; Languagetool grammar checking
  (make-thread (require 'languagetool))
  ;; Org mode configuration
  (make-thread (require 'org-mode))
  ;; mu4e mail
  (make-thread (require 'email))
  ;; Changes to dired
  (make-thread (require 'dired-tweaks))
  ;; Emacs Server
  (make-thread (require 'emacs-server))
  ;; Git integration
  (make-thread (require 'magit))
  ;; Editing tweaks such as autocomplete
  (make-thread (require 'editing))
  ;; Languages
  (make-thread (require 'languages))
  ;; RSS Reader
  (make-thread (require 'elfeed-tweaks))
  ;; Statusbar
  ;  (make-thread (require 'symon-tweaks))
  ;  (make-thread (require 'headerline))
  ;; Dailyplan-mode
  (make-thread (require 'encrypted-org-mode))
  )

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Use a hook so the message doesn't get clobbered by other messages.
(defun startup-time-message ()
  (message "Emacs ready in %s"
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time
                                   before-init-time))))
  )

(add-hook 'emacs-startup-hook
	  'startup-time-message)

;(require 'test)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open")))
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("f9aede508e587fe21bcfc0a85e1ec7d27312d9587e686a6f5afdbb0d220eab50" default))
 '(fci-rule-color "#3C3D37")
 '(highlight-changes-colors '("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   '(("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100)))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   '(elfeed-goodies elfeed-org exwm-edit symon yasnippet-snippets which-key use-package-ensure-system-package try switch-window sudo-edit spaceline smex smartparens rainbow-mode rainbow-delimiters python-docstring py-autopep8 pretty-mode ox-reveal org-sidebar org-mime org-bullets nyan-mode noflet multiple-cursors mu4e-alert monokai-theme magit-gitflow linum-relative langtool ido-vertical-mode htmlize helpful fancy-battery exwm esup elpy diredfl diminish dashboard counsel company-quickhelp calfw-org calfw-ical calfw-cal calfw blacken beacon bash-completion avy auctex all-the-icons-dired))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   '((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
