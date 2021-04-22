;;(setq gc-cons-threshold (* 50 1000 1000))
;; Portion of heap used for allocation.  Defaults to 0.1.
(setq gc-cons-percentage 0.6)

(add-to-list 'load-path "~/.config/emacs/modules")
(load-theme 'monokai t)
(setq byte-compile-warnings '(not obsolete))

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
  ;; Dired Plus
  (make-thread (require 'dired-plus))
  ;; Inline images Dired
  (make-thread (require 'dired-inline-images))
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
  ;(make-thread (require 'encrypted-org-mode))
  ;; Proverif
;  (make-thread (require 'proverif-config))
  )


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
 '(TeX-engine 'xetex)
 '(custom-safe-themes
   '("f9aede508e587fe21bcfc0a85e1ec7d27312d9587e686a6f5afdbb0d220eab50" default))
 '(package-selected-packages
   '(dired-git-info dired-rainbow jupyter dired-collapse openwith dired-quick-sort projectile org-download spaceline-all-the-icons sentence-navigation flycheck ob-async ob-sagemath ob-latex-as-png ob-shell ob-ipython latex-extra frog-jump-buffer exwm-edit symon yasnippet-snippets which-key use-package-ensure-system-package try switch-window sudo-edit spaceline smex smartparens rainbow-mode rainbow-delimiters python-docstring py-autopep8 pretty-mode ox-reveal org-sidebar org-mime org-bullets nyan-mode noflet multiple-cursors mu4e-alert monokai-theme magit-gitflow linum-relative langtool ido-vertical-mode htmlize helpful fancy-battery exwm esup elpy diredfl diminish dashboard counsel company-quickhelp calfw-org calfw-ical calfw-cal calfw blacken beacon bash-completion avy auctex all-the-icons-dired)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:box (:line-width 1 :color "black")))))
 '(mu4e-thread-folding-child-face ((t (:extend t :background "gray10" :underline))) nil '(spaceline-highlight-face ((t (:background "DarkGreen" :foreground "#FFEEEE" :inherit 'mode-line))))))
 
