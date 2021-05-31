;; Swiper / Ivy / Counsel
;;  Swiper gives us a really efficient incremental search with regular expressions
;;  and Ivy / Counsel replace a lot of ido or helms completion functionality
(use-package counsel
  :ensure t
  :defer t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :defer t
  :diminish (ivy-mode)
  :bind (("C-x C-b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "
	ivy-height 20
	enable-recursive-minibuffers t
	ivy-display-style 'fancy))

(use-package all-the-icons-ibuffer
  :ensure t
  :init (all-the-icons-ibuffer-mode 1))

(use-package all-the-icons-ivy
:ensure t
:init
(all-the-icons-ivy-setup)
:config
(setq all-the-icons-ivy-file-commands
      '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))

;; Overwrite some stuff for exwm and icons in Firefox
(defun all-the-icons-ivy--icon-for-firefox (mode buffname)
  "Apply `all-the-icons-icon-for-url' on Firefox window in exwm-mode.
Assuming that url is in title like in Keepass Helper extension."
  (if (string-equal (format "%s" mode) "exwm-mode")
      (let ((bnl (split-string buffname " - "))
	    (fnl (split-string buffname " — ")))
	    (let ((browser (format "%s" (last fnl))))
      (if (or (string-equal browser "(Mozilla Firefox)") (string-equal browser "(Mozilla Firefox (Private Browsing))"))
	  (all-the-icons-icon-for-url (first bnl) :face 'all-the-icons-blue)
	)))))

;; Overwrite some stuff for exwm and icons in Tor Browser
(defun all-the-icons-ivy--icon-for-tor (mode buffname)
  "Apply youtube icon on Tor Browser window in exwm-mode.
Not assuming that url is in title like in Keepass Helper extension, for privacy."
  (if (string-equal (format "%s" mode) "exwm-mode")
      (let ((bnl (split-string buffname " - ")))
	(if (string-equal (format "%s" (last bnl)) "(Tor Browser)")
	    (if (string-equal (format "%s" (last bnl 2)) "(YouTube Tor Browser)")
		(all-the-icons-icon-for-url "youtube.com" :face 'all-the-icons-red)
	      (all-the-icons-faicon "user-secret" :face 'all-the-icons-red)
	      )))))

;; Overwrite some stuff for exwm
(defun all-the-icons-ivy--icon-for-exwm (mode buffname)
  "Hard-code some icons for common programs."
  (if (string-equal (format "%s" mode) "exwm-mode")
      (cond ((string-prefix-p "Signal" buffname)
	     (all-the-icons-faicon "comment" :face 'all-the-icons-blue-alt))
	    ((string-prefix-p "Skype" buffname)
	     (all-the-icons-faicon "skype" :face 'all-the-icons-blue))
	    ((string-suffix-p " - Discord" buffname)
	     (all-the-icons-faicon "simplybuilt" :face 'all-the-icons-purple))
	    ((string-prefix-p "OBS" buffname)
	     (all-the-icons-faicon "video-camera" :face 'all-the-icons-purple-alt))
	    ((string-equal "Volume Control" buffname)
	     (all-the-icons-faicon "volume-up" :face 'all-the-icons-purple-alt))
	    ((file-directory-p buffname)
	     (all-the-icons-faicon "folder-open" :face 'all-the-icons-yellow))
	    ((string-suffix-p " - mpv" buffname)
	     (all-the-icons-faicon "play" :face 'all-the-icons-orange))
	    ((string-suffix-p "\.java" buffname)
	     (all-the-icons-alltheicon "java" :face 'all-the-icons-orange))
	    ((or(string-equal "st" buffname) (string-prefix-p (concat (user-login-name) "@") buffname) (string-prefix-p "root@" buffname))
	     (all-the-icons-faicon "terminal" :face 'all-the-icons-green))
	    )))



(defun all-the-icons-ivy--buffer-transformer (b s)
  "Return a candidate string for buffer B named S preceded by an icon.
Try to find the icon for the buffer's B `major-mode'.
If that fails look for an icon for the mode that the `major-mode' is derived from."
  (let ((mode (buffer-local-value 'major-mode b))
	(buffname (replace-regexp-in-string "<.*>$" "" s)))
    (format (concat "%s" all-the-icons-spacer "%s")
            (propertize "\t" 'display (or
                                       (all-the-icons-ivy--icon-for-mode mode)
                                       (all-the-icons-ivy--icon-for-mode (get mode 'derived-mode-parent))
				       (all-the-icons-ivy--icon-for-firefox mode buffname)
				       (all-the-icons-ivy--icon-for-tor mode buffname)
				       (all-the-icons-ivy--icon-for-exwm mode buffname)
                                       (funcall
                                        all-the-icons-ivy-family-fallback-for-buffer
                                        all-the-icons-ivy-name-fallback-for-buffer)))
            (all-the-icons-ivy--buffer-propertize b s))))

(use-package swiper
  :ensure t
  :defer t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;; IDO
;;; enable ido mode
(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

;;; ido vertical
(use-package ido-vertical-mode
  :ensure t
  :defer t
  :init
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;;; smex
(use-package amx
  :ensure t
  :defer t
  :init (amx-initialize)
  :config
  (setq amx-backend 'ivy
	-show-key-bindings t)
  :bind
  ("M-x" . amx))

;;; switch buffer
(global-set-key (kbd "C-x b") 'ido-switch-buffer)

(provide 'search)
