;; General interface settings
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq scroll-conservatively 100)

(defun visual-tweaks ()
  (when window-system (progn
			(global-hl-line-mode t)
			(add-hook 'eshell-mode-hook (lambda ()
						      (setq-local global-hl-line-mode
								  nil)))
			(add-hook 'term-mode-hook (lambda ()
						    (setq-local global-hl-line-mode
								nil)))
			(global-prettify-symbols-mode)
			(tool-bar-mode -1)
			(scroll-bar-mode -1)
			(menu-bar-mode 0)
			(setq uniquify-buffer-name-style 'forward)
			)))

;; Pretty-symbols
(use-package pretty-mode
  :ensure t
  :defer 1
  :config (add-hook 'prog-mode-hook 'pretty-mode))

;; Theme
;;; Monokai
(use-package monokai-theme
  :ensure t)

(load-theme 'monokai t)

;; Modeline
;;; Spaceline
(use-package spaceline
  :ensure t
  :defer 1
  :config
  (make-thread (visual-tweaks))
  (defered-loading)
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow))
  (spaceline-spacemacs-theme))

;;; diminish modes
(use-package diminish
  :ensure t
  :defer 1
  :init
  (diminish 'hungry-delete-mode)
  (diminish 'beacon-mode)
  (diminish 'rainbow-mode)
  (diminish 'which-key-mode)
  (diminish 'company-mode)
  (diminish 'undo-tree-mode)
  (diminish 'flycheck-mode)
  (diminish 'yas-minor-mode)
  (diminish 'auto-complete-mode)
  (diminish 'subword-mode))

;;; show lines and columns
(line-number-mode 1)
(column-number-mode 1)

;;; Clock
;(setq display-time-24hr-format t)
;(display-time-mode 1)

;;; Battery indicator
;(use-package fancy-battery
;  :ensure t
;  :defer 1
;  :config
;  (setq fancy-battery-show-percentage t)
;  (setq battery-update-interval 15)
;  (if window-system
;      (fancy-battery-mode)
;    (display-battery-mode)))

;;; Nyan cat

(use-package nyan-mode
  :ensure t
  :defer t
  :init
  (nyan-mode)
  (nyan-start-animation))

;; Highlighting
;;; rainbow
  (use-package rainbow-mode
    :ensure t
    :defer t
    :init (add-hook 'prog-mode-hook 'rainbow-mode))

;;; beacon
(use-package beacon
  :defer t
  :ensure t
  :init
  (beacon-mode 1))

;;; Rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;;; Visual line mode
(global-visual-line-mode 1)

;; Startup
;;; Dashboard
(use-package dashboard
  :ensure t
  :defer t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)))
  (setq dashboard-banner-logo-title "")
  )

(global-display-line-numbers-mode t)
(setq-default display-line-numbers-width 4)

(provide 'interface)
