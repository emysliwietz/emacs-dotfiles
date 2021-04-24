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
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))

(use-package ivy-rich
  :ensure t
  :defer t
  :init
  (ivy-rich-mode 1)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-path-style 'abbrev))

(use-package all-the-icons-ivy-rich
  :ensure t
  :defer t
  :init (all-the-icons-ivy-rich-mode 1))

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
(use-package smex
  :ensure t
  :defer t
  :init (smex-initialize)
  :bind
  ("M-x" . smex))

;;; switch buffer
(global-set-key (kbd "C-x b") 'ido-switch-buffer)

(provide 'search)
