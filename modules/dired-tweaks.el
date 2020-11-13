;; Dired
;;; Colourful dired
(use-package diredfl
  :ensure t
  :defer t
  :init (diredfl-global-mode 1))

(use-package all-the-icons-dired
  :ensure t
  :defer t
  :config
  (all-the-icons-dired-mode 1))


(provide 'dired-tweaks)

