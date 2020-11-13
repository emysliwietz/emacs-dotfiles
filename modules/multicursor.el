(use-package multiple-cursors
  :ensure t
  :defer t
  :init 
  (global-set-key (kbd "C-q") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))


(provide 'multicursor)
