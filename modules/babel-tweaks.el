;; Run/highlight code using babel in org-mode

(use-package ob-ipython
   :ensure t
   :defer t
   )

(use-package ob-latex-as-png
   :ensure t
   :defer t
   )

(use-package ob-async
   :ensure t
   :defer t
   )



(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (ipython . t)
   (latex-as-png . t)
   (shell . t)
   ;; Include other languages here...
   ))
;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)
;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)
;; Fix an incompatibility between the ob-async and ob-ipython packages
(setq ob-async-no-async-languages-alist '("ipython"))

(provide 'babel-tweaks)
