(use-package org-ref
  :ensure t
  :config
  (setq org-ref-bibliography-notes "~/Uni/dox/uni.org"
      org-ref-default-bibliography '("~/Uni/dox/uni.bib")
      org-ref-pdf-directory "~/Uni/dox/pdf/"))

(setq org-startup-with-inline-images t)

(provide 'org-latex-tweaks)
