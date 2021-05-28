(defun xresources-theme-color (name)
  "Read the color NAME (e.g. color5) from the X resources."
  (shell-command-to-string (format
                "xrdb -q | grep \"%s\" | awk '{print $2}' | tr -d \"\\n\""
                   (concat "*\\(.*\\)" name ":")))
  )

(defun xresources-theme ()
  (interactive)
  (setq ;; foreground and background
      monokai-foreground     (xresources-theme-color "foreground")
      monokai-background     (xresources-theme-color "background")
      ;; highlights and comments
      monokai-comments       (xresources-theme-color "color2")
      monokai-emphasis       (xresources-theme-color "color10")
      monokai-highlight      (xresources-theme-color "color14")
      monokai-highlight-alt  (xresources-theme-color "color13")
      monokai-highlight-line (xresources-theme-color "color8")
      monokai-line-number    (xresources-theme-color "color15")
      ;; colours
      monokai-blue           (xresources-theme-color "color4")
      monokai-cyan           (xresources-theme-color "color6")
      monokai-green          (xresources-theme-color "color2")
      monokai-gray           (xresources-theme-color "color8")
      monokai-violet         (xresources-theme-color "color5")
      monokai-red            (xresources-theme-color "color1")
      monokai-orange         (xresources-theme-color "color3")
      monokai-yellow         (xresources-theme-color "color11"))
  (load-theme 'monokai t)
  )

(defun xmonokai-theme ()
  (interactive)
  (setq ;; foreground and background
      monokai-foreground     (xresources-theme-color "foreground")
      monokai-background     (xresources-theme-color "background"))
  (load-theme 'monokai t)
  )

;(defun xresources-theme-color (name)
;  "Read the color NAME (e.g. color5) from the X resources."
;  (x-get-resource name ""))


(defun sigusr1-handler ()
  (interactive)
  (message "Hello")
  (xmonokai-theme))
(define-key special-event-map [sigusr1] 'sigusr1-handler)

;(deftheme xresources "~/.Xresources as a theme")

;(provide-theme 'xresources)

(provide 'xresources-custom-theme)
;;; xresources-theme.el ends here
