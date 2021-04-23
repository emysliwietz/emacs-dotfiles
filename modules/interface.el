
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
			(add-hook 'window-configuration-change-hook 'scratch-trans)

			(defun buffer-empty-p (&optional buffer)
			  (= (buffer-size buffer) 0))

			(defun frame-trans-on ()
			  (interactive)
			  (set-frame-parameter (selected-frame) 'alpha '(0 0)))

			(defun frame-trans-off ()
			  (interactive)
			  (set-frame-parameter (selected-frame) 'alpha '(100 100)))

			(defun scratch-trans ()
			  (setq my-buffer (get-buffer "*scratch*"))
			  (cond ((eq my-buffer (window-buffer (selected-window)))
				 (if (= (length (window-list)) 1) (frame-trans-on) (frame-trans-off)))
				((get-buffer-window my-buffer)
				 (frame-trans-off)) 
				(t
				 (frame-trans-off)))
			  )
			(tool-bar-mode 0)
			(scroll-bar-mode 0)
			(menu-bar-mode 0)
			(tooltip-mode 0)
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


(use-package all-the-icons)


;; Modeline
;;; Spaceline
(use-package spaceline
  :ensure t
  :config
  (make-thread (visual-tweaks))
  (defered-loading)
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow))
  (spaceline-define-segment all-the-icons
  "Inserts icon for buffer"
  (all-the-icons-icon-for-buffer)
  :tight t
  ))

(use-package spaceline-all-the-icons 
  :after spaceline
  :config (spaceline-all-the-icons-theme)
  (setq spaceline-all-the-icons-separator-type (quote arrow)))


(custom-set-faces
  '(mode-line ((t (:box (:line-width 1 :color "black"))))))
(setq modeline-height 10)


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
(defun highlight-visual-line ()
  (save-excursion
    (cons (progn (beginning-of-visual-line) (point))
          (progn (end-of-visual-line) (point)))))
(setq hl-line-range-function 'highlight-visual-line)
;;;; Highlight current row
(global-hl-line-mode 1)
;;;; To *visually* wrap lines
(global-visual-line-mode t)

(global-display-line-numbers-mode t)
(setq-default display-line-numbers-width 4)


;;; Unicode emojis
(if (>= emacs-major-version 27)
    (set-fontset-font t '(#x1f000 . #x1faff)
              (font-spec :family "Noto Color Emoji")))

;;;; setting up composition functions for emoji modifiers
(dolist (items `(((?🇦 . ?🇿) [".[🇦-🇿]+" 0 font-shape-gstring])
                 ((?🏳 . ?🏴) [".[️‍🌈⚧☠󠀠-󠁿]*" 0 font-shape-gstring])
                 (?⃣ ["[#*0-9]️⃣" 2 font-shape-gstring])
                 ;; TODO: I can't make keycap sequences work because I
                 ;; think they're trying to shape with the wrong font.
                 ,@(mapcar (lambda (range) (list range [".‍?[🏻-🏿]?[‍️♂♀]*️?" 0 font-shape-gstring]))
                           (concatenate 'list "☝🎅🏇👂👃👦👧👼💏💑💪🕴🕵🕺🖐🖕🖖🙇🚣🛀🛌🤏🤞🤟🤦🤽🤾🥷🦻👯❤"
                                        '((?⛹ . ?✍) (?🏂 . ?🏄) (?🏊 . ?🏌) (?👆 . ?👐)
                                          (?👫 . ?👮) (?👰 . ?👸) (?💁 . ?💇) (?🙅 . ?🙇) (?🙋 . ?🙏)
                                          (?🚴 . ?🚶) (?🤘 . ?🤜) (?🤰 . ?🤹) (?🤼 . ?🤾) (?🦵 . ?🦹)
                                          (?🧍 . ?🧏) (?🧒 . ?🧟))) )
                 (?🧑 [".‍?[🏻-🏿]?[‍⚕⚖✈❤️🌾🍳🍼🎄🎓🎤🎨🏫🏭👦-👩💋💻💼🔧🔬🚀🚒🤝🦯🦰-🦳🦼🦽🧑]*" 0 font-shape-gstring])
                 ((?👨 . ?👩) [".‍?[🏻-🏿]?[‍⚕⚖✈❤️🌾🍳🍼🎄🎓🎤🎨🏫🏭👦-👩💋💻💼🔧🔬🚀🚒🤝🦯🦰-🦳🦼🦽🧑]*" 0 font-shape-gstring])
                 ,@(mapcar (lambda (str) (list (elt str 0) (vector str 0 'font-shape-gstring)))
                           '("😶‍🌫️" "🐈‍⬛" "🐕‍🦺" "🐻‍❄️" "👁️‍🗨️" "😮‍💨" "😵‍💫"))))
  (set-char-table-range
   composition-function-table
   (car items)
   (list (cadr items))))

(provide 'interface)
