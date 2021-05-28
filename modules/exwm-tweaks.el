;; EXWM

(use-package exwm
  :ensure t
  :defer nil
  :config
  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default)
  (require 'exwm-randr)
  (cond ((string= (shell-command-to-string "hostname | tr -d '\n'") "astaroth")
	 (setq exwm-randr-workspace-output-plist '(1 "DP-1" 2 "HDMI-2" 3 "DVI-0"))
	 (add-hook 'exwm-randr-screen-change-hook
		   (lambda ()
		     (start-process-shell-command
		      "xrandr" nil "x randr --output eDP-1 --primary --mode 3840x2160 --pos 0x0 --rotate normal --output DP-1 --off --output HDMI-1 --off --output DP-2 --off --output HDMI-2 --scale 2x2 --mode 1920x1080 --pos 0x2160 --rotate normal"))) 
	 )((string= (shell-command-to-string "hostname | tr -d '\n'") "jarvis")
	   (setq exwm-randr-workspace-output-plist '(1 "DP-1" 2 "HDMI-2" 3 "DVI-1"))
	   (add-hook 'exwm-randr-screen-change-hook
		     (lambda ()
		       (start-process-shell-command
			"xrandr" nil "xrandr --output DisplayPort-0 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-0 --off --output DVI-0 --off --output DVI-1 --mode 1920x1080 --pos 0x0 --rotate normal")))
	   ))
  (exwm-randr-enable)
  (winner-mode t)
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (define-key exwm-mode-map (kbd "C-c") nil)
  (setq exwm-input-simulation-keys
	'(([?\C-b] . [left])
	  ([?\C-f] . [right])
	  ([?\C-p] . [up])
	  ([?\C-n] . [down])
	  ([?\C-a] . [home])
	  ([?\C-e] . [end])
	  ([?\M-a] . [C-a])
	  ([?\M-v] . [prior])
	  ([?\C-d] . [delete])
	  ([?\C-k] . [S-end delete])
	  ([?\C-w] . [?\C-x])
	  ([?\M-w] . [?\C-c])
	  ([?\C-y] . [?\C-v])
	  ;; search
	  ([?\C-s] . [?\C-f])
	  ([?\M-s] . [?\C-s])))
  (when (functionp 'exwm-enable-ido-workaround)
    (exwm-enable-ido-workaround))
  (with-eval-after-load 'ediff-wind
  (setq ediff-control-frame-parameters
	(cons '(unsplittable . t) ediff-control-frame-parameters)))
  
  (global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
;  (global-set-key (kbd "C-c m") 'toggle-maximize-buffer)


  (defun fullscreen ()
    (interactive)
    (if (eq major-mode 'exwm-mode)
      (call-interactively 'exwm-layout-toggle-fullscreen)      
      (toggle-maximize-buffer)
      ))

  (defun switchmonitor-next ()
    (interactive)
    (shell-command "xdotool mousemove_relative 1920 0"))

  (defun switchmonitor-prev ()
    (interactive)
    (shell-command "xdotool mousemove_relative -- -1920 0"))

  
  (setq exwm-workspace-number 9
        exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t
        exwm-manage-force-tiling t)
  (setq exwm-input-global-keys
      `(([?\s-r] . exwm-reset)
	([?\s-f] . fullscreen)
	([?\s-F] . toggle-maximize-buffer)
	([?\s-q] . kill-curr-buffer)
	([?\s-n] . switchmonitor-next)
	([?\s-p] . switchmonitor-prev)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))))
  (add-hook 'exwm-manage-finish-hook
          (lambda ()
            (if (and exwm-class-name
                       (string= exwm-class-name "St"))
              (progn
		(exwm-input-release-keyboard))
	      (progn))
	    (exwm-layout-hide-mode-line)))
  
(require 'exwm-edit)
(defun ag-exwm/on-exwm-edit-compose ()
  (funcall 'org-mode))
(add-hook 'exwm-edit-compose-hook 'ag-exwm/on-exwm-edit-compose)


(add-hook 'exwm-update-title-hook
          (lambda ()
              (exwm-workspace-rename-buffer exwm-title))))

(setq exwm-manage-configurations
      '(((equal exwm-class-name "gui.SmartcardGUI")
           floating t
           floating-mode-line nil
;           width 0.4
;           height 0.4
	   )))

(provide 'exwm-tweaks)
