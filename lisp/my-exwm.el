;;; -*- lexical-binding: t; -*-
(use-package exwm
  :ensure t
  :config

  ;; necessary to configure exwm manually
  (require 'exwm-config)

  ;; fringe size, most people prefer 1 
  (fringe-mode 10)

  ;; emacs as a daemon, use "emacsclient <filename>" to seamlessly edit files from the terminal directly in the exwm instance
  (server-start)

  ;; this fixes issues with ido mode, if you use helm, get rid of it
  (exwm-config-ido)

  ;; a number between 1 and 9, exwm creates workspaces dynamically so I like starting out with 1
  (setq exwm-workspace-number 1)

  ;; this is a way to declare truly global/always working keybindings
  ;; this is a nifty way to go back from char mode to line mode without using the mouse
  (exwm-input-set-key (kbd "s-r") #'exwm-reset)
  (exwm-input-set-key (kbd "s-k") #'exwm-workspace-delete)
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-swap)

  ;; the next loop will bind s-<number> to switch to the corresponding workspace
  (setq exwm-workspace-index-map
		(lambda (index) (number-to-string (1+ index))))
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda ()
                           (interactive)
                           (exwm-workspace-switch-create (1- ,i)))))

  ;; the simplest launcher, I keep it in only if dmenu eventually stopped working or something
  (exwm-input-set-key (kbd "s-&")
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))

  ;; (exwm-input-set-key (kbd "s-*") #'dmenu)
  (exwm-input-set-key (kbd "s-*")
					  (lambda ()
						(interactive)
						(start-process-shell-command "rofi" nil "rofi -show run")))

  (exwm-input-set-key
   (kbd "M-p")
   (lambda ()
	 (interactive)
	 (require 'subr-x)
	 (call-interactively ("maim -s | xclip -i -selection clipboard -t image/png"))))

  ;; an easy way to make keybindings work *only* in line mode
  (push ?\C-q exwm-input-prefix-keys)
  (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

  ;; simulation keys are keys that exwm will send to the exwm buffer upon inputting a key combination
  (setq exwm-input-simulation-keys
		'(
		  ;; movement
		  ([?\C-b] . left)
		  ([?\M-b] . C-left)
		  ([?\C-f] . right)
		  ([?\M-f] . C-right)
		  ([?\C-p] . up)
		  ([?\C-n] . down)
		  ([?\C-a] . home)
		  ([?\C-e] . end)
		  ([?\M-v] . prior)
		  ([?\C-v] . next)
		  ([?\C-d] . delete)
		  ([?\C-k] . (S-end delete))
		  ;; cut/paste
		  ([?\C-w] . ?\C-x)
		  ([?\M-w] . ?\C-c)
		  ([?\C-y] . ?\C-v)
		  ;; search
		  ([?\C-s] . ?\C-f)))

  ;; this little bit will make sure that XF86 keys work in exwm buffers as well
  (dolist (k '(XF86AudioLowerVolume
               XF86AudioRaiseVolume
               XF86PowerOff
               XF86AudioMute
               XF86AudioPlay
               XF86AudioStop
               XF86AudioPrev
               XF86AudioNext
               XF86ScreenSaver
               XF68Back
               XF86Forward
               Scroll_Lock
               print))
    (cl-pushnew k exwm-input-prefix-keys))

  ;; this just enables exwm, it started automatically once everything is ready
  (exwm-enable))

(use-package dmenu)

(require 'exwm-systemtray)
(exwm-systemtray-enable)

(defun exwm-rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name ":"
           (if (<= (length exwm-title) 50) exwm-title
			 (concat (substring exwm-title 0 49) "...")))))

(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

(provide 'my-exwm)
