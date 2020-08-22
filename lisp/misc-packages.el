;; icons
(use-package all-the-icons)

;; dashboard
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq
   dashboard-startup-banner "/home/orion/pictures/darlinglogo.png"
   dashboard-banner-logo-title "Welcome, darling!"
   dashboard-center-content t
   dashboard-items '((recents . 5)
                     (bookmarks . 5))
   dashboard-set-heading-icons t
   dashboard-set-file-icons t
   dashboard-set-navigator t
   dashboard-navigator-buttons
   `(;; line1
     ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
       "Github"
       "My Github"
       (lambda (&rest _) (browse-url "https://github.com/h8ed"))))
     ;; line 2
     ((,(all-the-icons-faicon "youtube" :height 1.1 :v-adjust 0.0)
       "Youtube"
       ""
       (lambda (&rest _) (browse-url "https://youtube.com")))
      (,(all-the-icons-faicon "apple" :height 1.1 :v-adjust 0.0)
       "Discord"
       ""
       (lambda (&rest _) (browse-url "https://discord.com")))))))

(use-package show-paren
  :defer t
  :hook (prog-mode . show-paren-mode)
  :config
  (setq-ns show-paren
    delay 0
    when-point-inside-paren t))

(use-package aggressive-indent
  :ensure t
  :hook ((prog-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)))

(use-package zoom
  :load-path "/home/orion/.emacs.d/lisp/zoom"
  :config
  (zoom-mode 1)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))
   '(zoom-ignored-major-modes '(dired-mode))))

;; ace window
(use-package ace-window)

;; completion
(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode))

(use-package ivy)
(ivy-mode 1)

;; (use-package helm
;; :ensure t
;; :bind (("M-x" . helm-M-x))
;; :config
;; (helm-mode 1))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

(use-package treemacs :bind ("C-c n" . treemacs))

(use-package lua-mode
  :ensure t
  :hook (lua-mode . indent-tabs-mode)
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  (setq lua-indent-level 4
        lua-indent-nested-block-content-align nil))

(use-package paredit)

(use-package god-mode)

(use-package emms
  :config
  (emms-all)
  (emms-default-players))

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode)
  :config
  (setq
   simple-modeline-show-input-method nil
   simple-modeline-show-minor-modes nil))

(use-package writeroom-mode)

(use-package elcord)

(use-package org
  :hook ((org-mode . org-bullets-mode)
         (org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode))
  :config
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (let* ((variable-tuple
          (cond ((x-list-fonts "TeX Gyre Pagella")  '(:font "TeX Gyre Pagella"))
                ((x-list-fonts "IBM Plex Serif")  '(:font "IBM Plex Serif"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "TeX Gyre Pagella" :height 120))))
   '(fixed-pitch ((t ( :family "IBM Plex Mono" :height 100)))))

  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-default ((t (:inherit variable-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))))

(use-package htmlize :ensure t)

(use-package emacs
  :delight
  (ryo-modal-mode)
  (aggressive-indent-mode "/=>")
  (ivy-mode "/i")
  (company-mode "/c")
  (emacs-lisp-mode "EL")
  (lisp-interaction-mode "λ")
  (eldoc-mode)
  (yas-minor-mode)
  (writeroom-mode)
  (lua-mode "Lua")
  (buffer-face-mode "/bf")
  (org-mode "Org")
  (visual-line-mode "/↵"))

;; (use-package vterm
;; :load-path "/home/orion/.emacs.d/lisp/emacs-libvterm/")

(provide 'misc-packages)
