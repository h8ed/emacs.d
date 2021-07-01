;;; icons
(setq use-package-always-ensure t)
(use-package quelpa-use-package)

(use-package expand-region)

(use-package all-the-icons)

(use-package dmenu)

(use-package dash)

(use-package dired+
  :load-path "~/.emacs.d/lisp/dired+.el")

(use-package aggressive-indent
  :hook ((prog-mode . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode)))

(use-package zoom
  ;;:load-path "/home/orion/.emacs.d/lisp/zoom"
  :config
  (zoom-mode 1)
  (custom-set-variables
   '(zoom-size '(0.618 . 0.618))
   '(zoom-ignored-major-modes '(dired-mode))))

;; completion
(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode))

(use-package ivy
  :config (ivy-mode 1))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package newlisp-mode
  :mode "\\.lsp\\'")

(use-package tex
  :defer t
  :ensure auctex
  :config
  ;;Eliminates the necessity for the save command before compilation is completed
  (setq TeX-save-query nil)

  ;;Function that combines two commands 1. revert pdfoutput buffer 2. pdf-outline
  (defun my-TeX-revert-document-buffer (file)
    (TeX-revert-document-buffer file)
    (pdf-outline))

  ;; Add custom function to the TeX compilation hook
  (add-hook 'TeX-after-compilation-finished-functions #'my-TeX-revert-document-buffer)
  (setq TeX-auto-save t))

;; (use-package haskell-mode
;; :defer t
;; :config (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode)))

(use-package lua-mode
  :defer t
  :config
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  (setq lua-indent-level 2
        lua-indent-nested-block-content-align nil))

(use-package fennel-mode
  :defer t
  :config (add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode)))

(use-package paredit)

(use-package emms
  :defer t
  :config
  (emms-all)
  (emms-default-players))

(use-package writeroom-mode
  :defer t
  :load-path "~/.emacs.d/lisp/writeroom-mode.el")

(use-package olivetti)

;; (use-package spaceship-mode
;;   :load-path "~/.emacs.d/spaceship-mode/spaceship-mode.el"
;;   :hook ((prog-mode . spaceship-mode)
;;          (emacs-lisp-mode . spaceship-mode)
;;          (startup-mode . spaceship-mode)
;;          (text-mode . spaceship-mode)))

;; (use-package tabble-mode
;;   :load-path "~/.emacs.d/spaceship-mode/tabble-mode.el"
;;   :config
;;   (add-hook 'spaceship-mode-hook (lambda () (tabble-mode 1))))

(use-package htmlize :defer t)

(use-package bitlbee :defer t)

(use-package hl-todo
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("##"         font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package org-starless
  :quelpa (org-starless :repo "TonCherAmi/org-starless" :fetcher github))

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

(provide 'pack)
