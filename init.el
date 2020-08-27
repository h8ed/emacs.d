;; ________  .__    _________                _____ 
;; \_____  \ |  |__ \_   ___ \  ____   _____/ ____\
;;  /   |   \|  |  \/    \  \/ /  _ \ /    \   __\ 
;; /    |    \   Y  \     \___(  <_> )   |  \  |   
;; \_______  /___|  /\______  /\____/|___|  /__|   
;;         \/     \/        \/            \/       

;; Everything in this file is completely public domain.
;; Anyone may use it for whatever they'd like.

;; The main goal of this configuration is to
;; utilize both the benefits of placement-based
;; bindings like XFK, while keeping the vim
;; movement keys (hjkl), and mnemonic leader
;; keybinds. Everything is meant to be configured
;; to *my* liking, so there may be some things
;; others would like to tweak to their own preference.

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- startup ops -- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Temporarily reduce garbage collection during startup. Inspect `gcs-done'.
(defun ambrevar/reset-gc-cons-threshold ()
  (setq gc-cons-threshold
        (car(get 'gc-cons-threshold 'standard-value))))
(setq gc-cons-threshold (* 64 1024 1024))
(add-hook 'after-init-hook #'ambrevar/reset-gc-cons-threshold)

;; Temporarily disable the file name handler.
(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)
(defun ambrevar/reset-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append default-file-name-handler-alist
                file-name-handler-alist))
  (cl-delete-duplicates file-name-handler-alist :test 'equal))
(add-hook 'after-init-hook #'ambrevar/reset-file-name-handler-alist)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; -- the basics -- ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; initialize packages
(package-initialize)

;; -- load packages, bootstrap use-package -- ;;
(require 'package) ;; Emacs builtin

;; set package.el repositories
(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

;; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; a list of pkgs to programmatically install
;; ensure installed via package.el
(setq my-package-list '(use-package))

;; programmatically install/ensure installed
;; pkgs in your personal list
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; source local files
(add-to-list
 'load-path (expand-file-name "~/.emacs.d/lisp/"))

(add-to-list
 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

(add-to-list
 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; require packages from load path
(require 'pack) ; misc packages for stuff i do with emacs .. duh
(require 'pdf-images) ; support for PDF and image viewing inside emacs
(require 'functions) ; personal functions
(require 'hydras) ; pseudo modal modes -- do i still need this?

;; basic changes
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(fringe-mode 0)
(xclip-mode 1)
(global-display-line-numbers-mode -1)
(electric-pair-mode)
(electric-indent-mode -1)
(add-hook
 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))
(global-hl-line-mode 1)
(emms-all)
(emms-default-players)

(defun simple-mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

;; set variables
(setq-default
 mode-line-format
 '((:eval (simple-mode-line-render
           ;; left
           ;; (format-mode-line "｢%*｣")
           (format-mode-line " ")
           ;; right
           (format-mode-line "%b ⟶ (%l,%c) "))))

 display-line-numbers-type 'relative
 ;; display-line-numbers-current-absolute t
 ;; display-line-numbers-width 4
 ;; display-line-numbers-widen t

 column-number-mode t

 cursor-in-non-selected-windows nil
 cursor-type 'bar
 show-parens-mode 1
 frame-resize-pixelwise t

 mouse-wheel-scroll-amount '(1)
 mouse-wheel-progressive-speed 'nil

 emms-source-file-default-directory "~/Music/"

 search-whitespace-regexp ".*"
 isearch-lax-whitespace t

 backup-by-copying t
 backup-directory-alist '((".*" . "~/.cache/emacs-backups/"))
 delete-old-versions t
 keep-new-versions 5
 keep-old-versions 2
 version-control t
 auto-save-file-name-transforms '((".*" "~/.cache/emacs-autosave/" t))
 auto-save-list-file-prefix "~/.cache/emacs-autosave/"
 create-lockfiles nil

 require-final-newline t

 enable-local-eval t

 indent-tabs-mode nil
 tab-width 4

 load-prefer-newer t

 custom-file (concat user-emacs-directory "/custom.el")
 explicit-shell-file-name "/home/orion/.nix-profile/bin/fish")

(defalias 'yes-or-no-p 'y-or-n-p)

;; UI changes
(load-theme 'xresources t)
(fringe-mode '(0 . 0))

;; Feeling some EXWM?
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)
;; (require 'my-exwm)
;; (defun wm-xmodmap ()
;; (call-process
;; "xmodmap" nil
;; (get-buffer-create "wm") nil (expand-file-name "~/.qwerty")))
;; (wm-xmodmap)
;; (require 'scrot)

;; Oh Mode - personal modal editing
(require 'modal)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "advanced" changes ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; change emacsclient `C-x k' functionality 
(add-hook 'server-switch-hook
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (when server-buffer-clients
              (local-set-key (kbd "C-x k") 'server-edit))))

;; set multiple variables with a common beginning pattern
(defmacro setq-ns (ns &rest args)
  "`setq' but with the namespace as NS.
If variable is a cons cell, then cdr is attached to setq.
For example, to set a buffer local variable, you pass the variable
name as (name-without-ns . local)."
  (declare (indent 1))
  `(progn
     ,@(mapcar
        (lambda (x)
          (let ((set 'setq)
                (var (car x)))
            (when (and (listp var) (not (listp (cdr var))))
              (setq set (intern (format "setq-%s" (cdr var)))
                    var (car var)))
            (list set
                  (intern (format "%s-%s" ns var)) (cadr x))))
        (seq-partition args 2))))

(defmacro setq-ens (ns &rest args)
  "`setq' but with the ending namespace as NS.
If variable is a cons cell, then cdr is attached to setq.
For example, to set a buffer local variable, you pass the variable
name as (name-without-ns . local)."
  (declare (indent 1))
  `(progn
     ,@(mapcar
        (lambda (x)
          (let ((set 'setq)
                (var (car x)))
            (when (and (listp var) (not (listp (cdr var))))
              (setq set (intern (format "setq-%s" (cdr var)))
                    var (car var)))
            (list set
                  (intern (format "%s-%s" var ns)) (cadr x))))
        (seq-partition args 2))))
