;; Everything in this file is completely public domain.
;; Anyone may use it for whatever they'd like.

;; The main goal of this configuration is to
;; utilize both the benefits of placement-based
;; bindings like XFK, while keeping the vim
;; movement keys (hjkl), and mnemonic leader
;; keybinds. Everything is meant to be configured
;; to *my* liking, so there may be some things
;; others would like to tweak to their own preference.

;; 
;; #505075 , #76848F , #BFBFBF
;;

;;; -- startup ops -- ;;;

;; temporarily reduce garbage collection during startup. Inspect `gcs-done'.
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

;;; -- the basics -- ;;;

;; use-package bootstrap
(package-initialize)

(require 'package) ;; emacs builtin

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(unless package-archive-contents
  (package-refresh-contents))

(setq my-package-list '(use-package))

(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

(set-frame-font "IBM Plex Mono 9" nil t)
;; (set-frame-font "Linux Libertine 12" nil t)
;; ;; (set-cursor-color "#3387a1")
(set-cursor-color "#e36387")

;; source local files
(add-to-list
 'load-path (expand-file-name "~/.emacs.d/lisp/"))

(add-to-list
 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

(add-to-list
 'load-path (expand-file-name "~/.emacs.d/spaceship-mode/"))

;; (add-to-list 'default-frame-alist '(minibuffer . nil))

;; require packages from load path
(require 'pack) ; misc packages
;(require 'pdf-images)
(require 'functions) ; personal functions
(require 'org-conf)

;; basic changes
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(fringe-mode 0)
;;(xclip-mode 1)
(electric-pair-mode)
(electric-indent-mode -1)
;; (add-hook
;;  'after-change-major-mode-hook 'o/text-mode-font)
(add-hook
 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))
(global-hl-line-mode 1)
(add-hook 'c-mode-hook 'o/c-tabs)
(emms-all)
(emms-default-players)

(defun simple-mode-line-render (left right)
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

;; set variables
(setq-default
 initial-buffer-choice '*scratch*

 mode-line-format nil
 ;; mode-line-format '((:eval (simple-mode-line-render
 ;; left
 ;; (format-mode-line "｢%*｣")
 ;; right
 ;; (format-mode-line "%b ⟶ (%l,%c) "))))

 display-line-numbers-type 'nil
 ;; display-line-numbers-current-absolute t
 ;; display-line-numbers-width 4
 ;; display-line-numbers-widen t

 column-number-mode t

 cursor-in-non-selected-windows nil
 cursor-type 'bar
 frame-resize-pixelwise t
 highlight-indent-guides-method 'bitmap

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

 c-default-style "bsd"

 require-final-newline t

 enable-local-eval t

 indent-tabs-mode nil
 tab-width 4

 load-prefer-newer t

 custom-file (concat user-emacs-directory "/custom.el")

 ;; default-minibuffer-frame (make-frame
 ;; '((name . "minibuffer")
 ;; (width . 80)
 ;; (height . 5)
 ;; (minibuffer . only)
 ;; (top . 0)
 ;; (left . 0)))
 ;; new-frame (make-frame
 ;; '((name . "editor")
 ;; (width . 80)
 ;; (height . 30)
 ;; (minibuffer . nil)
 ;; (top . 50)
 ;; (left . 0)))
 ;; new-frame (make-frame
 ;; '((name . "eterm")
 ;; (width . 80)
 ;; (height . 30)
 ;; (minibuffer . nil)
 ;; (top . 50)
 ;; (left . 0)))

 eshell-prompt-regexp "^[^#$\n]*[#$] "
 eshell-prompt-function
 (lambda nil
   (concat
	(if (string= (eshell/pwd) (getenv "HOME"))
	    "~" (eshell/basename (eshell/pwd)))
	" "
	(if (= (user-uid) 0) "# " "$ ")))
 )

(show-paren-mode 1)

(zoom-mode 1)
(custom-set-variables
 '(zoom-size '(0.618 . 0.618)))

(defalias 'yes-or-no-p 'y-or-n-p)

;; UI changes
(load-theme 'muted t)
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

;;; -- spaceship mode - proportional fonts with proper alignment -- ;;;
;; (require 'spaceship-mode)
;; (require 'tabble-mode)

;; (spaceship-mode 1)
;; (tabble-mode 1)

