(general-emacs-define-key global-map
  "C-;" 'isearch-backward
  "C-'" 'isearch-forward
  "<M-up>" 'drag-stuff-up
  "<M-down>" 'drag-stuff-down)

(general-translate-key nil 'dired-mode-map
  "j" "n"
  "k" "p")

(general-define-key
 :prefix "C-c"
 "h" 'windmove-left
 "j" 'windmove-down
 "k" 'windmove-up
 "l" 'windmove-right
 "a" 'ace-window
 "g" 'god-mode
 "d" 'er/mark-defun
 "p" 'er/mark-inside-pairs
 "q" 'er/mark-inside-quotes
 "m" 'treemacs
 "." 'xah-new-empty-buffer
 "C-<up>" 'o/varpitch
 "C-<down>" 'display-line-numbers-mode)

(use-package ryo-modal
  :commands ryo-modal-mode
  :bind ("<escape>" . o/ryo-stick)
  :config
  (ryo-modal-key
   "SPC" '(("c" "C-c")
		   ("x" "C-x")
		   ("h" move-beginning-of-line)
		   ("j" end-of-buffer)
		   ("k" beginning-of-buffer)
		   ("l" move-end-of-line)
		   ("e"
			(("b" eval-buffer)
			 ("e" eval-last-sexp)))
		   ("F" mc/edit-lines)
		   ("f"
            (("f" find-file)
             ("j" split-window-below)
             ("k" split-window-right)
			 ("h" describe-function)))
		   ("g" goto-line)
		   ("a" back-to-indentation)
		   ("q"
            (("q" delete-window)
             ("w" kill-buffer)))
		   ("s"
            (("t" switch-to-buffer)
             ("s" save-buffer)
			 ("q" save-buffers-kill-emacs)))))
  (ryo-modal-keys
   ("a" "M-x")
   ("b" xah-goto-matching-bracket)
   ("c" "M-w")
   ("d" o/delete)
   ("e" er/expand-region)
   ("F" o/insert-forward)
   ("f" ryo-modal-mode)
   ("g"
	(("j"
	  (("n" paredit-join-with-next-list)
	   ("p" paredit-join-with-previous-list)))))
   ("h" backward-char)
   ("H" beginning-of-defun)
   ("i" undo)
   ("J" forward-paragraph)
   ("j" next-line)
   ("K" backward-paragraph)
   ("k" previous-line)
   ("L" end-of-defun)
   ("l" forward-char)
   ("n" ryo-modal-repeat)
   ("o" forward-same-syntax :first '(kakoune-set-mark-here))
   ("O" forward-same-syntax :first '(kakoune-set-mark-if-inactive))
   ("q" o/quit)
   ("r" xah-forward-right-bracket)
   ("s" o/newline)
   ("S" o/newline-up)
   ("T" rectangle-mark-mode)
   ("t" set-mark-command)
   ("u" kakoune-backward-same-syntax :first '(kakoune-set-mark-here))
   ("U" kakoune-backward-same-syntax :first '(kakoune-set-mark-if-inactive))
   ("v" "C-y")
   ("w" xah-backward-left-bracket)
   ("x" kakoune-x)
   ("y" xah-toggle-letter-case)
   ("z" comment-line)
   ("," xah-beginning-of-line-or-block)
   ("." xah-end-of-line-or-block)
   ("(" paredit-wrap-sexp)
   (")" paredit-split-sexp)
   ("[" paredit-wrap-square)
   ("{" paredit-wrap-curly)
   ("<" paredit-wrap-angled)
   ("/" avy-goto-char-timer)
   ("'" xah-cycle-hyphen-underscore-space)
   ("\"" xah-shrink-whitespaces)
   ("-" xah-backward-punct)
   ("=" xah-forward-punct)
   ("m" ace-window))
  (ryo-modal-keys
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9")))

;; Colemak

;; (general-emacs-define-key global-map
;; "C-o" 'isearch-backward
;; "C-'" 'isearch-forward
;; "<M-up>" 'drag-stuff-up
;; "<M-down>" 'drag-stuff-down)

;; (general-translate-key nil 'dired-mode-map
;; "n" "n"
;; "e" "p")

;; (general-define-key
;; :prefix "C-c"
;; "k" 'windmove-left
;; "n" 'windmove-down
;; "e" 'windmove-up
;; "i" 'windmove-right
;; "a" 'ace-window
;; "g" 'god-mode
;; "d" 'er/mark-defun
;; "p" 'er/mark-inside-pairs
;; "q" 'er/mark-inside-quotes
;; "m" 'treemacs
;; "." 'xah-new-empty-buffer
;; "C-<up>" 'o/varpitch
;; "C-<down>" 'display-line-numbers-mode)

;; (use-package ryo-modal
;;   :commands ryo-modal-mode
;;   :bind ("<escape>" . o/ryo-stick)
;;   :config
;;   (ryo-modal-key
;;    "SPC" '(("c" "C-c")
;; 		   ("x" "C-x")
;; 		   ("k" move-beginning-of-line)
;; 		   ("n" end-of-buffer)
;; 		   ("e" beginning-of-buffer)
;; 		   ("i" move-end-of-line)
;; 		   ("f"
;; 			(("b" eval-buffer)
;; 			 ("e" eval-last-sexp)))
;; 		   ("F" mc/edit-lines)
;; 		   ("t"
;;             (("f" find-file)
;;              ("j" split-window-below)
;;              ("k" split-window-right)
;; 			 ("h" describe-function)))
;; 		   ("g" goto-line)
;; 		   ("a" back-to-indentation)
;; 		   ("q"
;;             (("q" delete-window)
;;              ("w" kill-buffer)))
;; 		   ("s"
;;             (("t" switch-to-buffer)
;;              ("s" save-buffer)
;; 			 ("q" save-buffers-kill-emacs)))))
;;   (ryo-modal-keys
;;    ("a" "M-x")
;;    ("z" xah-goto-matching-bracket)
;;    ("d" "M-w")
;;    ("s" o/delete)
;;    ("f" er/expand-region)
;;    ("T" o/insert-forward)
;;    ("t" ryo-modal-mode)
;;    ("g"
;; 	(("f"
;; 	  (("n" paredit-join-with-next-list)
;; 	   ("p" paredit-join-with-previous-list)))))
;;    ("k" backward-char)
;;    ("H" beginning-of-defun)
;;    ("u" undo)
;;    ("N" forward-paragraph)
;;    ("n" next-line)
;;    ("E" backward-paragraph)
;;    ("e" previous-line)
;;    ("I" end-of-defun)
;;    ("i" forward-char)
;;    ("m" ryo-modal-repeat)
;;    ("y" forward-same-syntax :first '(kakoune-set-mark-here))
;;    ("Y" forward-same-syntax :first '(kakoune-set-mark-if-inactive))
;;    ("q" o/quit)
;;    ("p" xah-forward-right-bracket)
;;    ("r" o/newline)
;;    ("R" o/newline-up)
;;    ("B" rectangle-mark-mode)
;;    ("b" set-mark-command)
;;    ("l" kakoune-backward-same-syntax :first '(kakoune-set-mark-here))
;;    ("L" kakoune-backward-same-syntax :first '(kakoune-set-mark-if-inactive))
;;    ("v" "C-y")
;;    ("w" xah-backward-left-bracket)
;;    ("c" kakoune-x)
;;    ("j" xah-toggle-letter-case)
;;    ("x" comment-line)
;;    ("," xah-beginning-of-line-or-block)
;;    ("." xah-end-of-line-or-block)
;;    ("(" paredit-wrap-sexp)
;;    (")" paredit-split-sexp)
;;    ("[" paredit-wrap-square)
;;    ("{" paredit-wrap-curly)
;;    ("<" paredit-wrap-angled)
;;    ("/" avy-goto-char-timer)
;;    ("'" xah-cycle-hyphen-underscore-space)
;;    ("\"" xah-shrink-whitespaces)
;;    ("-" xah-backward-punct)
;;    ("=" xah-forward-punct)
;;    ("h" ace-window))
;;   (ryo-modal-keys
;;    (:norepeat t)
;;    ("0" "M-0")
;;    ("1" "M-1")
;;    ("2" "M-2")
;;    ("3" "M-3")
;;    ("4" "M-4")
;;    ("5" "M-5")
;;    ("6" "M-6")
;;    ("7" "M-7")
;;    ("8" "M-8")
;;    ("9" "M-9")))
(ryo-modal-mode 1)

(provide 'oh-mode)
