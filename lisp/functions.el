;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My personal functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun o/ryo-stick ()
  (interactive)
  (if (ryo-modal-mode)
      '(nil)
    (ryo-modal-mode 1)))

(defun o/enter-oh-mode ()
  (interactive)
  (if (bound-and-true-p oh-mode)
      '(nil)
    (progn
      (setq cursor-type 'box)
      (oh-mode))))

(defun o/exit-oh-mode ()
  (interactive)
  (setq cursor-type 'bar)
  (oh-mode -1))

(defun o/exit-oh-mode-front ()
  (interactive)
  (setq cursor-type 'bar)
  (forward-char)
  (oh-mode -1))

(defun o/quit ()
  (interactive)
  (execute-kbd-macro (kbd "C-g")))

(defun o/newline ()
  (interactive)
  (end-of-line)
  (newline))

(defun o/newline-up ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1))

(defun o/insert-forward ()
  (interactive)
  (forward-char)
  (ryo-modal-mode -1))

(defun o/select-to-eol ()
  (interactive)
  (kakoune-set-mark-if-inactive)
  (move-end-of-line 1))

(defun o/select-to-bol ()
  (interactive)
  (kakoune-set-mark-if-inactive)
  (move-beginning-of-line 1))

(defun o/wrap-fun-paren ()
  (interactive)
  (ryo-modal-mode -1)
  (er/mark-defun)
  (execute-kbd-macro (kbd "(")))

(defun o/wrap-fun-sqr-bracket ()
  (interactive)
  (ryo-modal-mode -1)
  (progn (er/mark-defun)
         (execute-kbd-macro (kbd "["))
         (end-of-defun)
         (beginning-of-defun)
         (xah-backward-left-bracket)
         (move-end-of-line 1)
         (execute-kbd-macro (kbd "]"))))

(defun o/wrap-fun-curly-bracket ()
  (interactive)
  (ryo-modal-mode -1)
  (progn (er/mark-defun)
         (execute-kbd-macro (kbd "{"))
         (end-of-defun)
         (beginning-of-defun)
         (xah-backward-left-bracket)
         (move-end-of-line 1)
         (execute-kbd-macro (kbd "}"))))

(defun o/delete ()
  (interactive)
  (if (use-region-p)
	  (xah-cut-line-or-region)
	(delete-char 1)))

(defun o/voldown ()
  (interactive)
  (shell-command "pamixer -d 5"))
(defun o/volup ()
  (interactive)
  (shell-command "pamixer -i 5"))
(global-set-key (kbd "<XF86AudioLowerVolume>") 'o/voldown)
(global-set-key (kbd "<XF86AudioRaiseVolume>") 'o/volup)

(defun o/jump-to-char (arg char)
  (interactive "p\ncSelect to char: ")
  (setq kakoune-last-char-selected-to char)
  (setq kakoune-last-t-or-f ?f)
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
	    (search-forward (char-to-string char) nil nil arg))
    (point)))

(defun o/spc-spc ()
  (interactive)
  (if (use-region-p)
      (exchange-point-and-mark)
    '(nil)))

(defun o/set-mark-if-inactive ()
  (interactive)
  (unless (use-region-p) (set-mark (point))))

(defun o/set-mark-here ()
  (interactive) (set-mark (point)))

(defun o/backward-word ()
  (interactive)
  (o/set-mark-here)
  (kakoune-backward-same-syntax 1))

(defun o/forward-word ()
  (interactive)
  (o/set-mark-here)
  (forward-same-syntax))

(defun o/backward-word-active ()
  (interactive)
  (o/set-mark-if-inactive)
  (kakoune-backward-same-syntax 1))

(defun o/forward-word-active ()
  (interactive)
  (o/set-mark-if-inactive)
  (forward-same-syntax))

(defun o/varpitch ()
  (interactive)
  (if (bound-and-true-p org-mode)
	  (progn (variable-pitch-mode)
	         (visual-line-mode)
	         (org-bullets-mode))
	'(nil)))

(defun o/line-first-non-whitespace-char ()
  (interactive)
  (if (bound-and-true-p visual-line-mode)
      (beginning-of-visual-line)
    (back-to-indentation)))

(defun o/select-line (count)
  (interactive "p")
  (beginning-of-line)
  (set-mark (point))
  (forward-line count))

(defun o/goto-matching-bracket ()
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at (regexp-opt xah-left-brackets))
      (forward-sexp))
     ((looking-back (regexp-opt xah-right-brackets) (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

(defun o/backward-left-bracket ()
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun o/forward-right-bracket ()
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))

(defun o/toggle-letter-case ()
  (interactive)
  (let (
        (deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alpha:]")
        (setq $p1 (point))
        (skip-chars-forward "[:alpha:]")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1 (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

(defun o/beginning-of-line-or-block ()
  (interactive)
  (let (($p (point)))
    (if (or (equal (point) (line-beginning-position))
            (eq last-command this-command))
        (if (re-search-backward "\n[\t\n ]*\n+" nil "move")
            (progn
              (skip-chars-backward "\n\t ")
              ;; (forward-char )
              )
          (goto-char (point-min)))
      (progn
        (back-to-indentation)
        (when (eq $p (point))
          (beginning-of-line))))))

(defun o/end-of-line-or-block ()
  (interactive)
  (if (or (equal (point) (line-end-position))
          (eq last-command this-command))
      (progn
        (re-search-forward "\n[\t\n ]*\n+" nil "move" ))
    (end-of-line)))

(defun o/wrap-sexp (&optional argument open close)
  (interactive "P")
  (paredit-lose-if-not-in-sexp 'paredit-wrap-sexp)
  (let ((open (or open ?\( ))
        (close (or close ?\) )))
    (paredit-handle-sexp-errors
        ((lambda (n) (paredit-insert-pair n open close 'goto-char))
         (cond ((integerp argument) argument)
               ((consp argument) (paredit-count-sexps-forward))
               ((paredit-region-active-p) nil)
               (t 1)))
      (insert close)
      (backward-char)))
  (save-excursion (backward-up-list) (indent-sexp)))

(defun o/split-sexp ()
  (interactive)
  (cond ((paredit-in-string-p)
         (insert "\"")
         (save-excursion (insert " \"")))
        ((or (paredit-in-comment-p)
             (paredit-in-char-p))
         (error "Invalid context for splitting S-expression."))
        (t
         (let ((open (save-excursion (backward-up-list) (char-after)))
               (close (save-excursion (up-list) (char-before))))
           (delete-horizontal-space)
           (insert close)
           (save-excursion
             (insert ?\ )
             (insert open)
             (backward-char)
             (indent-sexp))))))

(defvar o/punctuation-regex nil "A regex string for the purpose of moving cursor to a punctuation.")
(setq o/punctuation-regex "[!\?\"\.,`'#$%&*+:;=@^|~]+")

(defun o/forward-punct (&optional n)
  (interactive "p")
  (re-search-forward xah-punctuation-regex nil t n))

(defun o/backward-punct (&optional n)
  (interactive "p")
  (re-search-backward xah-punctuation-regex nil t n))

(defun gk-pop-shell (arg)
  (interactive "P")
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      (let ((prefix-arg arg))
        (call-interactively #'shell))
      (current-buffer))
    '((side . bottom)))))
(global-set-key (kbd "M-c") 'gk-pop-shell)

(defun o/insert-column-az ()
  (interactive)
  (let (
        ($startChar (string-to-char (read-string "Start char: " "a")))
        ($howmany (string-to-number (read-string "How many: " "26")))
        ($colpos (- (point) (line-beginning-position))))
    (dotimes ($i $howmany )
      (progn
        (insert-char (+ $i $startChar))
        (forward-line)
        (beginning-of-line)
        (forward-char $colpos)))))
;; (global-set-key (kbd "C-c z") 'xah-insert-column-az)

(defun o/rename-file-and-buffer (new-name)
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
;; (global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(provide 'functions)
