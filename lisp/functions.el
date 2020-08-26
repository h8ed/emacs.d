;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; My personal functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun o/ryo-stick ()
  (interactive)
  (if (ryo-modal-mode)
	  '(nil)
    (ryo-modal-mode 1)))

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

(defun gk-pop-shell (arg)
  "Pop a shell in a side window.
Pass arg to ‘shell’."
  (interactive "P")
  (select-window
   (display-buffer-in-side-window
    (save-window-excursion
      (let ((prefix-arg arg))
        (call-interactively #'shell))
      (current-buffer))
    '((side . bottom)))))
(global-set-key (kbd "M-c") 'gk-pop-shell)

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

;; Print screen
;; (global-set-key
;;  (kbd "<print>")
;;  (lambda ()
;;    (interactive)
;;    (let ((path (concat "~/Documents/Screenshot-" (format-time-string "%Y-%m-%d,%H:%M:%S") ".png")))
;; 	 (start-process-shell-command
;; 	  "scrot" nil (concat "scrot -s -f " path))
;; 	 (message (concat "Screenshot saved to " path)))))

;; (defun x11-yank-image-at-point-as-image ()
;; "Yank the image at point to the X11 clipboard as image/png."
;; (interactive)
;; (let ((image (get-text-property (point) 'display)))
;; (if (eq (car image) 'image)
;; (let ((data (plist-get (cdr image) ':data))
;; (file (plist-get (cdr image) ':file)))
;; (cond (data
;; (with-temp-buffer
;; (insert data)
;; (call-shell-region
;; (point-min) (point-max)
;; "xclip -i -selection clipboard -t image/png")))
;; (file
;; (if (file-exists-p file)
;; (start-process
;; "xclip-proc" nil "xclip"
;; "-i" "-selection" "clipboard" "-t" "image/png"
;; "-quiet" (file-truename file))))
;; (t (message "The image seems to be malformed."))))
;; (message "Point is not at an image."))))

;; (global-set-key (kbd "M-p") 'x11-yank-image-at-point-as-image)

(defun xah-insert-column-az ()
  "Insert letters A to Z vertically, similar to `rectangle-number-lines'.
The commpand will prompt for a start char, and number of chars to insert.
The start char can be any char in Unicode.
URL `http://ergoemacs.org/emacs/emacs_insert-alphabets.html'
Version 2019-03-07"
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

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
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

(defun o/varpitch ()
  (interactive)
  (if (bound-and-true-p org-mode)
	  ((variable-pitch-mode)
	   (visual-line-mode)
	   (org-bullets-mode))
	'(nil)))

(provide 'functions)
