(global-unset-key (kbd "C-u"))
(global-set-key (kbd "<escape>") 'god-local-mode)

(general-define-key
 :keymaps 'god-local-mode-map
 "h" 'backward-char
 "j" 'next-line
 "k" 'previous-line
 "l" 'forward-char
 "f" 'god-local-mode
 "e" 'er/expand-region
 "s" 'o/newline
 "S" 'o/newline-up
 "t" 'set-mark-command
 "T" 'rectangle-mark-mode
 "u" 'kakoune-backward-same-syntax :first '(kakoune-set-mark-here)
 "U" 'kakoune-backward-same-syntax :first '(kakoune-set-mark-if-inactive)
 "o" 'forward-same-syntax :first '(kakoune-set-mark-here)
 "O" 'forward-same-syntax :first '(kakoune-set-mark-if-inactive))

(general-define-key
 :prefix "C-c"
 "C-h" 'windmove-left
 "C-j" 'windmove-down
 "C-k" 'windmove-up
 "C-l" 'windmove-right
 "C-a" 'ace-window
 "C-g" 'god-mode
 "C-d" 'er/mark-defun
 "C-p" 'er/mark-inside-pairs
 "C-q" 'er/mark-inside-quotes
 "C-x" 'kill-region
 "C-c" 'kill-ring-save
 "C-v" 'yank
 "C-z" 'comment-line)

(general-define-key
 :prefix "C-x"
 "x" 'kakoune-x)

(general-def :keymaps 'god-local-mode-map "SPC" nil)
(general-define-key
 :prefix "SPC"
 :keymaps 'god-local-mode-map
 "h" 'move-beginning-of-line
 "l" 'move-end-of-line
 "j" 'end-of-buffer
 "k" 'beginning-of-buffer)

(add-hook 'god-local-mode-hook
          (lambda ()
            (setq cursor-type (if overwrite-mode t 'bar))))

(provide 'demigod)
