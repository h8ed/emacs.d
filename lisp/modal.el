(require 'functions)

(general-emacs-define-key global-map
  "C-;" 'ctrlf-backward-literal
  "C-'" 'ctrlf-forward-literal
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
 "n" 'xah-new-empty-buffer
 "C-<up>" 'o/varpitch
 "C-<down>" 'display-line-numbers-mode)

(defvar o/alter-cursor nil
  "Alter the cursor style when `nav-mode' is active?")
(defvar o/cursor-type-active 'box
  "The cursor style to be used when nav-mode is active (if
`nav/alter-cursor' is non-nil).")
(defvar o/cursor-type-inactive cursor-type
  "The cursor style to be used when nav-mode is not active (if
`nav/alter-cursor' is non-nil).")

(define-minor-mode oh-mode
  "Orion's ersonal modal editing paradigm
By default, use ESC to enter, and `f' to exit the mode
\\{(oh-mode-map}
"
  nil " Oh"
  '(((kbd "a") . execute-extended-command)
    ((kbd "b") . o/goto-matching-bracket)
    ((kbd "c") . kill-ring-save)
    ((kbd "d") . o/delete)
    ((kbd "e") . er/expand-region)
    ((kbd "f") . o/exit-oh-mode)
    ((kbd "F") . o/exit-oh-mode-front)
    ((kbd "h") . backward-char)
    ((kbd "H") . beginning-of-defun)
    ((kbd "i") . undo)
    ((kbd "j") . next-line)
    ((kbd "J") . forward-paragraph)
    ((kbd "k") . previous-line)
    ((kbd "K") . backward-paragraph)
    ((kbd "l") . forward-char)
    ((kbd "L") . end-of-defun)
    ((kbd "m") . jump-to-register)
    ((kbd "n") . o/jump-to-char)
    ((kbd "o") . o/forward-word)
    ((kbd "O") . o/forward-word-active)
    ((kbd "q") . o/quit)
    ((kbd "r") . o/forward-right-bracket)
    ((kbd "s") . o/newline)
    ((kbd "S") . o/newline-up)
    ((kbd "t") . set-mark-command)
    ((kbd "T") . rectangle-mark-mode)
    ((kbd "u") . o/backward-word)
    ((kbd "U") . o/backward-word-active)
    ((kbd "v") . yank)
    ((kbd "w") . o/backward-left-bracket)
    ((kbd "x") . o/select-line)
    ((kbd "y") . o/toggle-letter-case)
    ((kbd "z") . comment-line)
    ((kbd ".") . o/end-of-line-or-block)
    ((kbd ",") . o/beginning-of-line-or-block)
    ((kbd "/") . avy-goto-char-timer)
    ((kbd "(") . o/wrap-sexp)
    ((kbd "\\") . o/split-sexp)
    ((kbd "[") . o/wrap-square)
    ((kbd "{") . o/wrap-curly)
    ((kbd "-") . o/backward-punt)
    ((kbd "=") . o/forward-punt)
    ((kbd "<") . paredit-wrap-angled)
    ((kbd "'") . copy-to-register)
    ((kbd "\"") . point-to-register)
    ((kbd "0") . [?\M-0])
    ((kbd "1") . [?\M-1])
    ((kbd "2") . [?\M-2])
    ((kbd "3") . [?\M-3])
    ((kbd "4") . [?\M-4])
    ((kbd "5") . [?\M-5])
    ((kbd "6") . [?\M-6])
    ((kbd "7") . [?\M-7])
    ((kbd "8") . [?\M-8])
    ((kbd "9") . [?\M-9]))
  :group 'oh-mode-group)

(general-def oh-mode-map
  :prefix "g"
  "g" 'move-to-window-line-top-bottom
  "h" 'o/line-first-non-whitespace-char
  "j" 'end-of-buffer
  "k" 'beginning-of-buffer
  "l" 'move-end-of-line)

(general-def oh-mode-map
  :prefix "SPC"
  "SPC" 'o/spc-spc
  "h" 'o/select-to-bol
  "j" 'end-of-buffer
  "k" 'beginning-of-buffer
  "l" 'o/select-to-eol
  "F" 'mc/edit-lines
  "g" 'goto-line
  "a" 'back-to-indentation
  "v" 'insert-register)

(general-def oh-mode-map
  :prefix "SPC e"
  "b" 'eval-buffer
  "e" 'eval-last-sexp)

(general-def oh-mode-map
  :prefix "SPC f"
  "f" 'find-file
  "j" 'split-window-below
  "k" 'split-window-right
  "h" 'describe-function)

(general-def oh-mode-map
  :prefix "SPC r"
  "f" 'o/rename-file-and-buffer)

(general-def oh-mode-map
  :prefix "SPC q"
  "q" 'delete-window
  "w" 'kill-buffer)

(general-def oh-mode-map
  :prefix "SPC s"
  "t" 'switch-to-buffer
  "s" 'save-buffer
  "q" 'save-buffers-kill-emacs)

(general-def oh-mode-map
  :prefix "SPC i"
  "w" 'er/mark-word
  "d" 'er/mark-defun
  "s" 'er/mark-symbol
  "'" 'er/mark-inside-quotes
  "(" 'er/mark-inside-pairs)

(global-set-key (kbd "<escape>") 'o/enter-oh-mode)

(provide 'modal)
