(use-package tablist)

(use-package pdf-tools
  :load-path "/home/orion/.emacs.d/lisp/pdf-tools/lisp/"
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
			  ("j" . pdf-view-next-line-or-next-page)
			  ("k" . pdf-view-previous-line-or-previous-page)
			  ("J" . forward-page)
			  ("K" . backward-page)
			  ("=" . pdf-view-enlarge)
			  ("-" . pdf-view-shrink)
			  ("w" . pdf-view-fit-width-to-window)
			  ("h" . pdf-view-fit-height-to-window)
			  ("p" . pdf-view-fit-page-to-window)
			  ("/" . isearch-forward))
  :config
  (use-package pdf-occur
    :commands (pdf-occur-global-minor-mode))
  (use-package pdf-history
    :commands (pdf-history-minor-mode))
  (use-package pdf-links
    :commands (pdf-links-minor-mode))
  (use-package pdf-outline
    :commands (pdf-outline-minor-mode))
  (use-package pdf-annot
    :commands (pdf-annot-minor-mode))
  (use-package pdf-sync
    :commands (pdf-sync-minor-mode))
  (pdf-tools-install :no-query)
  (require 'pdf-occur))

(provide 'pdf-images)
