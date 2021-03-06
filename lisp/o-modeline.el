;; -*- lexical-binding: t; -*-

(use-package moody :demand t)
(use-package dash :demand t)
(use-package dash-functional :demand t)

(display-time-mode -1)
(display-battery-mode -1)

(defun vz/mode-line-file-state ()
  (if (buffer-file-name)
      (cond
       (buffer-read-only    " [!]")
       ((buffer-modified-p) " [+]")
       (t                   ""))
    ""))

(defun vz/mode-line-file-short-dir ()
  (if-let ((it (if (derived-mode-p 'comint-mode)
                   (concat default-directory "/a")
                 (buffer-file-name))))
      (let* ((dir (->>
                   (f-dirname it)
                   (f-short)
                   (f-split)))
             (length (1- (length dir))))
        (concat
         (->>
          dir
          (-map-indexed (fn (if (eq <1> length)
                                <2>
                              (substring <2> 0 1))))
          (apply #'f-join)
          (f-short))
         "|"))
    ""))

(defun vz/mode-line-git-branch ()
  (if-let ((branch (when (or (buffer-file-name)
                             (derived-mode-p 'comint-mode))
                     (car (vc-git-branches)))))
      (format "(%s)" branch)
    ""))

;; From https://0x0.st/oYX8
(defun vz/mode-line-fill (face except)
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin)
                                             ,except)))
              'face face))

(defun vz/mode-line-roundise-text (text &optional foreground background)
  "Return an image object with TEXT surrounded by arcs on either side."
  (require 'svg)
  (if (s-blank? text)
      ""
    (let* ((f (font-at 0 nil "l"))
           (fw (window-font-width nil 'mode-line))
           (h (window-font-height nil 'mode-line))
           (w (+ h (* fw (string-width text))))
           (svg (svg-create w h)))
      (svg-rectangle svg 0 0 w h
                     :rx (/ h 2)
                     :ry (/ h 2)
                     :fill (or background
                               (if (moody-window-active-p) vz/mode-line-bg vz/mode-line-bgi)))
      (svg-text svg (format " %s " text)
                :font-family (font-get f :family)
                :font-size   (font-get f :size)
                :font-weight (font-get f :weight)
                :fill (or foreground
                          (if (moody-window-active-p) vz/mode-line-fg vz/mode-line-fgi))
                :x (/ h 2)
                :y (1+ (font-get f :size)))
      (propertize " " 'display (svg-image svg :ascent 'center)))))

;; (setq-default
;; vz/mode-line-format `("  "
;; (:eval (vz/mode-line-roundise-text (format-mode-line "｢%*｣")))
;; (:eval (vz/mode-line-roundise-text (format-mode-line "｢%b ⟶ (%l,%c)｣"))))
;; mode-line-format vz/mode-line-format)

(provide 'o-modeline)
