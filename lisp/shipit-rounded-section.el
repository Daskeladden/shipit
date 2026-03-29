;;; shipit-rounded-section.el --- Rounded background for magit sections -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 shipit contributors

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:
;; Renders rounded background panels behind magit section content using
;; SVG edge pieces for corners and overlays for the fill.

;;; Code:
(require 'svg)

(defcustom shipit-rounded-section-bg-color "#1e3550"
  "Background color for rounded section backgrounds.
Used for inline comments and other highlighted sections."
  :type 'color
  :group 'shipit)

(defun shipit-rounded--make-edge-svg (height radius color position side)
  "Create SVG edge piece.
HEIGHT/RADIUS in pixels, COLOR for fill.
POSITION is top, bottom, middle, or single.
SIDE is left or right."
  (let* ((width (+ radius 2))
         (svg (svg-create width height))
         (w width) (h height) (r radius))
    (pcase (cons position side)
      ('(top . left)
       (dom-append-child svg
        (dom-node 'path `((d . ,(format "M0,%d Q0,0 %d,0 L%d,0 L%d,%d L0,%d Z"
                                        r r w w h h))
                          (fill . ,color)))))
      ('(top . right)
       (dom-append-child svg
        (dom-node 'path `((d . ,(format "M0,0 L%d,0 Q%d,0 %d,%d L%d,%d L0,%d Z"
                                        (- w r) w w r w h h))
                          (fill . ,color)))))
      ('(bottom . left)
       (dom-append-child svg
        (dom-node 'path `((d . ,(format "M0,0 L%d,0 L%d,%d L%d,%d Q0,%d 0,%d Z"
                                        w w h r h h (- h r)))
                          (fill . ,color)))))
      ('(bottom . right)
       (dom-append-child svg
        (dom-node 'path `((d . ,(format "M0,0 L%d,0 L%d,%d Q%d,%d %d,%d L0,%d Z"
                                        w w (- h r) w h (- w r) h h))
                          (fill . ,color)))))
      ('(single . left)
       (dom-append-child svg
        (dom-node 'path `((d . ,(format "M0,%d Q0,0 %d,0 L%d,0 L%d,%d L%d,%d Q0,%d 0,%d Z"
                                        r r w w h r h h (- h r)))
                          (fill . ,color)))))
      ('(single . right)
       (dom-append-child svg
        (dom-node 'path `((d . ,(format "M0,0 L%d,0 Q%d,0 %d,%d L%d,%d Q%d,%d %d,%d L0,%d Z"
                                        (- w r) w w r w (- h r) w h (- w r) h h))
                          (fill . ,color)))))
      (_
       (svg-rectangle svg 0 0 w h :fill color)))
    (svg-image svg :ascent 'center)))

(defun shipit-rounded--make-fill-svg (width height color)
  "Create solid rectangle SVG WIDTH x HEIGHT with COLOR."
  (let ((svg (svg-create width height)))
    (svg-rectangle svg 0 0 width height :fill color)
    (svg-image svg :ascent 'center)))

(defun shipit-rounded--extract-icon-color (section)
  "Extract the icon color from SECTION's heading text.
Returns the color string or nil."
  (let ((start (oref section start))
        (content (oref section content))
        (result nil))
    (when (and start content)
      (let ((pos start))
        (while (and pos (< pos content) (not result))
          (setq result (get-text-property pos 'shipit-icon-color))
          (unless result
            (setq pos (next-single-property-change pos 'shipit-icon-color nil content))))))
    result))

(defun shipit-rounded--icon-color-to-bg (icon-color)
  "Derive a subtle dark background color from ICON-COLOR.
Blends the icon color with the default background at ~12% opacity."
  (let* ((bg (face-background 'default nil 'default))
         (bg-rgb (color-name-to-rgb (or bg "#000000")))
         (fg-rgb (color-name-to-rgb icon-color))
         (blend 0.12))
    (format "#%02x%02x%02x"
            (round (* 255 (+ (* blend (nth 0 fg-rgb)) (* (- 1 blend) (nth 0 bg-rgb)))))
            (round (* 255 (+ (* blend (nth 1 fg-rgb)) (* (- 1 blend) (nth 1 bg-rgb)))))
            (round (* 255 (+ (* blend (nth 2 fg-rgb)) (* (- 1 blend) (nth 2 bg-rgb))))))))

(defun shipit-rounded--remove-bg (start end)
  "Remove all rounded background overlays/properties between START and END."
  (remove-overlays start end 'shipit-rounded-bg t)
  (remove-text-properties start end '(line-prefix nil wrap-prefix nil)))

(defun shipit-rounded--apply-bg (start end bg-color &optional _content-cols)
  "Apply rounded background from START to END with BG-COLOR.
Uses left SVG edge pieces for rounded corners and :extend t for
the background fill to the window edge."
  (when (display-graphic-p)
    (let* ((char-height (frame-char-height))
           (radius 10))
      (shipit-rounded--remove-bg start end)
      (save-excursion
        (goto-char start)
        (let ((lines '()))
          (while (< (point) end)
            (push (cons (line-beginning-position) (line-end-position)) lines)
            (forward-line 1))
          (setq lines (nreverse lines))
          (let* ((count (length lines))
                 (last-idx (1- count)))
            (cl-loop for (lstart . lend) in lines
                     for idx from 0
                     do
                     (let ((position (cond
                                      ((and (= idx 0) (= idx last-idx)) 'single)
                                      ((= idx 0) 'top)
                                      ((= idx last-idx) 'bottom)
                                      (t 'middle))))
                       ;; Left rounded edge
                       (put-text-property lstart (1+ lstart) 'line-prefix
                                          (propertize " " 'display
                                                      (shipit-rounded--make-edge-svg
                                                       char-height radius bg-color position 'left)))
                       (put-text-property lstart (1+ lstart) 'wrap-prefix
                                          (propertize " " 'display
                                                      (shipit-rounded--make-edge-svg
                                                       char-height radius bg-color 'middle 'left)))
                       ;; Background with :extend t fills to window edge
                       ;; Include newline char so :extend t works on every line
                       (let ((ov (make-overlay lstart (min (1+ (max lend lstart)) (point-max)))))
                         (overlay-put ov 'face `(:background ,bg-color :extend t))
                         (overlay-put ov 'evaporate t)
                         (overlay-put ov 'shipit-rounded-bg t)))))))
)))

(defun shipit-rounded--update-section (section bg-color &optional content-cols)
  "Update rounded background for body content of magit SECTION.
The heading (with icon) is left unstyled. BG-COLOR is the pill background."
  (when (display-graphic-p)
    (let* ((inhibit-read-only t)
           (sect-start (oref section start))
           (sect-end (oref section end))
           (content-start (oref section content))
           (hidden (oref section hidden)))
      (when sect-start
        (shipit-rounded--remove-bg sect-start sect-end)
        ;; Only apply pill to body content (not heading)
        (unless hidden
          (when (and content-start (< content-start sect-end))
            (shipit-rounded--apply-bg content-start sect-end bg-color content-cols)))))))

(defun shipit-rounded--kill-highlight-overlays ()
  "Remove magit-section-highlight overlays via timer (after magit applies them)."
  (let ((buf (current-buffer)))
    (run-with-timer 0 nil
                    (lambda ()
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (dolist (ov (overlays-in (point-min) (point-max)))
                            (when (eq (overlay-get ov 'face) 'magit-section-highlight)
                              (delete-overlay ov)))))))))

;;; Auto-apply via advice on magit-section-show/hide

(defun shipit-rounded--apply-to-section-now (section)
  "Apply rounded background to SECTION if it has icon-color.
Used by both the advice and the walk-and-apply functions."
  (let ((icon-color (shipit-rounded--extract-icon-color section)))
    (when icon-color
      (let* ((inhibit-read-only t)
             (bg-color shipit-rounded-section-bg-color)
             (sect-start (oref section start))
             (own-end (shipit-rounded--own-content-end section)))
        (shipit-rounded--remove-bg sect-start own-end)
        (when (< sect-start own-end)
          (shipit-rounded--apply-bg sect-start own-end bg-color))))))

(defun shipit-rounded--after-section-show (section)
  "Apply rounded background after SECTION is shown, including children.
Advice for `magit-section-show'."
  (condition-case err
      (when (and (display-graphic-p)
                 section
                 (eieio-object-p section)
                 (slot-boundp section 'start)
                 (oref section start))
        (shipit-rounded--apply-to-section-now section)
        ;; Also re-apply to visible children
        (dolist (child (oref section children))
          (unless (oref child hidden)
            (shipit-rounded--apply-to-section-now child))))
    (error
     (when (fboundp 'shipit--debug-log)
       (shipit--debug-log "Rounded bg error: %s" (error-message-string err))))))

(defun shipit-rounded--after-section-hide (section)
  "Resize rounded background to heading-only after SECTION is hidden.
Advice for `magit-section-hide'."
  (condition-case nil
      (when (and section
                 (eieio-object-p section)
                 (slot-boundp section 'start)
                 (oref section start))
        (let ((icon-color (shipit-rounded--extract-icon-color section)))
          (when icon-color
            (let* ((inhibit-read-only t)
                   (bg-color shipit-rounded-section-bg-color)
                   (sect-start (oref section start))
                   (sect-end (oref section end))
                   (content-start (oref section content)))
              (shipit-rounded--remove-bg sect-start sect-end)
              (when (and content-start (< sect-start content-start))
                (shipit-rounded--apply-bg sect-start content-start bg-color))))))
    (error nil)))

(defun shipit-rounded--own-content-end (section)
  "Return the end of SECTION's own content, before any child sections.
Falls back to section end or `point' if no children."
  (let ((sect-end (or (oref section end) (point))))
    (if (oref section children)
        (let ((first-child-start (oref (car (oref section children)) start)))
          (if (and first-child-start (< first-child-start sect-end))
              first-child-start
            sect-end))
      sect-end)))

(defun shipit-rounded--apply-to-section (section icon-color)
  "Apply rounded background to SECTION's own content using ICON-COLOR.
Covers the heading and body but stops before child sections, so
each comment gets its own independent background.
Defers application to after buffer construction completes."
  (when (and (display-graphic-p)
             section
             (eieio-object-p section)
             (slot-boundp section 'start)
             (oref section start))
    (let ((buf (current-buffer))
          (sect section))
      (run-with-timer
       0 nil
       (lambda ()
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (condition-case err
                 (let* ((inhibit-read-only t)
                        (bg-color shipit-rounded-section-bg-color)
                        (sect-start (oref sect start))
                        (own-end (shipit-rounded--own-content-end sect)))
                   (shipit-rounded--remove-bg sect-start own-end)
                   (when (< sect-start own-end)
                     (shipit-rounded--apply-bg sect-start own-end bg-color)))
               (error
                (when (fboundp 'shipit--debug-log)
                  (shipit--debug-log "Rounded apply-to-section error: %s"
                                     (error-message-string err))))))))))))

(defun shipit-rounded--apply-to-visible-sections ()
  "Apply rounded backgrounds to all currently expanded sections.
Call after initial buffer render to handle sections that start expanded."
  (when (and (display-graphic-p) (bound-and-true-p magit-root-section))
    (let ((inhibit-read-only t))
      (shipit-rounded--walk-and-apply magit-root-section))))

(defun shipit-rounded--walk-and-apply (section)
  "Walk SECTION tree, applying bg to expanded sections with icon colors."
  (unless (oref section hidden)
    (shipit-rounded--apply-to-section-now section)
    (dolist (child (oref section children))
      (shipit-rounded--walk-and-apply child))))

(advice-add 'magit-section-show :after #'shipit-rounded--after-section-show)
(advice-add 'magit-section-hide :after #'shipit-rounded--after-section-hide)

(provide 'shipit-rounded-section)
;;; shipit-rounded-section.el ends here
