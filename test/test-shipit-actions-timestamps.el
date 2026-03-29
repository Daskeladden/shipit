;;; test-shipit-actions-timestamps.el --- Tests for actions timestamp gap coloring -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'shipit-actions)

(ert-deftest test-gap-face-below-threshold ()
  "GIVEN a delta below the minimum threshold
   WHEN shipit-actions--gap-face is called
   THEN it returns the default shipit-timestamp-face."
  (should (eq (shipit-actions--gap-face 0) 'shipit-timestamp-face))
  (should (eq (shipit-actions--gap-face 4) 'shipit-timestamp-face)))

(ert-deftest test-gap-face-mild-threshold ()
  "GIVEN a delta at or above 5s but below 30s
   WHEN shipit-actions--gap-face is called
   THEN it returns shipit-timestamp-gap-mild-face."
  (should (eq (shipit-actions--gap-face 5) 'shipit-timestamp-gap-mild-face))
  (should (eq (shipit-actions--gap-face 29) 'shipit-timestamp-gap-mild-face)))

(ert-deftest test-gap-face-moderate-threshold ()
  "GIVEN a delta at or above 30s but below 300s
   WHEN shipit-actions--gap-face is called
   THEN it returns shipit-timestamp-gap-moderate-face."
  (should (eq (shipit-actions--gap-face 30) 'shipit-timestamp-gap-moderate-face))
  (should (eq (shipit-actions--gap-face 299) 'shipit-timestamp-gap-moderate-face)))

(ert-deftest test-gap-face-high-threshold ()
  "GIVEN a delta at or above 300s but below 1800s
   WHEN shipit-actions--gap-face is called
   THEN it returns shipit-timestamp-gap-high-face."
  (should (eq (shipit-actions--gap-face 300) 'shipit-timestamp-gap-high-face))
  (should (eq (shipit-actions--gap-face 1799) 'shipit-timestamp-gap-high-face)))

(ert-deftest test-gap-face-extreme-threshold ()
  "GIVEN a delta at or above 1800s
   WHEN shipit-actions--gap-face is called
   THEN it returns shipit-timestamp-gap-extreme-face."
  (should (eq (shipit-actions--gap-face 1800) 'shipit-timestamp-gap-extreme-face))
  (should (eq (shipit-actions--gap-face 9000) 'shipit-timestamp-gap-extreme-face)))

(ert-deftest test-gap-face-negative-delta ()
  "GIVEN a negative delta (clock skew between CI runners)
   WHEN shipit-actions--gap-face is called
   THEN it returns the default shipit-timestamp-face."
  (should (eq (shipit-actions--gap-face -5) 'shipit-timestamp-face))
  (should (eq (shipit-actions--gap-face -100) 'shipit-timestamp-face)))

(ert-deftest test-gap-face-custom-thresholds ()
  "GIVEN a custom threshold list
   WHEN shipit-actions--gap-face is called
   THEN it uses the custom thresholds."
  (let ((shipit-actions-timestamp-gap-thresholds
         '((10 . shipit-timestamp-gap-mild-face)
           (60 . shipit-timestamp-gap-extreme-face))))
    (should (eq (shipit-actions--gap-face 5) 'shipit-timestamp-face))
    (should (eq (shipit-actions--gap-face 10) 'shipit-timestamp-gap-mild-face))
    (should (eq (shipit-actions--gap-face 60) 'shipit-timestamp-gap-extreme-face))))

(ert-deftest test-timestamp-to-seconds-iso8601 ()
  "GIVEN a standard ISO 8601 timestamp with fractional seconds
   WHEN shipit-actions--timestamp-to-seconds is called
   THEN it returns a float representing epoch seconds."
  (let ((result (shipit-actions--timestamp-to-seconds
                 "2026-03-21T14:24:18.2526703Z")))
    (should (floatp result))
    (should (> result 0))))

(ert-deftest test-timestamp-to-seconds-no-fractional ()
  "GIVEN an ISO 8601 timestamp without fractional seconds
   WHEN shipit-actions--timestamp-to-seconds is called
   THEN it returns a float."
  (let ((result (shipit-actions--timestamp-to-seconds
                 "2026-03-21T14:24:18Z")))
    (should (floatp result))
    (should (> result 0))))

(ert-deftest test-timestamp-to-seconds-nil ()
  "GIVEN a nil timestamp
   WHEN shipit-actions--timestamp-to-seconds is called
   THEN it returns nil."
  (should (null (shipit-actions--timestamp-to-seconds nil))))

(ert-deftest test-timestamp-delta ()
  "GIVEN two timestamps 2.5 hours apart
   WHEN their seconds values are subtracted
   THEN the delta is approximately 9032 seconds."
  (let* ((t1 (shipit-actions--timestamp-to-seconds
              "2026-03-21T14:24:18.2526703Z"))
         (t2 (shipit-actions--timestamp-to-seconds
              "2026-03-21T16:54:50.7869966Z"))
         (delta (- t2 t1)))
    (should (> delta 9030))
    (should (< delta 9035))))

(ert-deftest test-insert-log-lines-gap-coloring ()
  "GIVEN log lines with a 2.5-hour gap and gap coloring enabled
   WHEN shipit-actions--insert-log-lines is called
   THEN the first timestamp uses default face
   AND the second timestamp uses the extreme gap face."
  (with-temp-buffer
    (let ((shipit-actions--show-timestamps t)
          (shipit-actions--show-gap-coloring t)
          (lines '(("2026-03-21T14:24:18.2526703Z" . "line one")
                   ("2026-03-21T16:54:50.7869966Z" . "line two"))))
      (shipit-actions--insert-log-lines lines "  ")
      ;; Collect all face values from timestamp regions
      (goto-char (point-min))
      (let (faces)
        (while (not (eobp))
          (let ((pos (text-property-any (line-beginning-position)
                                        (line-end-position)
                                        'face 'shipit-timestamp-face)))
            (unless pos
              (setq pos (text-property-any (line-beginning-position)
                                           (line-end-position)
                                           'face 'shipit-timestamp-gap-extreme-face)))
            (when pos
              (push (get-text-property pos 'face) faces)))
          (forward-line 1))
        (setq faces (nreverse faces))
        ;; First line: default face (no prev-ts)
        (should (eq (nth 0 faces) 'shipit-timestamp-face))
        ;; Second line: extreme gap face (2.5h delta)
        (should (eq (nth 1 faces) 'shipit-timestamp-gap-extreme-face))))))

(ert-deftest test-insert-log-lines-gap-coloring-off ()
  "GIVEN log lines with a large gap but gap coloring disabled
   WHEN shipit-actions--insert-log-lines is called
   THEN all timestamps use the default shipit-timestamp-face."
  (with-temp-buffer
    (let ((shipit-actions--show-timestamps t)
          (shipit-actions--show-gap-coloring nil)
          (lines '(("2026-03-21T14:24:18.2526703Z" . "line one")
                   ("2026-03-21T16:54:50.7869966Z" . "line two"))))
      (shipit-actions--insert-log-lines lines "  ")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((pos (text-property-any (line-beginning-position)
                                      (line-end-position)
                                      'face 'shipit-timestamp-gap-extreme-face)))
          ;; No line should have the extreme face
          (should-not pos))
        (forward-line 1)))))

(ert-deftest test-timestamps-transient-defined ()
  "GIVEN the shipit-actions package is loaded
   WHEN checking for the timestamps transient
   THEN shipit-actions-timestamps-menu is a command."
  (should (commandp 'shipit-actions-timestamps-menu)))

(ert-deftest test-l-key-bound-to-transient ()
  "GIVEN shipit-actions-mode-map
   WHEN looking up the L key
   THEN it is bound to shipit-actions-timestamps-menu."
  (should (eq (lookup-key shipit-actions-mode-map (kbd "L"))
              'shipit-actions-timestamps-menu)))

(provide 'test-shipit-actions-timestamps)
;;; test-shipit-actions-timestamps.el ends here
