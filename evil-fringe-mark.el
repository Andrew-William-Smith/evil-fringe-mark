;;; evil-fringe-mark.el --- Display evil-mode marks in the fringe
;; This file is not part of GNU Emacs.

;; Copyright (C) 2018 Andrew Smith

;; Author: Andrew Smith <andy.bill.smith@gmail.com>
;; URL: https://github.com/Andrew-William-Smith/evil-fringe-mark
;; Version: 1.0.1
;; Package-Requires: ((emacs "24.3"))

;; This file is part of evil-fringe-mark.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file provides `evil-fringe-mark', which in turn provides the minor
;; modes `evil-fringe-mark-mode' and `global-evil-fringe-mark-mode'.  To
;; enable either mode, run its respective command.  These modes display
;; fringe bitmaps representing all `evil-mode' marks within a buffer; the
;; fringe in which bitmap overlays are placed may be changed by modifying
;; the global variable `evil-fringe-mark-side'.


;;; Code:
(require 'cl-lib)
(require 'evil)
(require 'fringe-helper)
(require 'evil-fringe-mark-overlays)

(make-variable-buffer-local
 (defvar evil-fringe-mark-list '()
   "Plist of fringe overlays for buffer-local marks."))

(defvar evil-fringe-mark-file-list '()
  "Plist of fringe overlays for file marks.")

(defvar evil-fringe-mark-side 'left-fringe
  "Fringe in which to place mark overlays.")

(defun evil-fringe-mark-put (char char-list marker)
  "Place an indicator for mark CHAR, of type CHAR-LIST, in the fringe at location MARKER."
  (set char-list (plist-put (symbol-value char-list) char
                            (fringe-helper-insert
                             (cdr (assoc char evil-fringe-mark-bitmaps)) marker
                             evil-fringe-mark-side 'font-lock-keyword-face))))

(defun evil-fringe-mark-delete (char)
  "Delete the indicator for mark CHAR from the fringe."
  (let ((char-list (if (>= char ?a)
                       'evil-fringe-mark-list
                     'evil-fringe-mark-file-list)))
    (fringe-helper-remove (plist-get (symbol-value char-list) char))
    (set char-list (evil-plist-delete char (symbol-value char-list)))))

(defun evil-fringe-mark-refresh-buffer ()
  "Redraw all mark indicators in the current buffer."
  ; Local marks
  (cl-loop for (char . marker) in evil-markers-alist do
           (if (and (markerp marker)
                    (>= char ?a))
               (evil-fringe-mark-put char 'evil-fringe-mark-list marker)))
  ; Global marks
  (cl-loop for (char . marker) in (default-value 'evil-markers-alist) do
           (if (and (markerp marker)
                    (>= char ?A)
                    (eq (current-buffer) (marker-buffer marker)))
               (evil-fringe-mark-put char 'evil-fringe-mark-file-list marker))))

(defun evil-fringe-mark-clear-buffer ()
  "Delete all mark indicators from the current buffer."
  ; Local marks
  (cl-loop for key in evil-fringe-mark-list do
           (if (numberp key)
             (evil-fringe-mark-delete key)))
  ; Global marks
  (cl-loop for key in evil-fringe-mark-file-list do
           (if (and (numberp key)
                    (eq (current-buffer) (overlay-buffer
                                             (plist-get evil-fringe-mark-file-list key))))
               (evil-fringe-mark-delete key))))

(defadvice evil-set-marker (around compile)
  "Advice function for `evil-fringe-mark'."
  (let ((char      (ad-get-arg 0))
        (marker    ad-do-it)
        (char-list (if (>= char ?a)
                       'evil-fringe-mark-list         ; Lowercase (local) mark
                     'evil-fringe-mark-file-list))    ; Uppercase (global) mark
        (old-mark  nil))
    (progn
      (setq old-mark (plist-get (symbol-value char-list) char))
      (if old-mark
          (fringe-helper-remove old-mark))
      (if (or evil-fringe-mark-mode global-evil-fringe-mark-mode)
          (evil-fringe-mark-put char char-list marker)))))

(defadvice evil-delete-marks (after compile)
  "Advice function for `evil-fringe-mark'."
  (evil-fringe-mark-clear-buffer)
  (evil-fringe-mark-refresh-buffer))

;;;###autoload
(define-minor-mode evil-fringe-mark-mode
  "Display evil-mode marks in the fringe."
  :lighter " EFM"
  (if evil-fringe-mark-mode
      (progn
        (evil-fringe-mark-refresh-buffer)
        (ad-activate 'evil-set-marker)
        (ad-activate 'evil-delete-marks))
    (progn
      (evil-fringe-mark-clear-buffer)
      (ad-deactivate 'evil-set-marker)
      (ad-deactivate 'evil-delete-marks))))

;;;###autoload
(define-minor-mode global-evil-fringe-mark-mode
  "Display evil-mode marks in the fringe.  Global version of `evil-fringe-mark-mode'."
  :lighter " gEFM"
  :global t
  :require 'evil-fringe-mark
  (if global-evil-fringe-mark-mode
      (progn
        ; Place all marks
        (save-current-buffer
          (cl-loop for buf in (buffer-list) do
                   (set-buffer buf)
                   (evil-fringe-mark-refresh-buffer)))
        (ad-activate 'evil-set-marker)
        (ad-activate 'evil-delete-marks))
    (progn
      ; Remove all buffer-local marks
      (save-current-buffer
        (cl-loop for buf in (buffer-list) do
                 (set-buffer buf)
                 (evil-fringe-mark-clear-buffer)))
      ; Remove all global marks
      (ad-deactivate 'evil-set-marker)
      (ad-deactivate 'evil-delete-marks))))

(provide 'evil-fringe-mark)
;;; evil-fringe-mark.el ends here
