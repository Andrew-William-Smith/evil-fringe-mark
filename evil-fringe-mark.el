;;; evil-fringe-mark.el --- Display evil-mode marks in the Emacs fringe
;; This file is not part of GNU Emacs.

;; Copyright (C) 2018 Andrew Smith

;; Author: Andrew Smith <andy.bill.smith@gmail.com>
;; URL: https://github.com/Andrew-William-Smith/
;; Version: 1.0.0
;; Package-Requires: ((emacs "21.0"))

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


;;; Code:
(require 'cl-lib)
(require 'evil)
(require 'fringe-helper)
(require 'evil-fringe-mark-overlays)

(make-variable-buffer-local
 (defvar evil-fringe-marks '()
   "Plist of fringe overlays for buffer-local marks."))

(defvar evil-fringe-file-marks '()
  "Plist of fringe overlays for file marks.")

(defvar evil-fringe-mark-side 'left-fringe
  "Fringe in which to place mark overlays.")

(defun evil-fringe-mark-put (char char-list marker)
  "Place a mark indicator in the fringe."
  (set char-list (plist-put (symbol-value char-list) char
                            (fringe-helper-insert
                             (alist-get char evil-fringe-mark-bitmaps) marker
                             evil-fringe-mark-side 'font-lock-keyword-face))))

(defun evil-fringe-mark-delete (char)
  "Delete the specified mark indicator from the fringe."
  (let ((char-list (if (> char 96)
                       (setq char-list 'evil-fringe-marks)
                     (setq char-list 'evil-fringe-file-marks))))
    (fringe-helper-remove (plist-get (symbol-value char-list) char))
    (set char-list (evil-plist-delete char (symbol-value char-list)))))

(defun evil-fringe-marks-refresh-buffer ()
  "Redraw all mark indicators in the current buffer."
  ; Local marks
  (cl-loop for (char . marker) in evil-markers-alist do
           (if (markerp marker)
               (if (> char 96)
                   (evil-fringe-mark-put char 'evil-fringe-marks marker))))
  ; Global marks
  (cl-loop for (char . marker) in (default-value 'evil-markers-alist) do
           (if (markerp marker)
               (if (and (> char 64)
                        (equal (current-buffer) (marker-buffer marker)))
                   (evil-fringe-mark-put char 'evil-fringe-file-marks marker)))))

(defun evil-fringe-marks-clear-buffer ()
  "Delete all mark indicators from the current buffer."
  ; Local marks
  (cl-loop for key in evil-fringe-marks do
           (if (numberp key)
             (evil-fringe-mark-delete key)))
  ; Global marks
  (cl-loop for key in evil-fringe-file-marks do
           (if (and (numberp key)
                    (equal (current-buffer) (overlay-buffer
                                             (plist-get evil-fringe-file-marks key))))
               (evil-fringe-mark-delete key))))

(defadvice evil-set-marker (around compile)
  (let ((char      (ad-get-arg 0))
        (marker    ad-do-it)
        (char-list nil)
        (old-mark  nil))
    (progn
      (if (> char 96)
          (setq char-list 'evil-fringe-marks)        ; Lowercase (local) mark
        (setq char-list 'evil-fringe-file-marks))    ; Uppercase (global) mark
      (setq old-mark (plist-get (symbol-value char-list) char))
      (if old-mark
          (fringe-helper-remove old-mark))
      (if (or evil-fringe-mark-mode global-evil-fringe-mark-mode)
          (evil-fringe-mark-put char char-list marker)))))

(defadvice evil-delete-marks (after compile)
  (evil-fringe-marks-clear-buffer)
  (evil-fringe-marks-refresh-buffer))

(define-minor-mode evil-fringe-mark-mode
  "Display evil-mode marks in the fringe."
  :lighter " EFM"
  (if evil-fringe-mark-mode
      (progn
        (evil-fringe-marks-refresh-buffer)
        (ad-activate 'evil-set-marker)
        (ad-activate 'evil-delete-marks))
    (progn
      (evil-fringe-marks-clear-buffer)
      (ad-deactivate 'evil-set-marker)
      (ad-deactivate 'evil-delete-marks))))

(define-minor-mode global-evil-fringe-mark-mode
  "Display evil-mode marks in the fringe.  Global version of `evil-fringe-mark-mode'."
  :lighter " gEFM"
  :global t
  (if global-evil-fringe-mark-mode
      (progn
        ; Place all marks
        (save-current-buffer
          (cl-loop for buf in (buffer-list) do
                   (set-buffer buf)
                   (evil-fringe-marks-refresh-buffer)))
        (ad-activate 'evil-set-marker)
        (ad-activate 'evil-delete-marks))
    (progn
      ; Remove all buffer-local marks
      (save-current-buffer
        (cl-loop for buf in (buffer-list) do
                 (set-buffer buf)
                 (evil-fringe-marks-clear-buffer)))
      ; Remove all global marks
      (ad-deactivate 'evil-set-marker)
      (ad-deactivate 'evil-delete-marks))))

(provide 'evil-fringe-mark)
;;; evil-fringe-mark.el ends here
