;;; emacspeak-tabulated-list.el --- Speech-enable   -*- lexical-binding: t; -*-
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable TABULATED-LIST 
;;; Keywords: Emacspeak,  Audio Desktop tabulated-list
;;;   LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;;   Copyright:

;; Copyright (C) 1995 -- 2022, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;;; TABULATED-LIST ==  tabulated list mode
;; Speech-enable tabulated lists and provide commands for intelligent
;; spoken output 

;;; Code:

;;;   Required modules

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;;  Map Faces:

(voice-setup-add-map 
 '(
   (tabulated-list-fake-header voice-bolden)))

;;;  Interactive Commands:
;;;###autoload
(defun emacspeak-tabulated-list-speak-cell ()
  "Speak current cell.
Optional interactive prefix arg speaks column header as well."
  (interactive )
  (cl-declare (tabulated-list-format))
  (unless (get-text-property (point) 'tabulated-list-column-name)
    (goto-char
     (next-single-property-change (point) 'tabulated-list-column-name)))
  (let* ((name (get-text-property (point) 'tabulated-list-column-name))
         (col (cl-position name tabulated-list-format
                           :test #'string= :key #'car))
         (value (elt (tabulated-list-get-entry)  col)))
    (if (called-interactively-p 'interactive) 
        (dtk-speak (concat name " " value))
      (dtk-speak  value))))

(cl-loop
 for f in 
 '(tabulated-list-next-column tabulated-list-previous-column)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'select-objet)
       (emacspeak-tabulated-list-speak-cell)))))

(defun emacspeak-tabulated-list-next-row ()
  "Move to next row and speak that cell"
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (forward-char col)
    (emacspeak-tabulated-list-speak-cell)))

(defun emacspeak-tabulated-list-previous-row ()
  "Move to previous row and speak that cell."
  (interactive)
  (let ((col (current-column)))
    (forward-line -1)
    (forward-char col)
    (emacspeak-tabulated-list-speak-cell)))

(defun emacspeak-tabulated-list-setup ()
  "Setup Emacspeak"
  (cl-declare (special tabulated-list-mode-map))
  (cl-loop
   for b in
   '(
     ("<left>" tabulated-list-previous-column)
     ("<right>" tabulated-list-next-column)
     ( "." emacspeak-tabulated-list-speak-cell)
     ("<down>"  emacspeak-tabulated-list-next-row)
     ("<up>" emacspeak-tabulated-list-previous-row))
   do
   (emacspeak-keymap-update tabulated-list-mode-map b)))

(emacspeak-tabulated-list-setup)

(provide 'emacspeak-tabulated-list)
;;;  end of file

                                        ; 
                                        ; 
                                        ; 

