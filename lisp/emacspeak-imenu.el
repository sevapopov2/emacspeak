;;; emacspeak-imenu.el --- Speech enable Imenu -- produce buffer-specific table of contents  -*- lexical-binding: t; -*-
;;; $Id$
;;; $Author: tv.raman.tv $
;;; Description: Auditory interface buffer indices
;;; Keywords: Emacspeak, Speak, Spoken Output, indices
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2007-08-25 18:28:19 -0700 (Sat, 25 Aug 2007) $ |
;;;  $Revision: 4532 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (c) 1995 -- 2018, T. V. Raman
;;; All Rights Reserved.
;;;
;;; This file is not part of GNU Emacs, but the same permissions apply.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;}}}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{  Required modules
(require 'cl-lib)
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)
(require 'imenu)
;;}}}
;;{{{  Introduction
;;; Commentary:
;;; Speech enable imenu and provide other useful navigation commands.
;;; Code:
;;}}}
;;{{{  variables

(defvar emacspeak-imenu-flattened-index-alist nil
  "Cached flattened index alist for buffer navigation")
(make-variable-buffer-local 'emacspeak-imenu-flattened-index-alist)

;;}}}
;;{{{  advise imenu to cache flattened alist

(defun emacspeak-imenu-flatten-index-alist (index-alist &optional concat-names prefix)
  ;; Takes a nested INDEX-ALIST and returns a flat index alist.
  ;; If optional CONCAT-NAMES is non-nil, then a nested index has its
  ;; name and a space concatenated to the names of the children.
  ;; Third argument PREFIX is for internal use only.

  (cl-declare (special imenu-level-separator))
  (cl-mapcan
   (function
    (lambda (item)
      (let* ((name (car item))
             (pos (cdr item))
             (new-prefix (and concat-names
                              (if prefix
                                  (concat prefix imenu-level-separator name)
                                name))))
        (cond
         ((or (markerp pos) (numberp pos)
              (overlayp pos))
          (list (cons new-prefix pos)))
         (t
          (emacspeak-imenu-flatten-index-alist pos
                                               new-prefix))))))
   index-alist))

(defadvice imenu--make-index-alist (after emacspeak pre act comp)
  "Cache flattened index alist"
  (cl-declare (special emacspeak-imenu-flattened-index-alist))
  (setq emacspeak-imenu-flattened-index-alist
        (emacspeak-imenu-flatten-index-alist
         imenu--index-alist t)))

;;}}}
;;{{{ advice
(cl-loop
 for f in
 '(imenu imenu-anywhere ido-imenu-anywhere)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Provide auditory feedback"
     (when (ems-interactive-p)
       (emacspeak-auditory-icon 'large-movement)
       (emacspeak-speak-line)))))

;;}}}
;;{{{  Navigation

(defvar emacspeak-imenu-autospeak nil
  "Speak contents of sections automatically if set.")

;;;###autoload
(defun emacspeak-imenu-goto-next-index-position ()
  "Goto the next index position in current buffer"
  (interactive)
  (cl-declare (special emacspeak-imenu-flattened-index-alist
                       emacspeak-imenu-autospeak
                       imenu--index-alist))
  (let ((pos (point))
        (guess 0)
        (target (point-max)))
    (unless imenu--index-alist (imenu--make-index-alist 'no-error))
    (unless emacspeak-imenu-flattened-index-alist
      (setq emacspeak-imenu-flattened-index-alist
            (emacspeak-imenu-flatten-index-alist
             imenu--index-alist t)))
    (cl-loop for item  in emacspeak-imenu-flattened-index-alist
             do
             (setq guess
                   (cond
                    ((overlayp (cdr item))
                     (overlay-start (cdr item)))
                    ((markerp (cdr item))
                     (marker-position (cdr item)))
                    (t (cdr item))))
             (when (< pos guess)
               (if (< guess target)
                   (setq target guess))))
    (goto-char target)
    (when (ems-interactive-p)
      (emacspeak-auditory-icon 'large-movement)
      (if emacspeak-imenu-autospeak
          (emacspeak-imenu-speak-this-section)
        (emacspeak-speak-line))
      (when (overlays-at (point))
        (goto-char (overlay-end (car (overlays-at (point)))))))))

;;;###autoload
(defun emacspeak-imenu-goto-previous-index-pos ()
  "Goto the previous index pos in current buffer"
  (interactive)
  (cl-declare (special emacspeak-imenu-flattened-index-alist
                       emacspeak-imenu-autospeak
                       imenu--index-alist))
  (let ((pos (point))
        (guess 0)
        (target (point-min)))
    (unless imenu--index-alist (imenu--make-index-alist 'no-error))
    (unless emacspeak-imenu-flattened-index-alist
      (setq emacspeak-imenu-flattened-index-alist
            (emacspeak-imenu-flatten-index-alist
             imenu--index-alist t)))
    (cl-loop for item  in emacspeak-imenu-flattened-index-alist
             do
             (setq guess
                   (cond
                    ((overlayp (cdr item))
                     (overlay-start (cdr item)))
                    ((markerp (cdr item))
                     (marker-position (cdr item)))
                    (t (cdr item))))
             (when (> pos guess)
               (if (> guess target)
                   (setq target guess))))
    (goto-char target)
    (when (ems-interactive-p)
      (emacspeak-auditory-icon 'large-movement)
      (if emacspeak-imenu-autospeak
          (emacspeak-imenu-speak-this-section)
        (emacspeak-speak-line)))))

;;}}}
;;{{{  speaking logical sections

(defun emacspeak-imenu-speak-this-section ()
  "Speak upto start of next index entry"
  (interactive)
  (let ((start (point)))
    (save-excursion
      (emacspeak-imenu-goto-next-index-position)
      (emacspeak-speak-region start (point)))))

;;}}}
;;{{{ bind keys

(defun emacspeak-imenu-setup-keys ()
  "Set up imenu keys"
  (define-key emacspeak-keymap "\M-i" 'imenu)
  (define-key emacspeak-keymap  "\M-p" 'emacspeak-imenu-goto-previous-index-position)
  (define-key emacspeak-keymap "\M-n" 'emacspeak-imenu-goto-next-index-position)
  (define-key emacspeak-keymap "\M- " 'emacspeak-imenu-speak-this-section))
(emacspeak-imenu-setup-keys)

;;}}}
;;{{{ customize settings

(cl-declaim (special imenu-space-replacement
                     imenu-max-items))
(setq imenu-space-replacement "."
      imenu-max-items 200)

;;}}}
(provide 'emacspeak-imenu)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
