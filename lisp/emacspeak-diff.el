;;; emacspeak-diff.el --- Speech enable  diff
;;; Description:   extension to speech enable diff
;;; Keywords: Emacspeak, Audio Desktop
;;{{{  LCD Archive entry:

;;}}}
;;{{{  Copyright:

;;; Initial version: Author: Igor B. Poretsky <poretsky@mlbox.ru>
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

;;{{{  Introduction:

;;; Commentary:
;;; diff is an emacs package for using UNIX diff from emacs.
;;; this module speech-enables diff.

;;}}}

;;; Code:
;;{{{ required modules
(require 'emacspeak-preamble)

;;}}}
;;{{{ Personalities  
(voice-setup-add-map
 '(
   (diff-header voice-bolden)
   (diff-file-header voice-animate)
   (diff-hunk-header voice-animate-medium)
   (diff-added voice-animate-extra)
   (diff-removed voice-animate-extra)
   (diff-changed voice-animate-extra)
   (diff-indicator-added voice-animate-extra)
   (diff-indicator-removed voice-animate-extra)
   (diff-indicator-changed voice-animate-extra)
   (diff-nonexistent voice-monotone)
   ))

;;}}}
;;{{{ advice  interactive commands

(defadvice diff-goto-source (after emacspeak pre act comp)
  "Provide spoken feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-line)))

(loop for f in 
      '(diff-hunk-next diff-hunk-prev)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'select-object)
            (emacspeak-speak-line)))))

(loop for f in 
      '(diff-file-next diff-file-prev)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'large-movement)
            (emacspeak-speak-line)))))

(loop for f in 
      '(diff-hunk-kill diff-file-kill)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'delete-object)
            (emacspeak-speak-line)))))

(loop for f in 
      '(diff-apply-hunk
        diff-refine-hunk
        diff-ignore-whitespace-hunk
        diff-context->unified
        diff-unified->context
        diff-ediff-patch
        diff-reverse-direction
        diff-split-hunk
        diff-test-hunk)
      do
      (eval
       `(defadvice ,f (after emacspeak pre act comp)
          "Provide spoken feedback."
          (when (interactive-p)
            (emacspeak-auditory-icon 'task-done)))))

;;}}}
;;{{{ advise process filter and sentinels

(defadvice diff-sentinel (after emacspeak pre act )
  "Provide auditory feedback."
  (emacspeak-auditory-icon 'task-done)
  (message "process diff finished%s"
           (if (equal 0 (ad-get-arg 0))
               " with no differences"
             "")))

;;}}}
(provide 'emacspeak-diff)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
