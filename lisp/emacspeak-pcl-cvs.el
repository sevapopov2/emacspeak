;;; emacspeak-pcl-cvs.el --- Speech enabled CVS access 
;;; $Id: emacspeak-pcl-cvs.el,v 17.0 2002/11/23 01:29:00 raman Exp $
;;; $Author: raman $
;;; Description:  Emacspeak extension to speech-enable CVS
;;; access 
;;; Keywords: Emacspeak, CVS, Audio Desktop
;;{{{  LCD Archive entry:

;;; LCD Archive Entry:
;;; emacspeak| T. V. Raman |raman@cs.cornell.edu
;;; A speech interface to Emacs |
;;; $Date: 2002/11/23 01:29:00 $ |
;;;  $Revision: 17.0 $ |
;;; Location undetermined
;;;

;;}}}
;;{{{  Copyright:

;;; Copyright (C) 1995 -- 2002, T. V. Raman<raman@cs.cornell.edu>
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

;;{{{ required modules

(eval-when-compile (require 'cl))
(declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-speak)
(require 'emacspeak-sounds)
(require 'voice-lock)

;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; Speech-enabled CVS access via package pcl-cvs.el

;;; Code:

;;}}}
;;{{{  speech enable interactive commands 

(defadvice cvs-mode-kill-buffers (after emacspeak pre act
                                        comp)
  "Produce an auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)
    (message "Killed all temporary CVS buffers.")))

(defadvice cvs-checkout (after emacspeak pre act comp)
  "Provide an auditory icon."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defsubst emacspeak-pcl-cvs-summarize-line ()
  (emacspeak-speak-line))

(defadvice cvs-mode-next-line (after emacspeak pre act comp)
  "Provide auditory feedback. "
  (when (interactive-p)
    (emacspeak-pcl-cvs-summarize-line)
    (emacspeak-auditory-icon 'select-object)))
(defadvice cvs-mode-previous-line (after emacspeak pre act comp)
  "Provide auditory feedback. "
  (when (interactive-p)
    (emacspeak-pcl-cvs-summarize-line)
    (emacspeak-auditory-icon 'select-object)))

(defadvice cvs-mode-mark (after emacspeak  pre act comp)
  "Provide auditory feedback. "
  (when (interactive-p)
    (emacspeak-pcl-cvs-summarize-line)
    (emacspeak-auditory-icon 'mark-object)))

;;}}}
(provide 'emacspeak-pcl-cvs)
;;{{{ end of file

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end:

;;}}}
