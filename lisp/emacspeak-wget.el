;;; emacspeak-wget.el --- speech-enable Wget interface
;;; Description:  Emacspeak extension to speech-enable Wget
;;; Keywords: Emacspeak, Wget, www, ftp, download manager
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

;;{{{ required modules

(require 'emacspeak-preamble)
;;}}}
;;{{{  Introduction:

;;; Commentary:

;;; This module speech-enables emacs-wget.
;;; Emacs-wget provides an interface of GNU wget for Emacs.  You can also
;;; call it from web browser on Emacs, like Emacs/W3 or emacs-w3m.
;;; See <http://pop-club.hp.infoseek.co.jp/emacs/emacs-wget/> for more info.

;;}}}
;;{{{

;;; Code:

(defadvice wget (after emacspeak pre act comp)
  "provide auditory confirmation"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice wget-web-page (after emacspeak pre act comp)
  "provide auditory confirmation"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice wget-info (after emacspeak pre act comp)
  "provide auditory confirmation"
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)))

(defadvice wget-next-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice wget-previous-line (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'select-object)
    (emacspeak-speak-line)))

(defadvice wget-progress-update (before emacspeak pre act comp)
  "provide auditory confirmation"
  (when (numberp (ad-get-arg 1))
    (emacspeak-auditory-icon 'progress)))

(defadvice wget-quit (after emacspeak pre act comp)
  "provide auditory confirmation"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

(defadvice wget-quit-and-exit (after emacspeak pre act comp)
  "provide auditory confirmation"
  (when (interactive-p)
    (emacspeak-auditory-icon 'close-object)))

;;}}}
(provide 'emacspeak-wget)
;;{{{ end of file 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
