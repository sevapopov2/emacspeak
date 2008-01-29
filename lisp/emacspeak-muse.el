;;; emacspeak-muse.el --- Speech-enable Muse
;;; $Id: emacspeak-muse.el 5222 2007-08-26 01:28:19Z tv.raman.tv $
;;; $Author: tv.raman.tv $
;;; Description:  Speech-enable Muse
;;; Keywords: Emacspeak,  Audio Desktop Muse
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
;;;Copyright (C) 1995 -- 2007, T. V. Raman 
;;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
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

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;; Commentary:

;; This module provides speech support for the muse publishing and
;; authoring environment which allows emacs users to create complex
;; documents using simple markup rules. Muse can be downloaded at ;;
;; http://www.newartisans.com/johnw/Emacs/muse.tar.gz. Muse is very
;; similar to emacs-wiki.el. The only difference is that it allows output
;; in several formats including latex, pdf, texinfo and so on.

;; Code:

;;{{{ required modules
(require 'emacspeak-preamble)

;;}}}
;;{{{ Advice interactive commands to speak.

(defadvice muse-mode (after emacspeak pre act comp)
  "Provide auditory feedback."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

(defadvice muse-next-reference (after emacspeak pre act comp)
  "Provide speech feedback."
  (when (interactive-p)
    (if (not (muse-link-at-point))
	(let ((emacspeak-speak-messages t))
	  (emacspeak-auditory-icon 'warn-user)
	  (message "No links on this page"))
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-text-range 'keymap))))

(defadvice muse-previous-reference (after emacspeak pre act comp)
  "Provide speech feedback."
  (when (interactive-p)
    (if (not (muse-link-at-point))
	(let ((emacspeak-speak-messages t))
	  (emacspeak-auditory-icon 'warn-user)
	  (message "No links on this page"))
      (emacspeak-auditory-icon 'large-movement)
      (emacspeak-speak-text-range 'keymap))))

(defadvice muse-follow-name-at-point (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))


(defadvice muse-index (after emacspeak pre act comp)
  "Produce an auditory icon if possible."
  (when (interactive-p)
    (emacspeak-auditory-icon 'open-object)
    (emacspeak-speak-mode-line)))

;;}}}
;;{{{ mapping font faces to personalities 

(voice-setup-add-map
 '(
   (muse-link-face voice-bolden)
   (muse-bad-link-face voice-lighten)
   (muse-header-1-face voice-brighten)
   (muse-header-2-face voice-brighten)
   (muse-header-3-face voice-brighten)
   (muse-header-4-face voice-brighten)
   (muse-header-5-face voice-brighten)
   (muse-header-6-face voice-brighten)
   ))

;;}}}
(provide 'emacspeak-muse)
;;{{{  emacs local variables 

;;; local variables:
;;; folded-file: t
;;; byte-compile-dynamic: t
;;; end: 

;;}}}
